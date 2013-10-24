module Main
    (main)
  where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (execState)
import qualified Data.ByteString as B
import Numeric (readDec, readHex)
import System.Environment (getArgs)
import System.IO

import CPU
import Dump
import Instructions
import Memory


{- TODO
    decimal arith. mode

    configuration code is ugly
        giant ugly mixed IO/Either e code
        confTrace vs. configTrace, etc.
    configuration of execution options (io/trace) done by bools
        rather than by composition

    IO code is ugly
        input requires state mangagement
        don't seem to have terminal in raw mode
            hence have to translate lf back to cr

    IO spinning in SW is host CPU intensive (and annoying), but because of
        polling for CTRL-C, even a modest delay (50ms), while solving the
        CPU usage on input spin loops, notably slows down the basic interpreter
        - need a way to detect very tight spin loops vs. longer ones.

    snag detection turned off due to crazy rts reuse
        either count snags, and stop at 100
        or include sp in snag calculation

    factor out execution so there can be a simpler config for the golf'd version
-}


data Config = Config { confState :: S, confTrace :: Bool, confIO :: Bool }

config0 :: Config
config0 = Config { confState = powerOnState, confTrace = False, confIO = True }



{-
    ram=0,16            -- 4k of RAM at 0x0000
    ram=40,64           -- 16k or RAM at 0x4000
    rom=C0,progfile     -- ROM loaded from profile starting at 0xC000
    reset               -- reset the CPU (sets PC from 0xFFFC vector)
    pc=00,10             -- set PC to 0x1000
-}

configure :: String -> Config -> IO (Either String Config)
configure arg conf = case break (== '=') arg of
    ("ram", '=':spec) -> configRam spec
    ("rom", '=':spec) -> configRom spec
    ("reset", "") -> configReset
    ("pc", '=':spec) -> configPC spec
    ("io", "") -> configIO
    ("trace", "") -> configTrace
    _ -> return $ Left $ "Unknown arg: " ++ arg
  where
    configRam spec = case break (== ',') spec of
        (startStr, ',':cntStr) -> return $ do
            start <- readByte startStr
            count <- readDec' cntStr
            return $ modMemory $ loadRAM start count
        _ -> return $ Left "invalid ram spec"

    configRom spec = case break (== ',') spec of
        (startStr, ',':filePath) -> do
            content <- B.readFile filePath
            return $ readByte startStr >>= \start -> return $ modMemory $ loadROM start content
        _ -> return $ Left "invalid rom spec"

    configReset = okay $ useS $ execState reset sIn
    configPC pcStr = case break (== ',') pcStr of
        (loStr, ',':hiStr) -> return $ do
            lo <- readByte loStr
            hi <- readByte hiStr
            return $ useS $ sIn { regPC = makeAddr lo hi }
        _ -> return $ Left "invalid pc spec"

    okay = return . Right
    modMemory f = useS sIn { memory = f $ memory sIn }
    readByte str = case filter (null . snd) $ readHex str of
        ((v,_):_) -> Right v
        _ -> Left $ "can't parse as hex: " ++ str
    readDec' str = case filter (null . snd) $ readDec str of
        ((v,_):_) -> Right v
        _ -> Left $ "can't parse as dec: " ++ str

    sIn = confState conf
    useS s = conf { confState = s }

    configTrace = okay $ conf { confTrace = True }
    configIO = okay $ (modMemory (loadRAM 0xF0 1)) { confIO = True }

configureAll :: Config -> [String] -> IO (Either String Config)
configureAll c [] = return $ Right c
configureAll c (a:as) =
    configure a c >>= either (return . Left) (\c' -> configureAll c' as)

type Start a = S -> IO (a,S)
type Step a = a -> S -> IO (a,S)
type Finish a = a -> S -> IO S

type Exec a = (Start a, Step a, Finish a)
type Wrap a b = Exec b -> Exec (a, b)

wrap :: Start a -> Step a -> Step a -> Finish a -> Exec b -> Exec (a,b)
wrap startA preA postA finishA (startB, stepB, finishB) = (startAB, stepAB, finishAB)
  where
    startAB s = do
        (a1,s1) <- startA s
        (b2,s2) <- startB s1
        return ((a1,b2),s2)
    stepAB (a,b) s = do
        (a1,s1) <- preA a s
        (b2,s2) <- stepB b s1
        (a3,s3) <- postA a1 s2
        return ((a3,b2),s3)
    finishAB (a,b) s = finishB b s >>= finishA a

execBase :: Exec ()
execBase = (start, step, finish)
  where
    start s = return ((),s)
    step () s = return ((), execState executeOne s)
    finish () s = return s

wrapTrace :: Wrap () b
wrapTrace = wrap start pre post finish
  where
    start s = hPutStr stderr disasmSpacer >> hPutStrLn stderr (dumpReg s) >> r s
    pre () s = hPutStr stderr (disasm s) >> r s
    post () s = hPutStrLn stderr (dumpReg s) >> r s
    finish () s = return s
    r s = return ((), s)

wrapIO :: Wrap (Int, IO ()) b
wrapIO = wrap start pre post finish
  where
    start s = do
        ibuf <- hGetBuffering stdin
        obuf <- hGetBuffering stdout
        iecho <- hGetEcho stdin
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        let cleanUp = do
            hSetEcho stdin iecho
            hSetBuffering stdin ibuf
            hSetBuffering stdout obuf
            putStr "\n\n"
        return ((0, cleanUp), s)
    pre (n,cleanUp) s = return ((n+1,cleanUp),s)
    post a@(n,cleanUp) s = do
        when (addrWrite s == Just outPortAddr) $ do
            let c = fetchByte outPortAddr $ memory s
            when (c /= 0) $ hPutChar stdout $ toEnum $ fromIntegral c
        if (addrRead s == Just inPortAddr)
            then do
                r <- if n < 16
                        then hWaitForInput stdin 50
                        else hReady stdin
                c <- if r then (fromIntegral . fromEnum) <$> hGetChar stdin else return 0
                let c' = if c == 0xA then 0xD else c
                let s' = s { memory = storeByte inPortAddr c' $ memory s }
                return ((0,cleanUp),s')
            else return (a,s)
    finish (_,cleanUp) s = cleanUp >> return s

    inPortAddr = makeAddr 0x04 0xF0
    outPortAddr = makeAddr 0x01 0xF0

exec :: Exec a -> S -> IO ()
exec (start, step, stop) s0 = start s0 >>= loop >>= uncurry stop >> return ()
  where
    loop (a,s) = do
        let pcsp = (regPC s, regS s)
        as'@(_,s') <- step a s
        let pcsp' = (regPC s', regS s')
        if pcsp /= pcsp'
            then loop as'
            else do
                putStrLn $ "Execution snagged at " ++ show (fst pcsp')
                return as'


main :: IO ()
main = getArgs >>= configureAll config0 >>= either putStrLn run
  where
    run c = ($ confState c) $ case (confTrace c, confIO c) of
        (False, False) -> exec execBase
        (False, True) -> exec $ wrapIO execBase
        (True, False) -> exec $ wrapTrace execBase
        (True, True) -> exec $ wrapIO $ wrapTrace execBase


