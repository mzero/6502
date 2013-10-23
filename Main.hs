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


run :: Config -> IO ()
run conf = do
    when (confTrace conf) $ do
        hPutStr stderr disasmSpacer
        hPutStrLn stderr $ dumpReg s0
    restore <- if confIO conf then setupIO else return (return ())
    run' s0
    restore
  where
    s0 = confState conf
    run' s = do
        when (confTrace conf) $  hPutStr stderr $ disasm s
        let pc = regPC s
            s' = execState executeOne s
            pc' = regPC s'
        when (confTrace conf) $ hPutStrLn stderr $ dumpReg s'
        s'' <- if confIO conf then handleIO s' else return s'
        if True  -- pc /= pc'
            then run' s''
            else putStrLn $ "Execution snagged at " ++ show pc'

    setupIO = do
        ibuf <- hGetBuffering stdin
        obuf <- hGetBuffering stdout
        iecho <- hGetEcho stdin
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        return $ do
            hSetEcho stdin iecho
            hSetBuffering stdin ibuf
            hSetBuffering stdout obuf
            putStr "\n\n"

    handleIO s = do
        when (addrWrite s == Just outPortAddr) $ do
            let c = fetchByte outPortAddr $ memory s
            when (c /= 0) $ hPutChar stdout $ toEnum $ fromIntegral c
        if (addrRead s == Just inPortAddr)
            then do
                r <- hReady stdin
                -- r <- hWaitForInput stdin 50 -- hReady stdin
                c <- if r then (fromIntegral . fromEnum) <$> hGetChar stdin else return 0
                let c' = if c == 0xA then 0xD else c
                return s { memory = storeByte inPortAddr c' $ memory s }
            else return s

    inPortAddr = makeAddr 0x04 0xF0
    outPortAddr = makeAddr 0x01 0xF0

main :: IO ()
main = getArgs >>= configureAll config0 >>= either putStrLn run

