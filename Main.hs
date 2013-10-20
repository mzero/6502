module Main
    (main)
  where

import Control.Monad.State (execState)
import qualified Data.ByteString as B
import Numeric (readDec, readHex)
import System.Environment (getArgs)

import CPU
import Dump
import Instructions
import Memory


{- TODO

    setting flags on compare instructions
    setting flags on ADC/SBC instructions
    decimal arith. mode

    input/output

    test suite
-}



{-
    ram=0,16            -- 4k of RAM at 0x0000
    ram=40,64           -- 16k or RAM at 0x4000
    rom=C0,progfile     -- ROM loaded from profile starting at 0xC000
    reset               -- reset the CPU (sets PC from 0xFFFC vector)
    pc=00,10             -- set PC to 0x1000
-}

configure :: String -> S -> IO (Either String S)
configure arg s = case break (== '=') arg of
    ("ram", '=':spec) -> configRam spec
    ("rom", '=':spec) -> configRom spec
    ("reset", "") -> configReset
    ("pc", '=':spec) -> configPC spec
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

    configReset = okay $ execState reset s
    configPC pcStr = case break (== ',') pcStr of
        (loStr, ',':hiStr) -> return $ do
            lo <- readByte loStr
            hi <- readByte hiStr
            return $ s { regPC = makeAddr lo hi }
        _ -> return $ Left "invalid pc spec"

    okay = return . Right
    modMemory f = s { memory = f $ memory s }
    readByte str = case filter (null . snd) $ readHex str of
        ((v,_):_) -> Right v
        _ -> Left $ "can't parse as hex: " ++ str
    readDec' str = case filter (null . snd) $ readDec str of
        ((v,_):_) -> Right v
        _ -> Left $ "can't parse as dec: " ++ str

configureAll :: S -> [String] -> IO (Either String S)
configureAll s [] = return $ Right s
configureAll s (a:as) =
    configure a s >>= either (return . Left) (\s' -> configureAll s' as)


run :: S -> IO ()
run s0 = do
    putStrLn $ dumpReg s0
    let pc0 = regPC s0
        s1 = execState executeOne s0
        pc1 = regPC s1
    if pc0 /= pc1
        then run s1
        else do
            putStrLn $ "Execution snagged at " ++ show pc1
            putStrLn $ dumpReg s1

main :: IO ()
main = getArgs >>= configureAll powerOnState >>= either putStrLn run

