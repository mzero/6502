module MOSTech6502
    ( S
    , St
    , decode
    )
  where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State (State, get, gets, modify, put)
import Data.Bits ((.&.), (.|.), clearBit, rotateL, rotateR, setBit, shiftL, shiftR, testBit, xor)
import Data.Int (Int8, Int16)
import Data.List (transpose)
import Data.Vector (Vector, (!), (//))
import Data.Word (Word8, Word16)

type Addr = Word16

makeAddr :: Word8 -> Word8 -> Addr
makeAddr lo hi = fromIntegral hi `shiftL` 16 .|. fromIntegral lo

zeroPage :: Word8 -> Addr
zeroPage v = fromIntegral v

signExt :: Word8 -> Word16
signExt v = fromIntegral (fromIntegral (fromIntegral v :: Int8) :: Int16)




data S = S { regA, regX, regY, regP, regS :: !Word8
           , regPC :: !Word16
           , memory :: Vector Word8
           , addrRead, addrWrite :: Maybe Addr
           }
type St = State S


bitC = 0
bitZ = 1
bitI = 2
bitD = 3
bitB = 4
bitV = 6
bitN = 7

setAZN = undefined
setAZCN = undefined
setZCN = undefined
setXZN = undefined
setYZN = undefined

{- TODO

    setting A/X/Y and flags
    setting flags on various instructions

    push/pop instructions

    decimal arith. mode

    input/output

    test suite

-}



fetch :: Addr -> St Word8
fetch addr = do
    mem <- gets memory
    modify $ \s -> s { addrRead = Just addr }
    return $ mem ! fromIntegral addr

fetchIndirectAddr :: Addr -> St Addr
fetchIndirectAddr addr = do
    mem <- gets memory
    let lo = mem ! fromIntegral addr
        hi = mem ! fromIntegral (addr + 1)
    return $ makeAddr lo hi

store :: Addr -> Word8 -> St ()
store addr v = modify $
    \s -> s { memory = memory s // upd, addrWrite = Just addr }
  where
    upd = [(fromIntegral addr, v)]

clearBus :: St()
clearBus = modify $ \s -> s { addrRead = Nothing, addrWrite = Nothing }


nextPC :: St Addr
nextPC = do
    s <- get
    let pc = regPC s
    modify $ \s -> s { regPC = pc + 1 }
    return pc

fetchPC :: St Word8
fetchPC = nextPC >>= fetch

indexX addr = gets regX >>= return . (addr +) . fromIntegral
indexY addr = gets regY >>= return . (addr +) . fromIntegral

addrImm = nextPC
addrZero = zeroPage <$> fetchPC
addrZeroX = zeroPage <$> ( (+) <$> fetchPC <*> gets regX )
addrZeroY = zeroPage <$> ( (+) <$> fetchPC <*> gets regY )
addrRel = (+) <$>  gets regPC <*> (signExt <$> fetchPC)
addrAbs = makeAddr <$> fetchPC <*> fetchPC
addrAbsX = addrAbs >>= indexX
addrAbsY = addrAbs >>= indexY
addrInd = addrAbs >>= fetchIndirectAddr
addrIndIdx = addrZeroX >>= fetchIndirectAddr
addrIdxInd = addrZero >>= fetchIndirectAddr >>= indexY


decode :: [St ()]
decode = concat $ transpose [ col0, col1, col2, col3, col4, col5, col6, col7
                            , col8, col9, colA, colB, colC, colD, colE, colF ]
  where
    col0 = [ insBRK,         insBPL, insJSR,         insBMI
           , insRTI,         insBVC, insRTS,         insBVS
           , insErr,         insBCC, insLDY addrImm, insBCS
           , insCPY addrImm, insBNE, insCPX addrImm, insBEQ
           ]
    col1 = colAlu addrIdxInd addrIndIdx
    col2 = colErr `except` (0xA, insLDX addrImm)
    col3 = colErr

    col4 = [ insErr,          insErr,           insBIT addrZero, insErr
           , insErr,          insErr,           insErr,          insErr
           , insSTY addrZero, insSTY addrZeroX, insLDY addrZero, insLDY addrZeroX
           , insCPY addrZero, insErr,           insCPX addrZero, insErr
           ]
    col5 = colAlu addrZero addrZeroX
    col6 = colBit addrZero addrZeroX `except` (0x9, insSTX addrZeroY)
    col7 = colErr

    col8 = [ insPHP, insCLC, insPLP, insSEC, insPHA, insCLI, insPLA, insSEI
           , insDEY, insTYA, insTAY, insCLV, insINY, insCLD, insINX, insSED ]
    col9 = colAlu addrImm addrAbsY `except` (0x8, insErr)
    colA = [ insASLacc, insErr, insROLacc, insErr, insLSRacc, insErr, insRORacc, insErr
           , insTXA, insTXS, insTAX, insTSX, insDEX, insErr, insNOP, insErr ]
    colB = colErr

    colC = [ insErr,         insErr, insBIT addrAbs, insErr
           , insJMP addrAbs, insErr, insJMP addrInd, insErr
           , insSTY addrAbs, insErr, insLDY addrAbs, insLDY addrAbsX
           , insCPY addrAbs, insErr, insCPX addrAbs, insErr
           ]
    colD = colAlu addrAbs addrAbsX
    colE = colBit addrAbs addrAbsX `except` (0x9, insErr)
    colF = colErr

    colAlu e o = concatMap (\i -> [i e, i o])
                [insORA, insAND, insEOR, insADC, insSTA, insLDA, insCMP, insSBC]
    colBit e o = concatMap (\i -> [i e, i o])
                [insASL, insROL, insLSR, insROR, insSTX, insLDX, insDEC, insINC]

    colErr = replicate 16 insErr
    [] `except` _ = []
    (i:is) `except` (n, j) | n == 0    = j : is
                           | otherwise = i : is `except` (n-1, j)



loadIns :: (Word8 -> St ()) -> St Addr -> St ()
loadIns loader mode = mode >>= fetch >>= loader

storeIns :: St Word8 -> St Addr -> St ()
storeIns fetcher mode = mode >>= \addr -> fetcher >>= store addr

aluIns :: (Word8 -> St ()) -> (Word8 -> Word8 -> Word8) -> St Addr -> St ()
aluIns set op mode = do
    v <- mode >>= fetch
    a <- gets regA
    set $ op a v

bitIns :: (Word8 -> Word8) -> St Addr -> St ()
bitIns op mode = mode >>= \addr -> fetch addr >>= store addr . op

bitAccIns :: (Word8 -> Word8) -> St ()
bitAccIns op = modify $ \s -> s { regA = op (regA s) }

stIns op bit = modify $ \s -> s { regP = op (regP s) bit }

jump addr = modify $ \s -> s { regPC = addr }

brIns bit t = do
    addr <- addrRel
    p <- gets regP
    when (testBit p bit == t) $ jump addr




insORA = aluIns setAZN (.|.)
insAND = aluIns setAZN (.&.)
insEOR = aluIns setAZN xor
insADC = aluIns setAZCN (+)
insSTA = storeIns $ gets regA
insLDA = aluIns setAZN (flip const)
insCMP = aluIns setZCN subtract
insSBC = aluIns setAZCN subtract

insASL = bitIns (flip shiftL 1)   -- TODO: set flags CZN
insROL = bitIns (flip rotateL 1)
insLSR = bitIns (flip shiftR 1)
insROR = bitIns (flip rotateR 1)
insSTX = storeIns $ gets regX
insLDX = loadIns $ \v -> modify $ \s -> s { regX = v }
insDEC = bitIns (subtract 1)
insINC = bitIns (+ 1)

insASLacc = bitAccIns (flip shiftL 1)   -- TODO: set flags CZN
insROLacc = bitAccIns (flip rotateL 1)
insLSRacc = bitAccIns (flip shiftR 1)
insRORacc = bitAccIns (flip rotateR 1)

insBIT mode = undefined
insJMP mode = mode >>= jump
insSTY      = storeIns $ gets regY
insLDY      = loadIns $ \v -> modify $ \s -> s { regY = v }
insCPX mode = mode >>= fetch >>= \v -> gets regX >>= \x -> setZCN (x - v)
insCPY mode = mode >>= fetch >>= \v -> gets regY >>= \y -> setZCN (y - v)

insBPL = brIns bitN False
insBMI = brIns bitN True
insBVC = brIns bitV False
insBVS = brIns bitV True
insBCC = brIns bitC False
insBCS = brIns bitC True
insBNE = brIns bitZ False
insBEQ = brIns bitZ True

insCLC = stIns clearBit bitC
insSEC = stIns setBit   bitC
insCLI = stIns clearBit bitI
insSEI = stIns setBit   bitI
insCLV = stIns clearBit bitV
insCLD = stIns clearBit bitD
insSED = stIns setBit   bitD

insBRK = undefined
insJSR = undefined
insRTI = undefined
insRTS = undefined

insPHP = undefined
insPLP = undefined
insPHA = undefined
insPLA = undefined

insNOP = return ()

insINX = gets regX >>= \x -> setXZN $ x + 1
insDEX = gets regX >>= \x -> setXZN $ x - 1
insINY = gets regY >>= \y -> setYZN $ y + 1
insDEY = gets regY >>= \y -> setYZN $ y - 1

insTAX = modify $ \s -> s { regX = regA s }
insTXA = modify $ \s -> s { regA = regX s }
insTAY = modify $ \s -> s { regY = regA s }
insTYA = modify $ \s -> s { regA = regY s }
insTXS = modify $ \s -> s { regS = regX s }
insTSX = modify $ \s -> s { regX = regS s }

insErr = undefined
