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




data S = S { regA, regX, regY, regP, regS :: Word8
           , regPC, regAddr :: Word16
           , memory :: Vector Word8
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



fetch :: St Word8
fetch = (!) <$> gets memory <*> fromIntegral `fmap` (gets regAddr)

fetchAddr :: St ()
fetchAddr = do
    s <- get
    let aLo = regAddr s
        aHi = aLo + 1
        lo = memory s ! fromIntegral aLo
        hi = memory s ! fromIntegral aHi
    put s { regAddr = makeAddr lo hi }

store :: Word8 -> St ()
store v = modify $ \s -> s { memory = memory s // [(fromIntegral $ regAddr s, v)] }


nextPC :: St ()
nextPC = modify $ \s -> let pc = regPC  s in s { regPC = pc + 1, regAddr = pc }

fetchPC :: St Word8
fetchPC = nextPC >> fetch

indexX = modify $ \s -> s { regAddr = regAddr s + fromIntegral (regX s) }
indexY = modify $ \s -> s { regAddr = regAddr s + fromIntegral (regY s) }

addrImm = nextPC
addrZero = fetchPC >>= \v -> modify $ \s -> s { regAddr = zeroPage v }
addrZeroX = fetchPC >>= \v -> modify $ \s -> s { regAddr = zeroPage (v + regX s) }
addrZeroY = fetchPC >>= \v -> modify $ \s -> s { regAddr = zeroPage (v + regY s) }
addrRel = fetchPC >>= \v -> modify $ \s -> s { regAddr = regPC s + signExt v }
addrAbs = do
    lo <- fetchPC
    hi <- fetchPC
    modify $ \s -> s { regAddr = makeAddr lo hi }
addrAbsX = addrAbs >> indexX
addrAbsY = addrAbs >> indexY
addrInd = addrAbs >> fetchAddr
addrIndIdx = addrZeroX >> fetchAddr
addrIdxInd = addrZero >> fetchAddr >> indexY



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


aluIns :: (Word8 -> St ()) -> (Word8 -> Word8 -> Word8) -> St () -> St ()
aluIns set op mode = do
    mode
    v <- fetch
    a <- gets regA
    set $ op a v

insORA = aluIns setAZN (.|.)
insAND = aluIns setAZN (.&.)
insEOR = aluIns setAZN xor
insADC = aluIns setAZCN (+)
insSTA mode = mode >> gets regA >>= store
insLDA = aluIns setAZN (flip const)
insCMP = aluIns setZCN subtract
insSBC = aluIns setAZCN subtract

bitIns :: (Word8 -> Word8) -> St () -> St ()
bitIns op mode = mode >> fetch >>= store . op

insASL = bitIns (flip shiftL 1)   -- TODO: set flags CZN
insROL = bitIns (flip rotateL 1)
insLSR = bitIns (flip shiftR 1)
insROR = bitIns (flip rotateR 1)
insSTX mode = mode >> gets regX >>= store
insLDX mode = mode >> fetch >>= \v -> modify $ \s -> s { regX = v }
insDEC = bitIns (subtract 1)
insINC = bitIns (+ 1)

bitAccIns :: (Word8 -> Word8) -> St ()
bitAccIns op = modify $ \s -> s { regA = op (regA s) }

insASLacc = bitAccIns (flip shiftL 1)   -- TODO: set flags CZN
insROLacc = bitAccIns (flip rotateL 1)
insLSRacc = bitAccIns (flip shiftR 1)
insRORacc = bitAccIns (flip rotateR 1)

jump = modify $ \s -> s { regPC = regAddr s }

insBIT mode = undefined
insJMP mode = mode >> jump
insSTY mode = mode >> gets regY >>= store
insLDY mode = mode >> fetch >>= \v -> modify $ \s -> s { regY = v }
insCPX mode = mode >> fetch >>= \v -> gets regX >>= \x -> setZCN (x - v)
insCPY mode = mode >> fetch >>= \v -> gets regY >>= \y -> setZCN (y - v)

brIns bit t = do
    addrRel
    p <- gets regP
    when (testBit p bit == t) jump

insBPL = brIns bitN False
insBMI = brIns bitN True
insBVC = brIns bitV False
insBVS = brIns bitV True
insBCC = brIns bitC False
insBCS = brIns bitC True
insBNE = brIns bitZ False
insBEQ = brIns bitZ True

stIns op bit = modify $ \s -> s { regP = op (regP s) bit }

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
