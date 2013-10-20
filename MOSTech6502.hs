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

import Memory

zeroPage :: Word8 -> Addr
zeroPage v = makeAddr v 0


data S = S { regA, regX, regY, regP, regS :: !Word8
           , regPC :: !Addr
           , memory :: !Memory
           , addrRead, addrWrite :: Maybe Addr
           }
type St = State S

[bitN, bitV, _, bitB, bitD, bitI, bitZ, bitC] = [7,6..0]

assignBit bit bool byte = (if bool then setBit else clearBit) byte bit

assignZ v = assignBit bitZ (v == 0)
assignZN v = assignBit bitZ (v == 0) . assignBit bitN (testBit v 7)
assign67 v = assignBit bitV (testBit v 6) . assignBit bitN (testBit v 7)

setZN v = modify $ \s -> s { regP = assignZN v $ regP s }
setAZN v = modify $ \s -> s { regA = v, regP = assignZN v $ regP s }
setXZN v = modify $ \s -> s { regX = v, regP = assignZN v $ regP s }
setYZN v = modify $ \s -> s { regY = v, regP = assignZN v $ regP s }

setZVNbit (a,v) = modify $ \s -> s { regP = assignZ (a .&. v) $ assign67 v $ regP s }

setACZN = undefined
setCZN = undefined

{- TODO

    setting flags on compare instructions
    setting flags on ADC/SBC instructions
    decimal arith. mode

    input/output

    test suite
-}



fetch :: Addr -> St Word8
fetch addr = do
    mem <- gets memory
    modify $ \s -> s { addrRead = Just addr }
    return $ fetchByte addr mem

fetchIndirectAddr :: Addr -> St Addr
fetchIndirectAddr addr0 = do
    mem <- gets memory
    let addr1 = nextAddrWrap addr0  -- correct! no page crossing allowed
        bLo = fetchByte addr0 mem
        bHi = fetchByte addr1 mem
    return $ makeAddr bLo bHi

store :: Addr -> Word8 -> St ()
store addr v = modify $
    \s -> s { memory = storeByte addr v $ memory s, addrWrite = Just addr }

clearBus :: St()
clearBus = modify $ \s -> s { addrRead = Nothing, addrWrite = Nothing }


nextPC :: St Addr
nextPC = do
    s <- get
    let pc = regPC s
    put s { regPC = nextAddrFull pc }
    return pc

fetchPC :: St Word8
fetchPC = nextPC >>= fetch

push :: Word8 -> St ()
push v = do
    s <- get
    let sp = regS s
    store (makeAddr sp 1) v
    put s { regS = sp - 1 }

pull :: St Word8
pull = do
    s <- get
    let sp = regS s + 1
    put s { regS = sp }
    fetch $ makeAddr sp 1

pushAddr :: Addr -> St ()
pushAddr addr = let (lo, hi) = splitAddr addr in push hi >> push lo

pullAddr :: St Addr
pullAddr = makeAddr <$> pull <*> pull


indexX addr = gets regX >>= return . indexAddr addr
indexY addr = gets regY >>= return . indexAddr addr

addrImm = nextPC
addrZero = zeroPage <$> fetchPC
addrZeroX = zeroPage <$> ( (+) <$> fetchPC <*> gets regX )
addrZeroY = zeroPage <$> ( (+) <$> fetchPC <*> gets regY )
addrRel = relativeAddr <$> gets regPC <*> fetchPC
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

aluIns :: (v -> St ()) -> (Word8 -> Word8 -> v) -> St Addr -> St ()
aluIns set op mode = do
    v <- mode >>= fetch
    a <- gets regA
    set $ op a v

modIns :: (Word8 -> St Word8) -> St Addr -> St ()
modIns op mode = mode >>= \addr -> fetch addr >>= op >>= store addr

modAccIns :: (Word8 -> St Word8) -> St ()
modAccIns op = gets regA >>= op >>= \v -> modify $ \s -> s { regA = v }

stIns op bit = modify $ \s -> s { regP = op (regP s) bit }

jump addr = modify $ \s -> s { regPC = addr }

brIns bit t = do
    addr <- addrRel
    p <- gets regP
    when (testBit p bit == t) $ jump addr

shiftOp shifter isRot inBit outBit v = do
    s <- get
    let newC = testBit v outBit
        bitIn = assignBit inBit $ isRot && testBit (regP s) bitC
        v' = bitIn $ shifter v 1
    put s { regP = assignBit bitC newC $ assignZN v' $ regP s }
    return v'

aslOp = shiftOp shiftL False 0 7
rolOp = shiftOp shiftL True  0 7
lsrOp = shiftOp shiftR False 7 0
rorOp = shiftOp shiftR True  7 0

vector addr = fetchIndirectAddr addr >>= jump

interrupt isBrk pcOffset addr = do
    gets regPC >>= pushAddr . flip indexAddr pcOffset
    gets regP >>= push . flip (if isBrk then setBit else clearBit) bitB
    insSEI
    vector addr

reset = vector (makeAddr 0xFC 0xFF) -- 0xFFFC
nmi = interrupt False 0 (makeAddr 0xFA 0xFF) -- 0xFFFA
irq = interrupt False 0 (makeAddr 0xFE 0xFF) -- 0xFFFE


insORA = aluIns setAZN (.|.)
insAND = aluIns setAZN (.&.)
insEOR = aluIns setAZN xor
insADC = aluIns setACZN (+)
insSTA = storeIns $ gets regA
insLDA = loadIns setAZN
insCMP = aluIns setCZN subtract
insSBC = aluIns setACZN subtract

insASL = modIns aslOp
insROL = modIns rolOp
insLSR = modIns lsrOp
insROR = modIns rorOp
insSTX = storeIns $ gets regX
insLDX = loadIns setXZN
insDEC = modIns $ \v -> let v' = v - 1 in setZN v' >> return v'
insINC = modIns $ \v -> let v' = v + 1 in setZN v' >> return v'

insASLacc = modAccIns aslOp
insROLacc = modAccIns rolOp
insLSRacc = modAccIns lsrOp
insRORacc = modAccIns rorOp

insBIT      = aluIns setZVNbit (,)
insJMP mode = mode >>= jump
insSTY      = storeIns $ gets regY
insLDY      = loadIns setYZN
insCPX mode = mode >>= fetch >>= \v -> gets regX >>= \x -> setCZN (x - v)
insCPY mode = mode >>= fetch >>= \v -> gets regY >>= \y -> setCZN (y - v)

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

insBRK = interrupt True 1 (makeAddr 0xFE 0xFF)
insJSR = addrAbs >>= \addr -> gets regPC >>= pushAddr . prevAddrFull >> jump addr
insRTI = insPLP >> pullAddr >>= jump
insRTS = pullAddr >>= jump . nextAddrFull

insPHP = gets regP >>= push
insPLP = pull >>= \v -> modify $ \s -> s { regP = v }
insPHA = gets regA >>= push
insPLA = pull >>= setAZN

insNOP = return ()

insINX = gets regX >>= setXZN . (+ 1)
insDEX = gets regX >>= setXZN . (subtract 1)
insINY = gets regY >>= setYZN . (+ 1)
insDEY = gets regY >>= setYZN . (subtract 1)

insTAX = gets regA >>= setXZN
insTXA = gets regX >>= setAZN
insTAY = gets regA >>= setYZN
insTYA = gets regY >>= setAZN
insTXS = modify $ \s -> s { regS = regX s }
insTSX = gets regS >>= setXZN

insErr = undefined
