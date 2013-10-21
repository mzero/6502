module Instructions
    ( executeOne
    , reset, nmi, irq
    )
  where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State (get, gets, modify, put)
import Data.Bits ((.&.), (.|.), clearBit, setBit, shiftL, shiftR, testBit, xor)
import Data.List (transpose)
import qualified Data.Vector as V
import Data.Word (Word8)

import Memory
import CPU


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


decode :: V.Vector (St ())
decode = V.fromList $ concat $ transpose
    [ col0, col1, col2, col3, col4, col5, col6, col7
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
                                     `except` (0xB, insLDX addrZeroY)
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
                                   `except` (0xB, insLDX addrAbsY)
    colF = colErr

    colAlu e o = concatMap (\i -> [i e, i o])
                [insORA, insAND, insEOR, insADC, insSTA, insLDA, insCMP, insSBC]
    colBit e o = concatMap (\i -> [i e, i o])
                [insASL, insROL, insLSR, insROR, insSTX, insLDX, insDEC, insINC]

    colErr = replicate 16 insErr

    except :: [a] -> (Int, a) -> [a]
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

cmpOp a b = (a >= b, a - b)

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
insCMP = aluIns setCZN cmpOp
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
insCPX mode = gets regX >>= \x -> mode >>= fetch >>= setCZN . cmpOp x
insCPY mode = gets regY >>= \y -> mode >>= fetch >>= setCZN . cmpOp y

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


executeOne :: St ()
executeOne = clearBus >> fetchPC >>= (decode V.!) . fromIntegral
