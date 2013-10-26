-- Z6502: a 6502 emulator
-- by Mark Lentczner

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import System.Environment
import System.IO

{- === CPU: 200 lines, 6502 characters === -}
type Addr = Word16
toAd = fromIntegral :: Int -> Addr
addr :: Word8 -> Word8 -> Addr
addr lo hi = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
lohi ad = (fromIntegral ad, fromIntegral $ ad `shiftR` 8)
zeroPage v = addr v 0
index ad idx = ad + fromIntegral (idx :: Word8)
relativeAddr ad off = index ad off - if off > 0x7f then 256 else 0

data Page = Missing | ROM !B.ByteString | RAM !(VU.Vector Word8)
type Memory = V.Vector Page
emptyMemory = V.replicate 256 Missing

fetchByte ad mv = case mv V.! hi of
    ROM bs -> B.index bs lo
    RAM vs -> vs VU.! lo
    _ -> 0
  where (hi,lo) = fromIntegral ad `divMod` 256
storeByte ad v mv = case mv V.! hi of
    RAM vs -> mv V.// [(hi, RAM $ vs VU.// [(lo, v)])]
    _ -> mv
  where (hi,lo) = fromIntegral ad `divMod` 256

data S = S { rA, rX, rY, rP, rS :: !Word8, rPC :: !Addr
           , mem :: !Memory, busR,busW :: Maybe Addr }
powerOnState = S 0 0 0 0 0 0 emptyMemory Nothing Nothing

[bitN, bitV, bitX, bitB, bitD, bitI, bitZ, bitC] = [7,6..0]
toBit b t v = (if t then setBit else clearBit) v b
toZ v = toBit bitZ (v == 0)
toZN v = toBit bitZ (v == 0) . toBit bitN (testBit v 7)
to67 v = toBit bitV (testBit v 6) . toBit bitN (testBit v 7)

setZN v = modify $ \s -> s { rP = toZN v $ rP s }
setAZN v = modify $ \s -> s { rA = v, rP=toZN v $ rP s }
setXZN v = modify $ \s -> s { rX = v, rP=toZN v $ rP s }
setYZN v = modify $ \s -> s { rY = v, rP=toZN v $ rP s }
setZVNbit (a,v) = modify $ \s -> s { rP = toZ (a .&. v) $ to67 v $ rP s }
setACZVN (c,v,a) = modify $ \s ->
    s { rA = a, rP = toBit bitC c $ toBit bitV v $ toZN a $ rP s }
setCZN (c,v) = modify $ \s -> s { rP = toBit bitC c $ toZN v $ rP s }

fetch a = state $ \s -> (fetchByte a $ mem s, s { busR = Just a })
fetchIndirectAddr a0 = do
    m <- gets mem
    let (lo,hi) = lohi a0
        a1 = addr (lo+1) hi
        bLo = fetchByte a0 m
        bHi = fetchByte a1 m
    return $ addr bLo bHi
store a v = modify $ \s -> s { mem = storeByte a v $ mem s, busW = Just a }

clearBus = modify $ \s -> s { busR = Nothing, busW = Nothing }
nextPC = state $ \s -> (rPC s, s { rPC = rPC s + 1 })
fetchPC = nextPC >>= \a -> gets mem >>= return . fetchByte a

adjSP n m = state $ \s -> (addr (rS s + m) 1, s { rS = rS s + n })
push v = adjSP (-1) 0 >>= flip store v
pull = adjSP 1 1 >>= fetch
pushAddr a = let (lo, hi) = lohi a in push hi >> push lo
pullAddr = addr <$> pull <*> pull
pushP fromSW = gets rP >>= push . toBit bitX True . toBit bitB fromSW
pullP = pull >>= \v -> modify $ \s -> s { rP = v .&. 0xCF }

indexX a = gets rX >>= return . index a
indexY a = gets rY >>= return . index a
aImm=nextPC
aZero=zeroPage<$>fetchPC
aZeroX=zeroPage<$>((+)<$>fetchPC<*>gets rX)
aZeroY=zeroPage<$>((+)<$>fetchPC<*>gets rY)
aRel=flip relativeAddr<$>fetchPC<*>gets rPC
aAbs=addr<$>fetchPC<*>fetchPC
aAbsX=aAbs>>=indexX
aAbsY=aAbs>>=indexY
aInd=aAbs>>=fetchIndirectAddr
aIndIdx=aZeroX>>=fetchIndirectAddr
aIdxInd=aZero>>=fetchIndirectAddr>>=indexY

decode = V.fromList $ concat $ transpose
 [[iBRK,iBPL,iJSR&aAbs,iBMI,iRTI,iBVC,iRTS,iBVS
  ,iErr,iBCC,iLDY&aImm,iBCS,iCPY&aImm,iBNE,iCPX&aImm,iBEQ]
 ,cAlu aIndIdx aIdxInd
 ,cErr//(10,iLDX&aImm)
 ,cErr
 ,[iErr,iErr,iBIT&aZero,iErr,iErr,iErr,iErr,iErr
  ,iSTY&aZero,iSTY&aZeroX,iLDY&aZero,iLDY&aZeroX,iCPY&aZero,iErr,iCPX&aZero,iErr]
 ,cAlu aZero aZeroX
 ,cBit aZero aZeroX//(9,iSTX&aZeroY)//(11,iLDX&aZeroY)
 ,cErr
 ,[iPHP,iCLC,iPLP,iSEC,iPHA,iCLI,iPLA,iSEI,iDEY,iTYA,iTAY,iCLV,iINY,iCLD,iINX,iSED]
 ,cAlu aImm aAbsY//(8,iErr)
 ,[iASLa,iErr,iROLa,iErr,iLSRa,iErr,iRORa,iErr
  ,iTXA,iTXS,iTAX,iTSX,iDEX,iErr,iNOP,iErr ]
 ,cErr
 ,[iErr,iErr,iBIT&aAbs,iErr,iJMP&aAbs,iErr,iJMP&aInd,iErr
  ,iSTY&aAbs,iErr,iLDY&aAbs,iLDY&aAbsX,iCPY&aAbs,iErr,iCPX&aAbs,iErr]
 ,cAlu aAbs aAbsX
 ,cBit aAbs aAbsX//(9,iErr)//(11,iLDX&aAbsY)
 ,cErr
 ]
cAlt is e o = is >>= (\i->[i&e,i&o])
cAlu = cAlt [iORA,iAND,iEOR,iADC,iSTA,iLDA,iCMP,iSBC]
cBit = cAlt [iASL,iROL,iLSR,iROR,iSTX,iLDX,iDEC,iINC]
cErr = replicate 16 iErr
is//(n,j) = let (f,_:h) = splitAt n is in f++j:h
i&a=a>>=i

loadIns l a = fetch a >>= l
storeIns f a = f >>= store a

aluIns set op ad = do
    v <- fetch ad
    a <- gets rA
    set $ op a v

modIns op a = fetch a >>= op >>= store a
modAccIns op = gets rA >>= op >>= \v -> modify $ \s -> s { rA = v }

stIns b op = modify $ \s -> s { rP = op (rP s) b }

jump a = modify $ \s -> s { rPC = a }
brIns b t = do
    a <- aRel
    p <- gets rP
    when (testBit p b == t) $ jump a

adcOp a b cIn = (cOut, v, s)
  where
    h = b + (if cIn then 1 else 0)
    s = a + h
    cOut = h < b || s < a
    v = testBit (a `xor` s .&. b `xor` s) 7
sbcOp a b cIn = adcOp a (complement b) cIn
carryOp f = gets rP >>= setACZVN . f . flip testBit bitC

cmpOp a b = (a >= b, a - b)

shiftOp shifter isRot inBit outBit v = do
    s <- get
    let newC = testBit v outBit
        bitIn = toBit inBit $ isRot && testBit (rP s) bitC
        v' = bitIn $ shifter v 1
    put s { rP = toBit bitC newC $ toZN v' $ rP s }
    return v'

vector a = fetchIndirectAddr a >>= jump

interrupt isBrk pcOffset a = do
    gets rPC >>= pushAddr . flip index pcOffset
    pushP isBrk
    iSEI
    vector a

reset = vector $ toAd 0xFFFC
nmi = interrupt False 0 $ toAd 0xFFFA
irq = interrupt False 0 $ toAd 0xFFFE

[iORA,iAND,iEOR]=aluIns setAZN<$>[(.|.),(.&.),xor]
[iADC,iSBC]=aluIns carryOp<$>[adcOp,sbcOp]
iSTA=storeIns$gets rA
iLDA=loadIns setAZN
iCMP=aluIns setCZN cmpOp

[iSTX,iSTY]=storeIns.gets<$>[rX,rY]
[iLDX,iLDY]=loadIns<$>[setXZN,setYZN]
[iCPX,iCPY]=(\r a->gets r>>= \v->fetch a>>=setCZN.cmpOp v)<$>[rX,rY]
[iDEC,iINC]=modIns.(\i v->setZN(v+i)>>return(v+i))<$>[-1,1]
[iDEX,iINX]=(gets rX>>=).(setXZN.).(+)<$>[-1,1]
[iDEY,iINY]=(gets rY>>=).(setYZN.).(+)<$>[-1,1]

shOps=[shiftOp d r b(7-b)|(d,b)<-[(shiftL,0),(shiftR,7)],r<-[False,True]]
[iASL,iROL,iLSR,iROR]=modIns<$>shOps
[iASLa,iROLa,iLSRa,iRORa]=modAccIns<$>shOps

iBIT=aluIns setZVNbit(,)
iJMP=jump

[iBPL,iBMI,iBVC,iBVS,iBCC,iBCS,iBNE,iBEQ]=brIns<$>[bitN,bitV,bitC,bitZ]<*>[False,True]
[iCLC,iSEC,iCLI,iSEI,iCLV,_,iCLD,iSED]=stIns<$>[bitC,bitI,bitV,bitD]<*>[clearBit,setBit]

iBRK=interrupt True 1 $ toAd 0xFFFE
iJSR a=gets rPC>>=pushAddr.(-1+)>>jump a
iRTI=iPLP>>pullAddr>>=jump
iRTS=pullAddr>>=jump.(1+)

iPHP=pushP True
iPLP=pullP
iPHA=gets rA>>=push
iPLA=pull>>=setAZN

iNOP=return ()

[iTAX,iTAY]=(gets rA>>=)<$>[setXZN,setYZN]
[iTXA,iTYA]=(>>=setAZN).gets<$>[rX,rY]
iTXS=modify $ \s -> s { rS=rX s }
iTSX=gets rS>>=setXZN

iErr=gets rPC>>=jump.(-1+)

executeOne = clearBus >> fetchPC >>= (decode V.!) . fromIntegral
{- === END OF CPU === -}


{- === MOTHERBOARD === -}
buildMemory rom =
    loadRAM 0xF0 1 $ loadRAM 0x00 ramSize $ loadROM romStart rom $ emptyMemory
  where
    ramSize = 256 - (B.length rom `div` 256)
    romStart = fromIntegral ramSize

    loadRAM p0 n = (V.// zip [p0..] (map RAM $ replicate n ramPage))
    ramPage = VU.replicate 256 0

    loadROM p0 bs = (V.// zip [p0..] (map ROM $ romPages bs))
    romPages b = case B.length b of
        l | l == 0    -> []
          | l < 256   -> [b `B.append` B.replicate (256 - l) 0]
          | l == 256  -> [b]
          | otherwise -> let (b0,bn) = B.splitAt 256 b in b0 : romPages bn

main = getArgs >>= go
  where
    go [romFile] = B.readFile romFile >>= exec . buildState . buildMemory
    go _ = putStrLn "agument should be a single ROM file"

    buildState m = execState reset (powerOnState { mem = m })

    exec s0 = do
        stopIO <- startIO
        loop (0 :: Int) s0
        stopIO

    loop n s = do
        let pcsp = (rPC s, rS s)
        (n',s') <- processIO n (execState executeOne s)
        let pcsp' = (rPC s', rS s')
        if pcsp /= pcsp'
            then (loop $! n') $! s'
            else do
                putStrLn $ "Execution snagged at " ++ show (fst pcsp')

    startIO = do
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

    processIO n s = do
        when (busW s == Just outPortAddr) $ do
            let c = fetchByte outPortAddr $ mem s
            when (c /= 0) $ hPutChar stdout $ toEnum $ fromIntegral c
        if (busR s == Just inPortAddr)
            then do
                r <- if n < 16
                        then hWaitForInput stdin 50
                        else hReady stdin
                c <- if r then (fromIntegral . fromEnum) <$> hGetChar stdin else return 0
                let c' = if c == 0xA then 0xD else c
                let s' = s { mem = storeByte inPortAddr c' $ mem s }
                return (0,s')
            else return (n+1,s)

    inPortAddr = toAd 0xF004
    outPortAddr = toAd 0xF001
