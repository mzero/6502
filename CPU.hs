module CPU
    ( S(..), powerOnState
    , St

    , zeroPage
    , bitN, bitV, bitD, bitI, bitZ, bitC

    , assignBit
    , assignZN

    , setZN, setAZN, setXZN, setYZN
    , setZVNbit, setACZVN, setCZN

    , fetch, store
    , clearBus
    , fetchIndirectAddr
    , nextPC, fetchPC

    , push, pull, pushAddr, pullAddr, pushP, pullP
    )
  where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (State, get, gets, modify, put)
import Data.Bits ((.&.), clearBit, setBit, testBit)
import Data.Word (Word8)

import Memory

zeroPage :: Word8 -> Addr
zeroPage v = makeAddr v 0


data S = S { regA, regX, regY, regP, regS :: !Word8
           , regPC :: !Addr
           , memory :: !Memory
           , addrRead, addrWrite :: Maybe Addr
           }
type St = State S

powerOnState :: S
powerOnState = S
        { regA = 0, regX = 0, regY = 0, regP = 0, regS = 0
        , regPC = makeAddr 0 0xFF
        , memory = emptyMemory
        , addrRead = Nothing, addrWrite = Nothing
        }

[bitN, bitV, bitX, bitB, bitD, bitI, bitZ, bitC] = [7,6..0]

assignBit bit bool byte = (if bool then setBit else clearBit) byte bit

assignZ v = assignBit bitZ (v == 0)
assignZN v = assignBit bitZ (v == 0) . assignBit bitN (testBit v 7)
assign67 v = assignBit bitV (testBit v 6) . assignBit bitN (testBit v 7)

setZN v = modify $ \s -> s { regP = assignZN v $ regP s }
setAZN v = modify $ \s -> s { regA = v, regP = assignZN v $ regP s }
setXZN v = modify $ \s -> s { regX = v, regP = assignZN v $ regP s }
setYZN v = modify $ \s -> s { regY = v, regP = assignZN v $ regP s }

setZVNbit (a,v) = modify $ \s -> s { regP = assignZ (a .&. v) $ assign67 v $ regP s }

setACZVN (c,v,a) = modify $ \s ->
    s { regA = a, regP = assignBit bitC c $ assignBit bitV v $ assignZN a $ regP s }
setCZN (c,v) = modify $ \s -> s { regP = assignBit bitC c $ assignZN v $ regP s }


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
fetchPC = nextPC >>= \addr -> gets memory >>= return . fetchByte addr

push :: Word8 -> St ()
push v = do
    s <- get
    let sp = regS s
    put s { regS = sp - 1 }
    store (makeAddr sp 1) v

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

pushP :: Bool -> St ()
pushP fromSW = gets regP >>= push . assignBit bitX True . assignBit bitB fromSW

pullP :: St ()
pullP = pull >>= \v -> modify $ \s -> s { regP = v .&. 0xCF }
