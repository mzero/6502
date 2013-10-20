module Memory
    ( Addr, makeAddr, splitAddr
    , nextAddrWrap, nextAddrFull
    , prevAddrWrap, prevAddrFull
    , indexAddr, relativeAddr

    , Memory
    , emptyMemory, loadROM, loadRAM

    , fetchByte, storeByte
    )
  where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Word (Word8)

newtype Addr = Addr (Word8, Word8)      -- lo, hi
    deriving (Eq, Show)

makeAddr :: Word8 -> Word8 -> Addr
makeAddr lo hi = Addr (lo, hi)

splitAddr :: Addr -> (Word8, Word8)
splitAddr (Addr lohi) = lohi

nextAddrWrap :: Addr -> Addr
nextAddrWrap (Addr (lo, hi)) = Addr (lo + 1, hi)

prevAddrWrap :: Addr -> Addr
prevAddrWrap (Addr (lo, hi)) = Addr (lo -  1, hi)

nextAddrFull :: Addr -> Addr
nextAddrFull (Addr (lo, hi)) | lo == 0xFF = Addr (0, hi + 1)
                             | otherwise  = Addr (lo + 1, hi)

prevAddrFull :: Addr -> Addr
prevAddrFull (Addr (lo, hi)) | lo == 0 = Addr (0xFF, hi - 1)
                         | otherwise = Addr (lo - 1, hi)

indexAddr :: Addr -> Word8 -> Addr
indexAddr (Addr (lo, hi)) idx = Addr (lo', hi')
  where
    lo' = lo + idx
    hi' = hi + if lo <= lo' then 0 else 1

relativeAddr :: Addr -> Word8 -> Addr
relativeAddr (Addr (lo, hi)) off = Addr (lo', hi')
  where
    lo' = lo + off
    hi' = hi +
        if off <= 0x7F
            then if lo <= lo' then 0 else 1
            else if lo >= lo' then 0 else (-1)


data Page = Missing | ROM B.ByteString | RAM (V.Vector Word8)

fetchPageByte :: Word8 -> Page -> Word8
fetchPageByte _ Missing = 0
fetchPageByte lo (ROM bs) = B.index bs $ fromIntegral lo
fetchPageByte lo (RAM vs) = vs V.! fromIntegral lo

storePageByte :: Word8 -> Word8 -> Page -> Maybe Page
storePageByte lo v (RAM vs) = Just $ RAM $ vs V.// [(fromIntegral lo, v)]
storePageByte _ _ _ = Nothing

newtype Memory = Memory (V.Vector Page)

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 Missing

loadROM :: Word8 -> B.ByteString -> Memory -> Memory
loadROM p0 bs (Memory mv) =
    Memory $ mv V.// zip [fromIntegral p0..] (map ROM $ page bs)
  where
    page b = case B.length b of
        l | l == 0    -> []
          | l < 256   -> [b `B.append` B.replicate (256 - l) 0]
          | l == 256  -> [b]
          | otherwise -> let (b0,bn) = B.splitAt 256 b in b0 : page bn

loadRAM :: Word8 -> Int -> Memory -> Memory
loadRAM p0 n (Memory mv) =
    Memory $ mv V.// zip [fromIntegral p0..] (map RAM $ replicate n ramPage)
  where
    ramPage = V.replicate 256 0


fetchByte :: Addr -> Memory -> Word8
fetchByte (Addr (lo,hi)) (Memory mv) = fetchPageByte lo (mv V.! fromIntegral hi)

storeByte :: Addr -> Word8 -> Memory -> Memory
storeByte (Addr (lo,hi)) v m@(Memory mv) =
    maybe m updatePage $ storePageByte lo v page
  where
    page = mv V.! fromIntegral hi
    updatePage p = Memory $ mv V.// [(fromIntegral hi, p)]

