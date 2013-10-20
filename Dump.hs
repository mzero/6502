module Dump
    ( dumpReg
    )
 where

import Data.Bits (testBit)
import Text.Printf

import CPU
import Memory

-- A:xx X:xx Y:xx  P:nv_bdizc  PC:xxxx (> xx xx xx)  SP:xx (xx xx xx ^)

dumpReg :: S -> String
dumpReg s = regs ++ flags ++ prog ++ stack
  where
    regs = printf "A:%02x X:%02x Y:%02x" (regA s) (regX s) (regY s)
    flags = "  P:" ++ pStr (regP s)
    prog = printf "  PC:%02x%02x (> %02x %02x %02x)" pchi pclo pc0 pc1 pc2
    stack = printf "  SP:%02x (%02x %02x %02x ^)" (regS s) sp3 sp2 sp1

    mem = memory s
    pc = regPC s
    (pclo, pchi) = splitAddr pc
    (pc0:pc1:pc2:_) = map (flip fetchByte mem) $ iterate nextAddrFull pc
    (_:sp1:sp2:sp3:_) = map (flip fetchByte mem) $ iterate prevAddrWrap $ makeAddr (regS s) 1
    pStr v = zipWith (\c b -> if testBit v b then c else '-') "NV?BDIZC" [7,6..0]
