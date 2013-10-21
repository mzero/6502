module Dump
    ( dumpReg
    , disasm
    , disasmSpacer
    )
 where

import Data.Bits (testBit)
import Data.List (transpose)
import qualified Data.Vector as V
import Data.Word (Word8)
import Text.Printf

import CPU
import Memory

dumpReg :: S -> String
dumpReg s = regs ++ flags ++ stack ++ busRead ++ busWrite
  where
    regs = printf "A:%02x X:%02x Y:%02x" (regA s) (regX s) (regY s)
    flags = "  " ++ pStr (regP s)
    stack = printf "  SP:%02x/%02x %02x %02x" (regS s) sp1 sp2 sp3
    busRead = maybe "        " (bus 'R') $ addrRead s
    busWrite = maybe "        " (bus 'W') $ addrWrite s

    mem = memory s
    (_:sp1:sp2:sp3:_) = map (flip fetchByte mem) $ iterate nextAddrWrap $ makeAddr (regS s) 1
    pStr v = zipWith (\c b -> if testBit v b then c else '-') "NVDIZC" [7,6,3,2,1,0]
    bus t addr = let (lo,hi) = splitAddr addr in printf "  %c:%02x%02x" t hi lo

disasm :: S -> String
disasm s = printf "%02x%02x: %-13s " pchi pclo inst
  where
    mem = memory s
    pc = regPC s
    (pclo, pchi) = splitAddr pc
    insBytes@(in0:_) = map (flip fetchByte mem) $ iterate nextAddrFull pc
    inst = (decode V.! fromIntegral in0) insBytes

disasmSpacer :: String
disasmSpacer = replicate 20 ' '

decode :: V.Vector ([Word8] -> String)
decode = V.fromList $ concat $ transpose
    [ col0, col1, col2, col3, col4, col5, col6, col7
    , col8, col9, colA, colB, colC, colD, colE, colF ]
  where
    col0 = [ addrImm "BRK",  addrRel "BPL", addrAbs "JSR",  addrRel "BMI"
           , addrImpl "RTI", addrRel "BVC", addrImpl "RTS", addrRel "BVS"
           , insErr,         addrRel "BCC", addrImm "LDY",  addrRel "BCS"
           , addrImm "CPY",  addrRel "BNE", addrImm "CPX",  addrRel "BEQ"
           ]
    col1 = colAlu addrIndIdx addrIdxInd
    col2 = colErr `except` (0xA, addrImm "LDX")
    col3 = colErr

    col4 = [ insErr,         insErr,          addrZero "BIT", insErr
           , insErr,         insErr,          insErr,         insErr
           , addrZero "STY", addrZeroX "STY", addrZero "LDY", addrZeroX "LDY"
           , addrZero "CPY", insErr,          addrZero "CPX", insErr
           ]
    col5 = colAlu addrZero addrZeroX
    col6 = colBit addrZero addrZeroX `except` (0x9, addrZeroY "STX")
                                     `except` (0xB, addrZeroY "LDX")
    col7 = colErr

    col8 = [ addrImpl "PHP", addrImpl "CLC", addrImpl "PLP", addrImpl "SEC"
           , addrImpl "PHA", addrImpl "CLI", addrImpl "PLA", addrImpl "SEI"
           , addrImpl "DEY", addrImpl "TYA", addrImpl "TAY", addrImpl "CLV"
           , addrImpl "INY", addrImpl "CLD", addrImpl "INX", addrImpl "SED"
           ]
    col9 = colAlu addrImm addrAbsY `except` (0x8, insErr)
    colA = [ addrAcc "ASL",  insErr,         addrAcc "ROL",  insErr
           , addrAcc "LSR",  insErr,         addrAcc "ROR",  insErr
           , addrImpl "TXA", addrImpl "TXS", addrImpl "TAX", addrImpl "TSX"
           , addrImpl "DEX", insErr,         addrImpl "NOP", insErr
           ]
    colB = colErr

    colC = [ insErr,        insErr, addrAbs "BIT", insErr
           , addrAbs "JMP", insErr, addrInd "JMP", insErr
           , addrAbs "STY", insErr, addrAbs "LDY", addrAbsX "LDY"
           , addrAbs "CPY", insErr, addrAbs "CPX", insErr
           ]
    colD = colAlu addrAbs addrAbsX
    colE = colBit addrAbs addrAbsX `except` (0x9, insErr)
                                   `except` (0xB, addrAbsY "LDX")
    colF = colErr

    colAlu e o = concatMap (\i -> [e i, o i])
                ["ORA", "AND", "EOR", "ADC", "STA", "LDA", "CMP", "SBC"]
    colBit e o = concatMap (\i -> [e i, o i])
                ["ASL", "ROL", "LSR", "ROR", "STX", "LDX", "DEC", "INC"]

    colErr = replicate 16 insErr

    except :: [a] -> (Int, a) -> [a]
    [] `except` _ = []
    (i:is) `except` (n, j) | n == 0    = j : is
                           | otherwise = i : is `except` (n-1, j)

addrImpl sym = insOneByte sym ""
addrAcc sym = insOneByte sym "A"
addrImm sym = insTwoByte sym "#" ""
addrZero sym = insTwoByte sym "" ""
addrZeroX sym = insTwoByte sym "" ",X"
addrZeroY sym = insTwoByte sym "" ",Y"
addrRel sym = insTwoByte sym "" ""
addrAbs sym = insThreeByte sym "" ""
addrAbsX sym = insThreeByte sym "" ",X"
addrAbsY sym = insThreeByte sym "" ",Y"
addrInd sym = insThreeByte sym "(" ")"
addrIndIdx sym = insTwoByte sym "(" ",X)"
addrIdxInd sym = insTwoByte sym "(" "),Y"

insErr (ins:_) = printf "$%02x ???" ins
insErr _ = error "no instruction byte"
insOneByte sym op _ = sym ++ " " ++ op
insTwoByte sym pre post (_:op:_) =
    sym ++ " " ++ pre ++ printf "$%02x" op ++ post
insTwoByte _ _ _ _ = error "no more instruction bytes"
insThreeByte sym pre post (_:op1:op2:_) =
    sym ++ " " ++ pre ++ printf "$%02x%02x" op2 op1 ++ post
insThreeByte _ _ _ _ = error "not enough instruction bytes"

