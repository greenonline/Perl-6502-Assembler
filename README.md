# Perl-6502-Assembler
An "assembler" for 6502 written in Perl

This is probably the most pointless tool, a 6502 assembly code parser, written in Perl. You can't really call it an assembler, because, whle it *does* assemble the code, it can't produce any binaries, only the hex code.

I only wrote it as I had become fasinated by the 6502 assembler written in BASIC, by Mark Zimmermann, published in Personal Computing Decemeber 1978. I had intended just to "deconstruct" the Zimmermann engine into pseudo code and a flowchart, but then I started writing a Perl script in parallel, which eventually took on a life of its own and the pseudo code and flowchart never got finished.

One *sort of* useful thing that it *does* do is to display the number of cycles per instruction. So there is that, at least, I guess.

Labels are supported, as are variables. 

Numbers may be in hex or decimal.

The supported list of "tokens" is:

 - `ORG`
 - `DB`
 - `DC`
 - `DW`
 - `END`

The error checking is not extensive, but dodgy mnemonics should be detected.

Example output

```none
Labels found:
LABEL1 : 0326   
MORE : 032B   


ORG $300
CREG = $400
IMM = $38
ADD1 = 33025
0300   A9 66                   LDA #102             2
0302   A2 00                   LDX #0               2
0304   9D 00 80                STA 32768,X          4
0307   E8                      INX                  2
0308   F0 3D                   BEQ $347             2
030A   4C 3E 03                JMP 830              3
030D   EA                      NOP                  2
030E   EA                      NOP                  2
030F   9D 00 81                STA 33024,X          4
0312   9D 01 81                STA ADD1,X           4
0315   E8                      INX                  2
0316   F0 3A                   BEQ $352             2
0318   4C 49 03                JMP 841              3
031B   56 80                   LSR 128,X            6
031D   8E 09 81                STX 33033            4
0320   00                      BRK                  7
0321   AD 00 04                LDA CREG             4
0324   A9 38                   LDA #IMM             2
0326   AD 00 04      LABEL1    LDA CREG             4
0329   A9 38                   LDA #IMM             2
032B   EA            MORE      NOP                  2
032C   EA                      NOP                  2
032D   9D 00 81                STA 33024,X          4
0330   9D 01 81                STA ADD1,X           4
0333   E8                      INX                  2
0334   F0 1C                   BEQ $352             2
0336   4C 49 03                JMP 841              3
0339   56 80                   LSR 128,X            6
033B   8E 09 81                STX 33033            4
033E   00                      BRK                  7
033F   32                      DB $32
0340   23                      DC $23
0341   FF FF                   DW $FFFF
END

```
