
BASE @ HEX
GET-CURRENT
ALSO *scope DEFINITIONS PREVIOUS

WORDLIST CONSTANT asm-wid
: ASSEMBLER   GET-ORDER NIP asm-wid SWAP SET-ORDER ;
: ]ASM   ALSO ASSEMBLER ; 
: ASM[   PREVIOUS ;
: [ASM]   ]ASM ; IMMEDIATE
: [END-ASM]   ASM[ ; IMMEDIATE

ALSO ASSEMBLER DEFINITIONS

\ Enumerations
: enumerate:  ( N start "name1" ... "nameN" -- )
   TUCK + SWAP ?DO   I CONSTANT  LOOP ;

\ N field operations
#1 1
enumerate: N<-T#

\ T field operations
#3 1
enumerate: T<-N# T<-R# T<-IR#

\ R field operations
#3 1
enumerate: R<-M1# R<-PC# R<-T#

\ PC field operations
#3 1
enumerate: PC<-R# PC<-IR# PC<-?IR#

\ RAM field operations
#7 1
enumerate: N<-POP# R<-POP# N<-RAM# T<-RAM# PUSH<-N# PUSH<-R# RAM<-N#

\ ALU field operations
#15 1
enumerate: T<-ALU_NOT# T<-ALU_AND# T<-ALU_OR# T<-ALU_XOR# T<-ALU_SHL# T<-ALU_SHR# T<-ALU_ADD# T<-ALU_SUB# T<-ALU_MUL# T<-ALU_DIV# T<-ALU_ADD1# T<-ALU_ADD2# 13ALU0# 14ALU0# T<-ALU_CARRY#

\ ALU EXT1 field operations
#15 1
enumerate: 1ALU1# 2ALU1# 3ALU1# 4ALU1# T<-ALU_SHLC# T<-ALU_SHRC# T<-ALU_ADDC# T<-ALU_SUBC# 9ALU1# 10ALU1# T<-ALU_ADD1C# T<-ALU_ADD2C# 13ALU1# T<-ALU_IRQEN# T<-ALU_SLEEP#

\ ALU EXT2 field operations
#15 1
enumerate: 1ALU2# 2ALU2# 3ALU2# 4ALU2# 5ALU2# 6ALU2# 7ALU2# 8ALU2# 9ALU2# 10ALU2# 11ALU2# 12ALU2# 13ALU2# T<-ALU_IRQSET# T<-ALU_IRQDIS#

\ ALU EXT3 field operations
#15 1
enumerate: 1ALU3# 2ALU3# 3ALU3# 4ALU3# 5ALU3# 6ALU3# 7ALU3# 8ALU3# 9ALU3# 10ALU3# 11ALU3# 12ALU3# 13ALU3# 14ALU3# 15ALU3#

: N_>  ( ucode -- op# )
   #13 RSHIFT %1 AND ;
: N_0  ( ucode -- ucode )  \ reset N field operation
   %1 #13 LSHIFT INVERT AND ;
: >N_  ( ucode op# -- ucode )
   OVER N_> ABORT" N... field already set"
   %1 AND #13 LSHIFT OR ;

: T_>  ( ucode -- op# )
   #11 RSHIFT %11 AND ;
: T_0  ( ucode -- ucode )  \ reset T field operation
   %11 #11 LSHIFT INVERT AND ;
: >T_  ( ucode op# -- ucode )
   OVER T_> ABORT" T... field already set"
   %11 AND #11 LSHIFT OR ;

: R_>  ( ucode -- op# )
   #9 RSHIFT %11 AND ;
: R_0  ( ucode -- ucode )  \ reset R field operation
   %11 #9 LSHIFT INVERT AND ;
: >R_  ( ucode op# -- ucode )
   OVER R_> ABORT" R... field already set"
   %11 AND #9 LSHIFT OR ;

: PC_>  ( ucode -- op# )
   #7 RSHIFT %11 AND ;
: PC_0  ( ucode -- ucode )  \ reset PC field operation
   %11 #7 LSHIFT INVERT AND ;
: >PC_  ( ucode op# -- ucode )
   OVER PC_> ABORT" PC field already set"
   %11 AND #7 LSHIFT OR ;

: RAM_>  ( ucode -- op# )
   #4 RSHIFT %111 AND ;
: RAM_0  ( ucode -- ucode )  \ reset RAM field operation
   %111 #4 LSHIFT INVERT AND ;
: >RAM_  ( ucode op# -- ucode )
   OVER RAM_> ABORT" RAM field already set"
   %111 AND #4 LSHIFT OR ;

: ALU_>  ( ucode -- op# )
   #0 RSHIFT %1111 AND ;
: ALU_0  ( ucode -- ucode )  \ reset ALU field operation
   %1111 #0 LSHIFT INVERT AND ;
: >ALU_  ( ucode op# -- ucode)
   OVER ALU_> ABORT" ALU field already set"
   %1111 AND #0 LSHIFT OR ;

: EXT_RAM_>  ( ucode -- op# )
   #0 RSHIFT %11 AND ;
: EXT_RAM_0  ( ucode -- ucode )  \ reset EXT_RAM field operation
   %11 #0 LSHIFT INVERT AND ;
: >EXT_RAM_  ( ucode op# -- ucode )
   OVER EXT_RAM_> ABORT" EXT_RAM field already set"
   %11 AND #0 LSHIFT OR ;

: N<-T  ( ucode -- ucode )
   N<-T# >N_ ;
: T<-N  ( ucode -- ucode )
   T<-N# >T_ ;
: T<-R  ( ucode -- ucode )
   T<-R# >T_ ;
: T<-IR  ( ucode -- ucode )
   T<-IR# >T_ ;

\ ALU instructions
: T<-ALU_NOT  ( ucode -- ucode )
   T<-ALU_NOT# >ALU_ ;
: T<-ALU_AND  ( ucode -- ucode )
   T<-ALU_AND# >ALU_ ;
: T<-ALU_OR  ( ucode -- ucode )
   T<-ALU_OR# >ALU_ ;
: T<-ALU_XOR  ( ucode -- ucode )
   T<-ALU_XOR# >ALU_ ;
: T<-ALU_SHL  ( ucode -- ucode )
   T<-ALU_SHL# >ALU_ ;
: T<-ALU_SHR  ( ucode -- ucode )
   T<-ALU_SHR# >ALU_ ;
: T<-ALU_ADD  ( ucode -- ucode )
   T<-ALU_ADD# >ALU_ ;
: T<-ALU_SUB  ( ucode -- ucode )
   T<-ALU_SUB# >ALU_ ;
: T<-ALU_MUL  ( ucode -- ucode )
   T<-ALU_MUL# >ALU_ ;
: T<-ALU_DIV  ( ucode -- ucode )
   T<-ALU_DIV# >ALU_ ;
: T<-ALU_ADD1  ( ucode -- ucode )
   T<-ALU_ADD1# >ALU_ ;
: T<-ALU_ADD2  ( ucode -- ucode )
   T<-ALU_ADD2# >ALU_ ;
: T<-ALU_CARRY  ( ucode -- ucode )
   T<-ALU_CARRY# >ALU_ ;

\ ALU EXT1 instructions
: T<-ALU_SHLC  ( ucode -- ucode )
   T<-ALU_SHLC# >ALU_
   T<-N ;
: T<-ALU_SHRC  ( ucode -- ucode )
   T<-ALU_SHRC# >ALU_
   T<-N ;
: T<-ALU_ADDC  ( ucode -- ucode )
   T<-ALU_ADDC# >ALU_
   T<-N ;
: T<-ALU_SUBC  ( ucode -- ucode )
   T<-ALU_SUBC# >ALU_
   T<-N ;
: T<-ALU_ADD1C  ( ucode -- ucode )
   T<-ALU_ADD1C# >ALU_
   T<-N ;
: T<-ALU_ADD2C  ( ucode -- ucode )
   T<-ALU_ADD2C# >ALU_
   T<-N ;
: T<-ALU_IRQEN  ( ucode -- ucode )
   T<-ALU_IRQEN# >ALU_
   T<-N ;
: T<-ALU_SLEEP  ( ucode -- ucode )
   T<-ALU_SLEEP# >ALU_
   T<-N ;

\ ALU EXT2 instructions
: T<-ALU_IRQSET  ( ucode -- ucode )
   T<-ALU_IRQSET# >ALU_
   T<-R ;
: T<-ALU_IRQDIS  ( ucode -- ucode )
   T<-ALU_IRQDIS# >ALU_
   T<-R ;

\ ALU EXT3 instructions
\ : T<-ALU_XXX  ( ucode -- ucode )
\    T<-ALU_XXX# >ALU_
\    T<-IR ;

: R<-M1  ( ucode -- ucode )
   R<-M1# >R_ ;
: R<-PC  ( ucode -- ucode )
   R<-PC# >R_ ;
: R<-T  ( ucode -- ucode )
   R<-T# >R_ ;
: PC<-R  ( ucode -- ucode )
   PC<-R# >PC_ ;
: PC<-IR  ( ucode -- ucode )
   PC<-IR# >PC_ ;
: PC<-?IR  ( ucode -- ucode )
   PC<-?IR# >PC_ ;

\ Memory instructions
: PUSH<-N  ( ucode -- ucode )
   DUP RAM_>
   CASE
      N<-POP# OF N<-T ENDOF
      R<-POP# OF R<-T ENDOF
      N<-RAM# OF N<-T ENDOF
      T<-RAM# OF T<-N ENDOF
      0 OF PUSH<-N# >RAM_ EXIT ENDOF
      ABORT" RAM field already set"
   ENDCASE
   PUSH<-N# >EXT_RAM_ ;
: PUSH<-R  ( ucode -- ucode )
   DUP RAM_>
   CASE
      N<-POP# OF N<-T ENDOF
      R<-POP# OF R<-T ENDOF
      N<-RAM# OF N<-T ENDOF
      T<-RAM# OF T<-N ENDOF
      0 OF PUSH<-R# >RAM_ EXIT ENDOF
      ABORT" RAM field already set"
   ENDCASE
   PUSH<-R# >EXT_RAM_ ;
: RAM<-N  ( ucode -- ucode )
   DUP RAM_>
   CASE
      N<-POP# OF N<-T ENDOF
      R<-POP# OF R<-T ENDOF
      N<-RAM# OF N<-T ENDOF
      T<-RAM# OF T<-N ENDOF
      0 OF RAM<-N# >RAM_ EXIT ENDOF
      ABORT" RAM field already set"
   ENDCASE
   RAM<-N# >EXT_RAM_ ;
: N<-POP  ( ucode -- ucode )
   DUP RAM_>
   DUP PUSH<-N# =
   OVER PUSH<-R# = OR
   OVER RAM<-N# = OR IF
      >EXT_RAM_
      N<-T
   ELSE
      ABORT" RAM field already set"
   THEN
   RAM_0 N<-POP# >RAM_ ;
: R<-POP  ( ucode -- ucode )
   DUP RAM_>
   DUP PUSH<-N# =
   OVER PUSH<-R# = OR
   OVER RAM<-N# = OR IF
      >EXT_RAM_
      R<-T
   ELSE
      ABORT" RAM field already set"
   THEN
   RAM_0 R<-POP# >RAM_ ;
: N<-RAM  ( ucode -- ucode )
   DUP RAM_>
   DUP PUSH<-N# =
   OVER PUSH<-R# = OR
   OVER RAM<-N# = OR IF
      >EXT_RAM_
      N<-T
   ELSE
      ABORT" RAM field already set"
   THEN
   RAM_0 N<-RAM# >RAM_ ;
: T<-RAM  ( ucode -- ucode )
   DUP RAM_>
   DUP PUSH<-N# =
   OVER PUSH<-R# = OR
   OVER RAM<-N# = OR IF
      >EXT_RAM_
      T<-N
   ELSE
      ABORT" RAM field already set"
   THEN
   RAM_0 T<-RAM# >RAM_ ;

: ucode,  ( ucode -- )
   t, ;
0 CONSTANT ucode0

: nop_asm  ( -- )
   ucode0 ucode, ;
: drop_asm  ( -- )
   ucode0 T<-N N<-POP ucode, ;
: rdrop_asm  ( -- )
   ucode0 R<-POP ucode, ;
: dup_asm  ( -- )
   ucode0 N<-T PUSH<-N ucode, ;
: rdup_asm  ( -- )
   ucode0 PUSH<-R ucode, ;
: over_asm  ( -- )
   ucode0 T<-N N<-T PUSH<-N ucode, ;
: swap_asm  ( -- )
   ucode0 T<-N N<-T ucode, ;
: rswap_asm  ( -- )
   ucode0 R<-POP PUSH<-R ucode, ;
: nip_asm  ( -- )
   ucode0 N<-POP ucode, ;
: ret_asm  ( -- )
   ucode0 PC<-R R<-POP ucode, ;
: reti_asm  ( -- )
   ucode0 PC<-R R<-POP T<-ALU_IRQEN ucode, ;
: rcall_asm  ( -- )
   ucode0 PC<-R R<-PC ucode, ;
: r>_asm  ( -- )
   ucode0 T<-R N<-T PUSH<-N R<-POP ucode, ;
: >r_asm  ( -- )
   ucode0 R<-T PUSH<-R T<-N N<-POP ucode, ;
: dup>r_asm  ( -- )
   ucode0 R<-T PUSH<-R ucode, ;
: r@_asm  ( -- )
   ucode0 T<-R N<-T PUSH<-N ucode, ;
: r!_asm  ( -- )
   ucode0 R<-T T<-N N<-POP ucode, ;
: r@!_asm  ( -- )
   ucode0 T<-R R<-T ucode, ;
: rotswap_asm  ( -- )
   ucode0 N<-POP PUSH<-N ucode, ;
: @_asm  ( -- )
   ucode0 T<-RAM ucode, ;
: dup@swap_asm  ( -- )
   ucode0 N<-RAM PUSH<-N ucode, ;
: !_asm  ( -- )
   ucode0 RAM<-N N<-POP ucode, ;
: @!_asm  ( -- )
   ucode0 T<-RAM RAM<-N ucode, ;
: not_asm  ( -- )
   ucode0 T<-ALU_NOT ucode, ;
: and_asm  ( -- )
   ucode0 T<-ALU_AND N<-POP ucode, ;
: or_asm  ( -- )
   ucode0 T<-ALU_OR N<-POP ucode, ;
: xor_asm  ( -- )
   ucode0 T<-ALU_XOR N<-POP ucode, ;
: shl_asm  ( -- )
   ucode0 T<-ALU_SHL ucode, ;
: shr_asm  ( -- )
   ucode0 T<-ALU_SHR ucode, ;
: add_asm  ( -- )
   ucode0 T<-ALU_ADD N<-POP ucode, ;
: sub_asm  ( -- )
   ucode0 T<-ALU_SUB N<-POP ucode, ;
: mul_asm  ( -- )
   ucode0 T<-ALU_MUL ucode, ;
: div_asm  ( -- )
   ucode0 T<-ALU_DIV ucode, ;
: add1_asm  ( -- )
   ucode0 T<-ALU_ADD1 ucode, ;
: add2_asm  ( -- )
   ucode0 T<-ALU_ADD2 ucode, ;
: carry_asm  ( -- )
   ucode0 T<-ALU_CARRY N<-T PUSH<-N ucode, ;
: shlc_asm  ( -- )
   ucode0 T<-ALU_SHLC ucode, ;
: shrc_asm  ( -- )
   ucode0 T<-ALU_SHRC ucode, ;
: addc_asm  ( -- )
   ucode0 T<-ALU_ADDC N<-POP ucode, ;
: subc_asm  ( -- )
   ucode0 T<-ALU_SUBC N<-POP ucode, ;
: add1c_asm  ( -- )
   ucode0 T<-ALU_ADD1C ucode, ;
: add2c_asm  ( -- )
   ucode0 T<-ALU_ADD2C ucode, ;
: irqen_asm  ( -- )
   ucode0 T<-ALU_IRQEN ucode, ;
: sleep_asm  ( -- )
   ucode0 T<-ALU_SLEEP N<-T PUSH<-N ucode, ;
: irqset_asm  ( -- )
   ucode0 T<-ALU_IRQSET ucode, ;
: irqdis_asm  ( -- )
   ucode0 T<-ALU_IRQDIS N<-T PUSH<-N ucode, ;

: shorti?  ( n1 -- n2 flag )
   DUP $FFFF > OVER $-8000 <
   OR ABORT" Immediate argument out of range"
   DUP $8000 AND IF  \ extend sign
      $-10000 OR
   THEN
   DUP #-2048 #2048 WITHIN ;
: ldi_asm  ( n -- )
   shorti? IF
      $FFF AND $7000 OR t,
   ELSE
      $FFFF AND
      ucode0 T<-IR N<-T PUSH<-N ucode,
      t,
   THEN ;
: @-ldi  ( a-taddr -- n )  \ extract integer from ldi at a-taddr
   DUP t@
   DUP $F000 AND $7000 = IF
      NIP $FFF AND  \ short ldi
      DUP $800 AND IF
         $F000 OR  \ extend sign
      THEN
   ELSE
      ucode0 T<-IR N<-T PUSH<-N
      <> ABORT" No ldi instruction"
      tcell+ t@
   THEN ;

: call_asm  ( a-taddr -- )
   $FFFF AND 1 RSHIFT
   $8000 OR
   t, ;

: callu_asm  ( a-taddr -- )
   $FFFE AND
   ucode0 PC<-IR R<-PC PUSH<-R ucode,
   t, ;

: rel?  ( s d -- rel -1 | d 0 )  \ is relative branch from s to d?
   $FFFF AND SWAP
   $FFFF AND #11 RSHIFT
   OVER #11 RSHIFT -  ( d s-d )
   DUP -1 2 WITHIN IF
      CASE
         0 OF $000 ENDOF  \ same page of s
         -1 OF $400 ENDOF  \ following page of s
         1 OF $800 ENDOF  \ previous page of s
      ENDCASE
      SWAP 1 RSHIFT $3FF AND OR -1
   ELSE
      DROP 0
   THEN ;

: branch_asm  ( s d -- )
   rel? IF
      $4000 OR t,
   ELSE
      ucode0 PC<-IR ucode,
      t,
   THEN ;

: branchz_asm  ( s d -- )
   rel? IF
      $5000 OR t,
   ELSE
      ucode0 PC<-?IR T<-N N<-POP ucode,
      t,
   THEN ;

: next_asm  ( s d -- )
   rel? IF
      $6000 OR t,
   ELSE
      ucode0 PC<-?IR R<-M1 ucode,
      t,
   THEN ;

: unext_asm  ( ucode -- )
   there SWAP
   PC<-?IR R<-M1 ucode,
   t, ;

: ahead  ( -- s-taddr )  there $4000 t, ;
: if  ( -- s-taddr )  there $5000 t, ;
: then  ( s-taddr -- )
   DUP there rel? INVERT ABORT" No possible relative jump"
   OVER t@ OR SWAP t! ;
: else  ( s-taddr -- s-taddr )
   ahead SWAP then ;
: begin  ( -- d-taddr )  there ;
: again  ( d-taddr -- )  there SWAP branch_asm ;
: until  ( d-taddr -- )  there SWAP branchz_asm ;
: while  ( d-taddr -- s-taddr d-taddr )
   if SWAP ;
: repeat  ( s-taddr d-taddr -- )
   again then ;
: for  ( -- d-taddr )  there ;
: next  ( d-taddr -- )  there SWAP next_asm ;
: aft  ( d-taddr -- d-taddr s-taddr )
   DROP ahead begin SWAP ;

PREVIOUS SET-CURRENT
BASE !