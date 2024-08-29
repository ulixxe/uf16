target

-1 equ TRUE
0 equ FALSE

\ 6.2.2298 ANS94 CORE EXT
TRUE constant true  ( -- true )

\ 6.2.1485 ANS94 CORE EXT
FALSE constant false  ( -- false )

\ 6.1.1370 ANS94 CORE
code execute  ( i*x xt -- j*x )
   >r_asm
   ret_asm
end-code

\ 6.1.1380 ANS94 CORE
code exit  ( -- )
   ret_asm
end-code   inline immediate

code nop  ( -- )
   nop_asm
   ret_asm
end-code   inline

\ 6.1.1260 ANS94 CORE
code drop  ( x -- )
   drop_asm
   ret_asm
end-code   inline

code rdrop  ( R: x -- )
   rdrop_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.2.1930 ANS94 CORE EXT
code nip  ( x1 x2 -- x2 )
   nip_asm
   ret_asm
end-code   inline

\ 6.1.0370 ANS94 CORE
code 2drop  ( x1 x2 -- )
   drop_asm
   drop_asm
   ret_asm
end-code

\ 6.1.1290 ANS94 CORE
code dup  ( x -- x x )
   dup_asm
   ret_asm
end-code   inline

\ 6.1.0630 ANS94 CORE
code ?dup  ( x -- 0 | x x )
   dup_asm if
      dup_asm
   then
   ret_asm
end-code

\ 6.1.0380 ANS94 CORE
code 2dup  ( x1 x2 -- x1 x2 x1 x2 )
   over_asm
   over_asm
   ret_asm
end-code

\ 6.1.1990 ANS94 CORE
code over  ( x1 x2 -- x1 x2 x1 )
   over_asm
   ret_asm
end-code   inline

\ 6.1.2260 ANS94 CORE
code swap  ( x1 x2 -- x2 x1 )
   swap_asm
   ret_asm
end-code   inline

\ 6.1.2160 ANS94 CORE
code rot  ( x1 x2 x3 -- x2 x3 x1 )
   rotswap_asm
   swap_asm
   ret_asm
end-code

code -rot  ( x1 x2 x3 -- x3 x1 x2 )
   swap_asm
   rotswap_asm
   ret_asm
end-code

\ 6.2.2300 ANS94 CORE EXT
code tuck  ( x1 x2 -- x2 x1 x2 )
   swap_asm
   over_asm
   ret_asm
end-code

\ 6.1.0580 ANS94 CORE
code >r  ( x -- ) ( R: -- x )
   >r_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.2.0340 ANS94 CORE EXT
code 2>r  ( x1 x2 -- ) ( R: -- x1 x2 )
   swap_asm
   >r_asm
   >r_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.1.2060 ANS94 CORE
code r>  ( -- x ) ( R: x -- )
   r>_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.2.0410 ANS94 CORE EXT
code 2r>  ( -- x1 x2 ) ( R: x1 x2 -- )
   r>_asm
   r>_asm
   swap_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.1.2070 ANS94 CORE
code r@  ( -- x ) ( R: x -- x )
   r@_asm
   ret_asm  \ it is inlined, otherwise doesn't work
end-code   inline

\ 6.1.0650 ANS94 CORE
code @  ( a-addr -- x )
   @_asm
   ret_asm
end-code   inline

\ 6.1.0350 ANS94 CORE
code 2@  ( a-addr -- x1 x2 )
   ucode0 PUSH<-N N<-RAM ucode,
   ucode0 T<-ALU_ADD2 ucode,
   ucode0 N<-RAM T<-N ucode,
   ret_asm
end-code

\ 6.1.0010 ANS94 CORE
code !  ( x a-addr -- )
   !_asm
   drop_asm
   ret_asm
end-code

\ 6.1.0310 ANS94 CORE
code 2!  ( x1 x2 a-addr -- )
   !_asm
   add2_asm !_asm
   drop_asm
   ret_asm
end-code

\ 6.1.0720 ANS94 CORE
code and  ( x1 x2 -- x3 )
   and_asm
   ret_asm
end-code   inline

\ 6.1.1980 ANS94 CORE
code or  ( x1 x2 -- x3 )
   or_asm
   ret_asm
end-code   inline

\ 6.1.2490 ANS94 CORE
code xor  ( x1 x2 -- x3 )
   xor_asm
   ret_asm
end-code   inline

\ 6.1.1720 ANS94 CORE
code invert  ( x1 -- x2 )
   not_asm
   ret_asm
end-code   inline

\ 6.1.1805 ANS94 CORE
code lshift  ( x1 u -- x2 )
   dup_asm if
      -1 ldi_asm add_asm >r_asm
      ucode0 T<-ALU_SHL unext_asm
      rdrop_asm
   else
      drop_asm
   then
   ret_asm
end-code

code u2/  ( u1 -- u2 )  \ unsigned right shift
   shl_asm shrc_asm   \ reset CARRY
   shrc_asm
   ret_asm
end-code

\ 6.1.2162 ANS94 CORE
code rshift  ( x1 u -- x2 )
   dup_asm if
      -1 ldi_asm add_asm
      swap_asm ] u2/ [ ]ASM swap_asm
      dup_asm if
         -1 ldi_asm add_asm
         >r_asm
         ucode0 T<-ALU_SHR unext_asm
         rdrop_asm
      else
         drop_asm
      then
   else
      drop_asm
   then
   ret_asm
end-code

\ 6.1.0320 ANS94 CORE
code 2*  ( x1 -- x2 )
   shl_asm
   ret_asm
end-code   inline

\ 6.1.0890 ANS94 CORE
code cells  ( n1 -- n2 )
   shl_asm
   ret_asm
end-code   inline

\ 6.1.0330 ANS94 CORE
code 2/  ( x1 -- x2 )
   shr_asm
   ret_asm
end-code   inline

\ 6.1.0120 ANS94 CORE
code +  ( n1|u1 n2|u2 -- n3|u3 )
   add_asm
   ret_asm
end-code   inline

\ 6.1.0130 ANS94 CORE
code +!  ( n|u a-addr -- )
   ucode0 T<-RAM R<-T PUSH<-R ucode,
   ucode0 T<-ALU_ADD ucode,
   ucode0 T<-R N<-T R<-POP ucode,
   !_asm drop_asm
   ret_asm
end-code

\ 6.1.0290 ANS94 CORE
code 1+  ( n1|u1 -- n2|u2 )
   add1_asm
   ret_asm
end-code   inline

\ 6.1.0870 ANS94 CORE
: c@  ( c-addr -- char )  \ little endian
   dup $FFFE and @
   swap $1 and if
      #8 rshift
   then
   $00FF and ;

\ 6.1.0850 ANS94 CORE
: c!  ( char c-addr -- )  \ little endian
   dup $FFFE and
   [ASM] dup>r_asm [END-ASM]
   @ swap $1 and if
      $00FF and
      swap #8 lshift
   else
      $FF00 and
      swap $00FF and
   then
   or r> ! ;

\ 6.1.0897 ANS94 CORE
: char+  ( c-addr1 -- c-addr2 )
   1+ ;   inline

\ 6.1.0898 ANS94 CORE
: chars  ( n1 -- n2 )
;   inline

\ 6.1.0880 ANS94 CORE
code cell+  ( a-addr1 -- a-addr2 )
   add2_asm
   ret_asm
end-code   inline

\ 6.1.0160 ANS94 CORE
code -  ( n1|u1 n2|u2 -- n3|u3 )
   sub_asm
   ret_asm
end-code   inline

\ 6.1.0300 ANS94 CORE
: 1-  ( n1|u1 -- n2|u2 )
   1 - ;

\ 6.1.1910 ANS94 CORE
: negate  ( n1 -- n2 )
   invert 1+ ;

\ 8.6.1.1230 ANS94 DOUBLE
code dnegate  ( d1 -- d2 )
   not_asm
   swap_asm
   not_asm
   add1_asm
   swap_asm
   0 ldi_asm
   addc_asm
   ret_asm
end-code

\ 6.1.0690 ANS94 CORE
code abs  ( n -- u )
   dup_asm
   shlc_asm
   ucode0 T<-ALU_CARRY ucode, if
      not_asm
      add1_asm
   then
   ret_asm
end-code

\ 6.2.0500 ANS94 CORE EXT
: <>  ( x1 x2 -- flag )
   xor dup if drop TRUE then ;

\ 6.1.0530 ANS94 CORE
: =  ( x1 x2 -- flag )
   xor if FALSE else TRUE then ;

\ 6.1.0480 ANS94 CORE
code <  ( n1 n2 -- flag )
   ucode0 PUSH<-N N<-T T<-ALU_XOR ucode,
   shlc_asm
   ucode0 T<-ALU_CARRY ucode, if
      drop_asm
      shlc_asm
   else
      sub_asm
   then
   ucode0 T<-ALU_CARRY ucode,
   ret_asm
end-code

: >=  ( n1 n2 -- flag )
   < invert ;

\ 6.1.0540 ANS94 CORE
: >  ( n1 n2 -- flag )
   swap < ;

: <=  ( n1 n2 -- flag )
   > invert ;

\ 6.2.0260 ANS94 CORE EXT
: 0<>  ( x -- flag )
   if TRUE else FALSE then ;

\ 6.1.0270 ANS94 CORE
: 0=  ( x -- flag )
   if FALSE else TRUE then ;

\ 6.1.0250 ANS94 CORE
code 0<  ( n -- flag )
   shlc_asm
   ucode0 T<-ALU_CARRY ucode,
   ret_asm
end-code

\ 6.2.0280 ANS94 CORE EXT
code 0>  ( n -- flag )
   dup_asm
   if
      not_asm shlc_asm
      ucode0 T<-ALU_CARRY ucode,
   then
   ret_asm
end-code

\ 6.1.2340 ANS94 CORE
code u<  ( u1 u2 -- flag )
   sub_asm
   ucode0 T<-ALU_CARRY ucode,
   ret_asm
end-code

\ 6.2.2350 ANS94 CORE EXT
: u>  ( u1 u2 -- flag )
   swap u< ;

\ 6.1.1880 ANS94 CORE
: min  ( n1 n2 -- n3 )
   2dup < if drop else nip then ;

\ 6.1.1870 ANS94 CORE
: max  ( n1 n2 -- n3 )
   2dup < if nip else drop then ;

\ 6.1.2360 ANS94 CORE
code um*  ( u1 u2 -- ud )
   0 ldi_asm
   #15 ldi_asm >r_asm
   ucode0 T<-ALU_MUL unext_asm
   rdrop_asm
   nip_asm
   ret_asm
end-code

host headless on target
: ?dnegate  ( d f -- d' )
   0< if dnegate then ;
host headless off target

\ 6.1.1810 ANS94 CORE
: m*  ( n1 n2 -- d )
   2dup xor >r
   abs swap abs um*
   r> ?dnegate ;

\ 6.1.0090 ANS94 CORE
: *  ( n1|u1 n2|u2 -- n3|u3 )
   m* drop ;

\ 6.1.2370 ANS94 CORE
code um/mod  ( ud u1 -- u2 u3 )  \ -- rem quot
   swap_asm
   #32 ldi_asm shr_asm  \ reset carry
   >r_asm
   ucode0 T<-ALU_DIV unext_asm
   rdrop_asm
   nip_asm
   shrc_asm  \ remainder rollback and carry load
   carry_asm ] abort" Out of range quotient" [ ]ASM
   swap_asm
   ret_asm
end-code

: ud/mod  ( ud1 u1 -- u2 ud2 )  \ -- rem quot
   >r 0 r@ um/mod r> swap >r um/mod r> ;

\ 8.6.1.1160 ANS94 DOUBLE
: dabs  ( d -- ud )
   dup ?dnegate ;

host headless on target
: ?negate  ( n f -- n' )
   0< if negate then ;
host headless off target

\ 6.1.1561 ANS94 CORE
: fm/mod  ( d n1 -- n2 n3 )  \ floored
   dup 0< dup >r
   if negate >r dnegate r> then
   >r dup 0< if r@ + then r> um/mod r>
   if swap negate swap then ;

\ 6.1.2214 ANS94 CORE
: sm/rem  ( d n1 -- n2 n3 )  \ symmetric
   2dup xor >r  \ quotient sign
   over >r      \ remainder sign
   abs >r dabs
   r> um/mod swap
   r> ?negate
   swap r> ?negate ;

\ 6.1.2170 ANS94 CORE
: s>d  ( n -- d )
   dup 0< ;

\ 6.1.0240 ANS94 CORE
: /mod  ( n1 n2 -- n3 n4 )
   >r s>d r> fm/mod ;

\ 6.1.1890 ANS94 CORE
: mod  ( n1 n2 -- n3 )
   /mod drop ;

\ 6.1.0230 ANS94 CORE
: /  ( n1 n2 -- n3 )
   /mod nip ;

\ 6.1.0110 ANS94 CORE
: */mod  ( n1 n2 n3 -- n4 n5 )
   >r m* r> fm/mod ;

\ 6.1.0100 ANS94 CORE
: */  ( n1 n2 n3 -- n4 )
   */mod nip ;

\ 6.1.0430 ANS94 CORE
: 2swap  ( xd1 xd2 -- xd2 xd1 )
   rot >r
   rot r> ;

\ 6.1.0400 ANS94 CORE
: 2over  ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
   2>r 2dup 2r> 2swap ;

\ 8.6.2.0420 ANS94 DOUBLE EXT
: 2rot  ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
   >r >r 2swap r> r> 2swap ;

\ 8.6.1.1040 ANS94 DOUBLE
: d+  ( ud1|d1 ud2|d2 -- ud3|d3 )
   rot 2swap
   + -rot
   [ASM] addc_asm [END-ASM] ;

\ 8.6.1.1050 ANS94 DOUBLE
: d-  ( ud1|d1 ud2|d2 -- ud3|d3 )
   rot swap 2swap
   - -rot
   [ASM] subc_asm [END-ASM] ;

\ 8.6.1.1120 ANS94 DOUBLE
: d=  ( xd1 xd2 -- flag )
   rot xor  -rot xor
   or if false else true then ;

\ 8.6.1.1110 ANS94 DOUBLE
: d<  ( d1 d2 -- flag )
   rot 2dup = if 2drop u< else > nip nip then ;

\ 8.6.2.1270 ANS94 DOUBLE EXT
: du<  ( ud1 ud2 -- flag )
   rot 2dup = if 2drop u< else u> nip nip then ;

\ 8.6.1.1090 ANS94 DOUBLE
: d2*  ( xd1 -- xd2 )
   2dup d+ ;

\ 8.6.1.1100 ANS94 DOUBLE
code d2/  ( xd1 -- xd2 )
   shr_asm
   swap_asm shrc_asm swap_asm
   ret_asm
end-code

\ 8.6.1.1140 ANS94 DOUBLE
: d>s  ( d -- n )
   drop ;

\ 8.6.1.1080 ANS94 DOUBLE
: d0=  ( xd -- flag )
   or 0 = ;

\ 8.6.1.1075 ANS94 DOUBLE
: d0<  ( d -- flag )
   nip 0< ;

\ 8.6.1.1210 ANS94 DOUBLE
: dmax  ( d1 d2 -- d3 )
   2over 2over d< if  2swap  then 2drop ;

\ 8.6.1.1220 ANS94 DOUBLE
: dmin  ( d1 d2 -- d3 )
   2over 2over d< 0= if  2swap then  2drop ;

\ 8.6.1.1820 ANS94 DOUBLE
: m*/  ( d1 n1 +n2 -- d2 )
   >r s>d >r abs -rot s>d r> xor r>
   swap >r >r  ( |n1| d1 ) ( R: sign[d1*n1] +n2 )
   dabs rot tuck um* 2swap um*  ( udh udl )
   swap >r 0 d+ r> -rot  ( u ud )
   r@ um/mod -rot r> um/mod -rot
   r> if
      if 1 0 d+ then
      dnegate
   else
      drop
   then ;

\ 8.6.1.1830 ANS94 DOUBLE
: m+  ( d1|ud1 n -- d2|ud2 )
   s>d d+ ;

\ 6.2.2440 ANS94 CORE EXT
: within ( n1|u1 n2|u2 n3|u3 -- flag )
   over - >r - r> u< ;

: @execute  ( i*x a-addr -- j*x )
   @ ?dup if execute then ;

host headless on target
: a-move  ( a-addr1 a-addr2 u -- )
   for aft
      [ ]ASM
      ucode0 R<-T T<-N PUSH<-R ucode,
      ucode0 N<-RAM T<-ALU_ADD2 ucode,
      r@!_asm
      ucode0 RAM<-N T<-ALU_ADD2 ucode,
      ucode0 N<-T T<-R R<-POP ucode,
      swap_asm
      ASM[ ]
   then next 2drop ;

: a-fill  ( a-addr u x -- )
   swap 1- >r
   swap
   [ ]ASM ucode0 RAM<-N T<-ALU_ADD2 unext_asm ASM[ ]
   rdrop 2drop ;
host headless off target

\ 17.6.1.0910 ANS94 STRING
: cmove  ( c-addr1 c-addr2 u -- )
   for aft
      >r dup c@ r@ c! 1+ r> 1+
   then next 2drop ;

\ 17.6.1.0920 ANS94 STRING
: cmove>  ( c-addr1 c-addr2 u -- )
   [ASM] dup>r_asm [END-ASM] 1-
   tuck + -rot + swap
   [ASM] for [END-ASM] aft
      >r dup c@ r@ c! 1- r> 1-
   then next 2drop ;

\ 6.1.1900 ANS94 CORE
: move  ( addr1 addr2 u -- )
   >r 2dup swap dup r@ +
   within if
      r> cmove>
   else
      r> cmove
   then ;

\ 6.1.1540 ANS94 CORE
: fill  ( c-addr u char -- )
   swap for swap aft
      2dup c! 1+
   then next 2drop ;

\ 6.1.0770 ANS94 CORE
: bl  ( -- char )
   $20 ;   inline

code carry  ( -- x )
   carry_asm
   ret_asm
end-code   inline

code sleep  ( -- x )
   sleep_asm
   ret_asm
end-code   inline

code irqdis  ( -- x )
   irqdis_asm
   ret_asm
end-code   inline

code irqen  ( -- )
   irqen_asm
   ret_asm
end-code   inline

code irqset  ( x -- )
   irqset_asm
   drop_asm
   ret_asm
end-code   inline

0 [IF]
   Local Variables:
   forth-local-words:
   (
   (("for" "aft" "then" "next") compile-only (font-lock-keyword-face . 2))
   )
   forth-local-indent-words:
   (
   ("for[ \\t]+\\(?:[^ \\t]+[ \\t]+\\)*?aft" (0 . 2) (0 . 2))
   (("for" "aft") (0 . 2) (0 . 2))
   ("then[ \\t]+\\(?:[^ \\t]+[ \\t]+\\)*?next" (-2 . 0) (0 . -2))
   (("then" "next") (-2 . 0) (0 . -2))
   )
   End:
[THEN]