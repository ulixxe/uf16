target

$80 equ IMMEDIATE_FLAG
$40 equ INLINE_FLAG
$20 equ HIDDEN_FLAG

: pause  ( -- )
   #pause @ 1+
   dup #tasks @
   >= if
      sleep drop
      drop 0 #pause !
   else
      #pause !
      'pause @execute
   then ;

\ 10.6.2.1905 ANS94 FACILITY EXT
: ms  ( u -- )
   #MS /  ( #TICKS )
   tick# @
   begin
      pause
      tick# @ dup >r
      swap -  \ # tick passed
      -  \ # ticks to wait
      r>
      over 0> invert
   until
   2drop ;

\ 10.6.1.1755 ANS94 FACILITY
: key?  ( -- flag )
   ERXBFULL @ $0100 and 0<> ;

\ 6.1.1750 ANS94 CORE
: key  ( -- char )
   begin key? invert while pause repeat ERXBUF @ ;

\ 6.1.1320 ANS94 CORE
: emit  ( x -- )
   begin ETXBEMPTY @ 0= while pause repeat ETXBUF ! ;

\ 6.1.1650 ANS94 CORE
: here  ( -- addr )
   cp @ ;

\ 6.2.2000 ANS94 CORE EXT
: pad  ( -- c-addr )
   here #80 + ;

\ 6.2.2290 ANS94 CORE EXT
: tib  ( -- c-addr )
   'tib @ ;

\ 6.1.2216 ANS94 CORE
: source  ( -- c-addr u )
   tib  #tib @ ;

: digit  ( u -- char )
   #9 over < $7 and +  \ for hex
   [char] 0 + ;

\ 6.1.0490 ANS94 CORE
: <#  ( -- )
   pad hld ! ;

\ 6.1.1670 ANS94 CORE
: hold  ( char -- )
   hld @ 1- dup hld ! c! ;

\ 6.1.0030 ANS94 CORE
: #  ( ud1 -- ud2 )
   base @ ud/mod rot digit hold ;

\ 6.1.0050 ANS94 CORE
: #s  ( ud -- 0 0 )
   begin # 2dup or while repeat ;

\ 6.1.2210 ANS94 CORE
: sign  ( n -- )
   0< if [char] - hold then ;

\ 6.1.0040 ANS94 CORE
: #>  ( xd -- c-addr u )
   2drop hld @ pad over - ;

\ 6.1.2220 ANS94 CORE
: space  ( -- )
   bl emit ;

\ 6.1.2230 ANS94 CORE
: spaces  ( n -- )
   0 max
   for aft space then next ;

\ 6.1.0990 ANS94 CORE
: cr  ( -- )
   $0A $0D emit emit ;

: >char  ( char -- char )  \ convert to valid char
   dup $7F bl  within
   if drop [char] . then ;

\ 6.1.2310 ANS94 CORE
: type  ( c-addr u -- )
   for aft
      dup c@ >char emit 1+
   then next
   drop ;

\ 6.1.0980 ANS94 CORE
: count  ( c-addr1 -- c-addr2 u )
   dup 1+ swap c@ ;

\ 6.1.0706 ANS94 CORE
: aligned  ( addr -- a-addr )
   dup $1 and if 1+ then ;

host headless on target
: (abort")  ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
   r>
   count  ( c-addr u )
   over over + aligned
   >r
   rot if type cr 'abort @execute then
   2drop ;   ' (abort") host '(abort") ! target
host headless off target

: ud.r  ( ud n -- )
   >r <# #s #> r> over - spaces type ;

: ud.  ( ud -- )
   0 ud.r space ;

\ 6.1.2320 ANS94 CORE
: u.  ( u -- )
   0 ud. ;

\ 6.2.2330 ANS94 CORE EXT
: u.r  ( u n -- )
   0 swap ud.r ;

: str  ( n -- c-addr u )
   dup >r abs 0 <# #s r> sign #> ;

\ 8.6.1.1070 ANS94 DOUBLE
: d.r  ( d n -- )
   >r tuck dabs <# #s rot sign #>
   r> over - spaces type ;

\ 8.6.1.1060 ANS94 DOUBLE
: d.  ( d -- )
   0 d.r space ;

\ 6.1.0180 ANS94 CORE
: .  ( n -- )
   s>d d. ;

\ 6.2.0210 ANS94 CORE EXT
: .r  ( n1 n2 -- )
   >r s>d r> d.r ;

host headless on target
: >sp  ( u -- a-addr )
   dup [ $2 SADDR_WIDTH host LSHIFT target ] literal and
   over [ $4 SADDR_WIDTH host LSHIFT NEGATE INVERT $FFFF AND target ] literal
   'SP0 @ and =
   or if 'SP0 else 'RP0 then
   @ [ $4 SADDR_WIDTH host LSHIFT NEGATE $FFFF AND target ] literal and
   or ;
host headless off target

\ 6.1.1200 ANS94 CORE
: depth  ( -- +n )
   irqdis fill_dcache
   'SP0 @
   'SP @  rot irqset
   >sp ( sp0 sp )
   swap - 2/
   nip_dcache ;

\ 6.2.2030 ANS94 CORE EXT
: pick  ( xu ... x1 x0 u -- xu ... x1 x0 xu )
   cells >r
   irqdis fill_dcache
   r>  'SP @  rot irqset
   >sp ( u sp )
   swap - >r drop_dcache
   irqdis fill_dcache
   r> dup @
   nip swap irqset nip_dcache ;

\ 15.6.1.0220 ANS94 TOOLS
: .s  ( -- )
   depth
   [char] < emit
   dup str type
   [char] > emit space
   for aft
      r@ pick .
   then next ;

\ 6.2.1660 ANS94 CORE EXT
: hex  ( -- )
   #16 base ! ;

\ 6.1.1170 ANS94 CORE
: decimal  ( -- )
   #10 base ! ;

host headless on target
: c4.  ( u -- )
   0 <# # # # # #> type ;
host headless off target

\ 15.6.1.1280 ANS94 TOOLS
: dump  ( addr u -- )
   base @ >r hex
   over #1 and +  \ add 1 more byte if addr is not cell aligned
   aligned swap u2/ 2* swap  \ cell alignment
   begin
      over cr c4.
      [char] : emit space
      2dup u2/
      #7 for
         dup if
            over @ c4.
            1- swap cell+ swap
         else
            #4 spaces
         then
         space
      next
      2drop space
      #15 for
         dup if
            over c@ >char emit
            1- swap char+ swap
         else
            space
         then
      next
      dup while
   repeat
   cr 2drop
   r> base ! ;

\ 17.6.1.0245 ANS94 STRING
: /string  ( c-addr1 u1 n -- c-addr2 u2 )
   rot over +  -rot - ;

host headless on target
: digit?  ( char -- u true | false )
   [char] 0 -
   #9 over < if
      $7 - dup #10 < or
      #36 over < if  \ BASE 2 .. 36
         $20 - dup #10 < or
      then
   then
   dup base @ u<
   dup invert if nip then ;

: accumulate  ( ud1 u -- ud2 )
   \ ud2 = ud1*base + u
   swap base @ um* drop
   rot base @ um* d+ ;
host headless off target

\ 6.1.0570 ANS94 CORE
: >number  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
   for aft ( ud c-addr )
      dup c@ digit? if [ host -ROT target ]
         swap 1+ >r accumulate r>
      then next
      0
   else
      r> 1+
   then ;

host headless on target
: number?  ( c-addr u -- 0 | n 1 | d -1 )
   base @ >r
   ?dup if
      over c@
      dup [char] $ = if
         hex drop 1 /string
      else
         dup [char] # = if
            decimal drop 1 /string
         else
            dup [char] % = if
               #2 base ! drop 1 /string
            else
               drop
            then
         then
      then
      over c@ [char] - = >r
      r@ if 1 /string then
      ?dup if
         0 -rot 0 -rot
         >number
         ?dup if
            1 = swap c@ [char] . =
            and if
               r@ if dnegate then
               -1
            else
               2drop 0
            then
         else
            2drop
            r@ if negate then
            1
         then
      else
         drop 0
      then
      rdrop
   else
      drop 0
   then
   r> base ! ;
host headless off target

\ 6.2.2008 ANS94 CORE EXT
: parse  ( char "ccc<char>" -- c-addr u )
   tib >in @ + swap  ( c-addr char )
   #tib @ >in @ - for over aft  ( c-addr char c-addr1 )
      over over c@ = if
         nip 2dup swap -
         swap 1+ tib - >in !
         rdrop exit
      then
      1+
   then next
   2drop #tib @ >in !
   #tib @ over tib - - ;

host headless on target
: skip  ( char "<chars>" -- )
   tib >in @ +
   #tib @ >in @ - for aft  ( char c-addr )
      over over c@ <> if
         nip tib - >in !
         rdrop exit
      then
      1+
   then next
   2drop #tib @ >in ! ;

: token  ( char "<chars>ccc<char>" -- c-addr u )
   dup skip parse ;
host headless off target

\ 6.1.0895 ANS94 CORE
: char  ( "<spaces>name" -- char )
   bl token
   0= abort" char?" c@ ;

host headless on target
: pack  ( c-addr1 u c-addr2 -- c-addr3 )
   \ build counted string from u characters
   aligned  ( c-addr u a-addr )
   over $00FE and over + 0 swap !  \ 0 fill
   2dup c!
   dup >r 1+ swap cmove r> ;
host headless off target

\ 6.1.2450 ANS94 CORE
: word  ( char "<chars>ccc<char>" -- c-addr )
   token
   dup $FFE0 and abort" Word too long"
   here pack ;

host headless on target
: ^h  ( c-addr1 c-addr2 c-addr3 -- c-addr1 c-addr2 c-addr4 )
   >r over r> swap over
   xor if
      $8 emit
      1- bl emit
      $8 emit
   then ;

: tap  ( c-addr1 c-addr2 c-addr3 char -- c-addr1 c-addr2 c-addr4 )
   dup emit over c! 1+ ;
host headless off target

\ 6.1.0695 ANS94 CORE
: accept  ( c-addr u1 -- u2 )
   over + over
   begin  ( start end curr )
      key >r
      2dup xor if
         r@ bl - $5F u< if
            r@ tap
         else
            r@ $08 = if
               ^h
            then
         then
      else
         r@ $08 = if
            ^h
         then
      then
   r> $0D = until
   nip swap - ;

host headless on target
: (query)  ( -- )
   tib TIB_SIZE accept #tib !
   #0 >in ! ;
host headless off target

\ 6.2.2040 ANS94 CORE EXT
\ : query  ( -- )
\    TIB_BASE 'tib !
\    (query) ;

host headless on target
: (find)  ( c-addr a-addr -- c-addr 0 | xt nfa )
   \ c-addr is assumed cell aligned
   over c@ 2/ >r @  ( lfa )
   begin  ( c-addr a-addr ) ( R: u )
      over over dup if
         cell+ ( nfa )
         over @ over @ [ $FF1F HIDDEN_FLAG 'forth OR ] literal and xor
         [ASM] rdup_asm for [END-ASM]  ( c-addr1 c-addr2 x )
            dup if
               0 [ASM] r!_asm [END-ASM]
            else
               r@ if
                  drop cell+ swap cell+ swap
                  over @ over @ xor
               then
            then
         next
         nip
      then
      nip
   while @ repeat
   dup if
      nip cell+  ( nfa )
      dup r> cells + cell+  ( nfa xt )
      swap
   else
      rdrop
   then ;
host headless off target

\ 6.1.1550 ANS94 CORE
: find  ( c-addr -- c-addr 0 | xt 1 | xt -1 )
   \ c-addr is assumed cell aligned
   last (find)
   dup if
      @ $E0 and  ( flags )
      IMMEDIATE_FLAG = if 1 else -1 then
   then ;

\ 6.1.0070 ANS94 CORE
: '  ( "<spaces>name" -- xt )
   bl word
   last (find)
   if else drop 0 then ;

host headless on target
: (c")  ( -- c-addr )  \ counted string
   r>
   dup count  ( c-addr c-addr1 u )
   + aligned
   >r ;   ' (c") host '(c") ! target

: (s")  ( -- c-addr u )
   r>
   count  ( c-addr u )
   over over + aligned
   >r ;   ' (s") host '(s") ! target

: (.")  ( -- )
   r>
   count  ( c-addr u )
   over over + aligned
   >r
   type ;   ' (.") host '(.") ! target
host headless off target

\ 6.1.0710 ANS94 CORE
: allot  ( n -- )
   cp +! ;

\ 6.1.0150 ANS94 CORE
: ,  ( x -- )
   here !  1 cells allot ;

\ 6.1.0860 ANS94 CORE
: c,  ( char -- )
   here c!  1 allot ;

\ 6.1.0705 ANS94 CORE
: align  ( -- )
   here aligned cp ! ;

host headless on target
: link,  ( -- )
   align here
   last @ ,
   last ! ;

: header,  ( "<spaces>name" -- ) 
   link, bl word
   c@ 1+ allot
   here 1 and if 0 c, then ;  \ pad with 0 if name is not cell-aligned

: lfa>nfa  ( a-addr -- a-addr )
   cell+ ;   inline

: nfa>xt  ( a-addr -- xt )
   dup c@ $1F and
   + 1+ aligned ;

: xt>nfa  ( xt -- a-addr )  \ 0 if not found
   last begin
      @ dup while
         over over
         lfa>nfa nfa>xt <> while
      repeat
      lfa>nfa
   then
   nip ;

: |flags  ( x -- )
   $E0 and
   last @ lfa>nfa
   dup c@ ( flags ) rot or
   swap c! ;
host headless off target

\ 6.1.1710 ANS94 CORE
: immediate  ( -- )
   IMMEDIATE_FLAG |flags ;

: inline  ( -- )
   INLINE_FLAG |flags ;

: hidden  ( -- )
   HIDDEN_FLAG |flags ;

: 0hidden  ( -- )
   last @ lfa>nfa
   dup c@ ( flags )
   [ HIDDEN_FLAG 'forth INVERT $FFFF 'forth AND ] literal and
   swap c! ;

\ 15.6.1.2465 ANS94 TOOLS
: words  ( -- )
   last cr
   begin
      @ ?dup while
         dup lfa>nfa count $1F and
         type space
   repeat ;

host headless on target
: call,  ( xt -- )
   2/ $8000 or , ;
host headless off target

\ 6.2.0945 ANS94 CORE EXT
: compile,  ( xt -- )
   [ ]ASM ucode0 PC<-R R<-POP ASM[ ] literal >r
   dup xt>nfa c@  ( xt flags )
   2dup
   INLINE_FLAG and if  \ inline
      begin  ( a-addr )
         dup @
         dup r@ <>
      while
            , cell+
      repeat
      rot IMMEDIATE_FLAG and if  \ include ret instruction
         ,
      else  \ skip ret instruction
         drop
      then
      2drop
   else
      2drop call,
   then
   rdrop ;

host headless on target
: shorti?  ( n -- flag )
   $-800 $800 within ;
host headless off target

\ 6.1.0950 ANS94 CORE
: constant  ( x "<spaces>name" -- )
   header,
   dup shorti? if
      $FFF and $7000 or ,
      [ ]ASM ucode0 PC<-R R<-POP ASM[ ] literal , inline
   else
      [ ]ASM ucode0 T<-IR N<-T PUSH<-N
      PC<-R R<-POP ASM[ ] literal ,
      ,
   then ;

\ 6.1.1000 ANS94 CORE
: create  ( "<spaces>name" -- )
   header,
   [ ]ASM ucode0 T<-IR N<-T PUSH<-N
   PC<-R R<-POP ASM[ ] literal ,
   here cell+ , ;

\ 6.1.0550 ANS94 CORE
: >body  ( xt -- a-addr )
   cell+ cell+ ;

host headless on target
: (does>)  ( -- ) ( R: nest-sys1 -- )
   r>
   last @ lfa>nfa nfa>xt
   [ ]ASM ucode0 PC<-IR R<-PC PUSH<-R ASM[ ] literal
   over !
   cell+ ! ;
host headless off target

\ 6.1.1250 ANS94 CORE
: does>  ( -- ) ( R: nest-sys1 -- )
   [ ' (does>) ] literal compile,
   [ ' r> ] literal compile, ;  immediate

\ 6.1.2410 ANS94 CORE
: variable  ( "<spaces>name" -- )
   create 1 cells allot ;

\ 8.6.1.0440 ANS94 DOUBLE
: 2variable  ( "<spaces>name" -- )
   create 2 cells allot ;

host headless on target
variable to-message   \ 0 : FROM ,  1 : TO .
0 to-message !
host headless off target

\ 6.2.2295 ANS94 CORE EXT
: to  ( x “<spaces>name” -- )
   1 to-message ! ;

\ 6.2.2405 ANS94 CORE EXT
: value  ( x “<spaces>name” -- )
   create ,
  (does>) r>
   to-message @ if ! else @ then
   0 to-message ! ;

\ 6.1.2540 ANS94 CORE
: ]  ( -- )
   true state ! ;

\ 6.1.2500 ANS94 CORE
: [  ( -- )
   false state ! ;  immediate

\ 6.1.0450 ANS94 CORE
: :  ( "<spaces>name" -- )
   header, hidden ] ;

\ 6.1.0460 ANS94 CORE
: ;  ( -- )
   [ ]ASM ucode0 PC<-R R<-POP ASM[ ] literal ,
   [ ' [ ] [ASM] call_asm [END-ASM]
   0hidden ;  immediate

host headless on target
: literal,  ( x -- )
   dup shorti? if
      $FFF and $7000 or ,
   else
      [ ]ASM ucode0 T<-IR N<-T PUSH<-N ASM[ ] literal ,
      ,
   then ;
host headless off target

\ 8.6.1.0360 ANS94 DOUBLE
: 2constant  ( x1 x2 "<spaces>name" -- )
   header,
   swap literal,
   dup shorti? if
      $FFF and $7000 or ,
      [ ]ASM ucode0 PC<-R R<-POP ASM[ ] literal ,
   else
      [ ]ASM ucode0 T<-IR N<-T PUSH<-N
      PC<-R R<-POP ASM[ ] literal ,
      ,
   then ;

\ 6.2.1850 ANS94 CORE EXT
: marker  ( "<spaces>name" -- )
   header,
   last @
   dup @ literal,
   [ ' last ] literal compile,
   [ ' ! ] literal compile,
   literal,
   [ ' cp ] literal compile,
   [ ' ! ] literal compile,
   [ ]ASM ucode0 PC<-R R<-POP ASM[ ] literal , ;

\ 6.1.2510 ANS94 CORE
: [']  ( "<spaces>name" -- )
   ' literal, ;  immediate

\ 6.1.2520 ANS94 CORE
: [char]  ( "<spaces>name" -- )
   char literal, ;  immediate

\ 6.1.1780 ANS94 CORE
: literal  ( x -- )
   literal, ;  immediate

\ 8.6.1.0390 ANS94 DOUBLE
: 2literal  ( x1 x2 -- )
   swap literal, literal, ;  immediate

\ 6.1.2033 ANS94 CORE
: postpone  ( "<spaces>name" -- )
   '  dup xt>nfa c@ $E0 and  ( flags )
   IMMEDIATE_FLAG = if
      compile,
   else
      literal, [ ' compile, ] literal compile,
   then ;  immediate

host headless on target
: rel?  ( s-addr d-addr -- rel true | d-addr false )
   swap #11 rshift
   over #11 rshift -
   dup -1 #2 within if
      dup 0 = if
         $000
      else
         dup -1 = if
            $400
         else
            dup 1 = if
               $800
            then
         then
      then
      nip swap
      1 rshift $3FF and or true
   else
      drop false
   then ;

: branch  ( s-addr d-addr -- )
   rel? if
      $4000 or ,
   else
      [ ]ASM ucode0 PC<-IR ASM[ ] literal ,
      ,
   then ;

: 0branch  ( s-addr d-addr -- )
   rel? if
      $5000 or ,
   else
      [ ]ASM ucode0 PC<-?IR T<-N N<-POP ASM[ ] literal ,
      ,
   then ;

: branch-1  ( s-addr d-addr -- )
   rel? if
      $6000 or ,
   else
      [ ]ASM ucode0 PC<-?IR R<-M1 ASM[ ] literal ,
      ,
   then ;
host headless off target

\ 15.6.2.0702 ANS94 TOOLS EXT
: ahead  ( -- s-addr )
   here $4000 , ;  immediate

\ 6.1.1700 ANS94 CORE
: if  ( -- s-addr )
   here $5000 , ;  immediate

\ 6.1.2270 ANS94 CORE
: then  ( s-addr -- )
   dup here rel? invert abort" No possible relative jump"
   over @ or swap ! ;  immediate

\ 6.1.1310 ANS94 CORE
: else  ( s-addr -- s-addr )
   [ ' ahead compile, ] swap [ ' then compile, ] ;  immediate

\ 6.1.0760 ANS94 CORE
: begin  ( -- d-addr )
   here ;  immediate

\ 6.2.0700 ANS94 CORE EXT
: again  ( d-addr -- )
   here swap branch ;  immediate

\ 6.1.2390 ANS94 CORE
: until  ( d-addr -- )
   here swap 0branch ;  immediate

\ 6.1.2430 ANS94 CORE
: while  ( d-addr -- s-addr d-addr )
   [ ' if compile, ] swap ;  immediate

\ 6.1.2140 ANS94 CORE
: repeat  ( s-addr d-addr -- )
   [ ' again compile, ' then compile, ] ;  immediate

: for  ( -- d-addr )
   [ ]ASM ucode0 R<-T PUSH<-R T<-N N<-POP ASM[ ] literal ,
   here ;  immediate

: next  ( d-addr -- )
   here swap branch-1
   [ ]ASM ucode0 R<-POP ASM[ ] literal , ;  immediate

: aft  ( d-addr -- d-addr s-addr )
   drop [ ' ahead compile, ' begin compile, ] swap ;  immediate

host headless on target
: (do)  ( n1|u1 n2|u2 -- ) ( R: -- n3|u3 n4|u4 )
   swap 1- [ASM] dup>r_asm rswap_asm [END-ASM]
   swap - >r [ASM] rswap_asm [END-ASM] ;

: (+loop)  ( n -- )
   [ASM] rswap_asm [END-ASM] r@
   swap - r@ over xor
   $8000 and
   swap
   [ASM] r!_asm rswap_asm [END-ASM] ;
host headless off target

\ 6.1.1240 ANS94 CORE
: do  ( -- d-addr )
   ['] (do) call,
   here ; immediate

\ 6.1.1800 ANS94 CORE
: loop  ( d-addr -- )
   here swap branch-1
   [ ]ASM ucode0 R<-POP ASM[ ] literal ,
   [ ]ASM ucode0 R<-POP ASM[ ] literal , ;  immediate

\ 6.1.0140 ANS94 CORE
: +loop  ( d-addr -- )
   ['] (+loop) call,
   here swap 0branch
   [ ]ASM ucode0 R<-POP ASM[ ] literal ,
   [ ]ASM ucode0 R<-POP ASM[ ] literal , ;  immediate

\ 6.1.1680 ANS94 CORE
: i  ( -- n|u )
   r>
   [ASM] rswap_asm [END-ASM] r@
   [ASM] rswap_asm [END-ASM] r@
   - swap >r ;

\ 6.1.1730 ANS94 CORE
: j  ( -- n|u )
   r> r> r>
   [ASM] rswap_asm [END-ASM] r@
   [ASM] rswap_asm [END-ASM] r@
   - swap >r swap >r swap >r ;

\ 6.1.2380 ANS94 CORE
: unloop  ( -- )
   rdrop rdrop ;   inline

host headless on target
: (")  ( xt "ccc<quote>" -- )
   [ ]ASM ucode0 PC<-IR R<-PC PUSH<-R ASM[ ] literal , , \ to avoid tail call
   [char] " parse
   dup c, here swap  ( c-addr1 c-addr2 u )
   dup allot
   here #1 and if 0 c, then  \ align with \0
   cmove ;
host headless off target

\ 6.2.0855 ANS94 CORE EXT
: c"  ( "ccc<quote>" -- )
   [ ' (c") ] literal (") ;  immediate

\ 6.1.2165 ANS94 CORE
: s"  ( "ccc<quote>" -- )
   [ ' (s") ] literal (") ;  immediate

\ 6.1.0190 ANS94 CORE
: ."  ( "ccc<quote>" -- )
   [ ' (.") ] literal (") ;  immediate

\ 6.1.0680 ANS94 CORE
: abort"  ( "ccc<quote>" -- )
   [ ' (abort") ] literal (") ;  immediate

\ 6.1.2120 ANS94 CORE
: recurse  ( -- )
   last @ lfa>nfa nfa>xt
   call, ;  immediate

\ 6.2.2535 ANS94 CORE EXT
: \  ( "ccc<eol>" -- )
   begin
      bl token nip 0=
   until ;  immediate

\ 6.1.0080 ANS94 CORE
: (  ( "ccc<paren>" -- )
   1 begin
      #tib @ >in @ = if
         cr (query)
      else
         tib >in @ + c@
         dup [char] ( = if
            drop 1+
         else
            [char] ) = if 1- then
         then
         1 >in +!
      then
   ?dup 0= until ;  immediate

host headless on target
: eval  ( -- )
   begin
      bl token ?dup
   while
         pad pack find ?dup if
            state @ if
               0> if execute else compile, then
            else
               drop execute
            then
         else
            count number?
            dup 0= abort" Undefined word"
            state @ if
               1- if swap literal, then
               literal,
            else
               drop
            then
         then
   repeat
   drop ;
host headless off target

\ 6.1.1360 ANS94 CORE
: evaluate  ( i*x c-addr u -- j*x )
   'tib @ >r
   #tib @ >r
   >in @ >r
   #tib ! 'tib !
   #0 >in !
   eval
   r> >in !
   r> #tib !
   r> 'tib ! ;

\ 6.1.2050 ANS94 CORE
: quit  ( -- ) ( R: i*x -- )
   irqdis
   'RP0 @  ( rp0 )
   [ASM] dup>r_asm [END-ASM] fill_rcache
   'RP [ASM] !_asm drop_asm [END-ASM]  \ rp = rp0
   irqset rdrop
   TIB_BASE 'tib !
   0 state !
   begin
      (query) space eval ."  ok" cr
   again ;

\ 6.1.0670 ANS94 CORE
: abort  ( i*x -- ) ( R: j*x -- )
   irqdis fill_dcache
   'SP0 @  ( sp0 )
   'SP [ ]ASM ucode0 RAM<-N ucode, ASM[ ] 2drop  \ sp = sp0
   irqset quit ;   ' abort 'abort !

\ 6.2.2395 ANS94 CORE EXT
: unused  ( -- u )
   [ DSTACK_BASE 1 cells 'forth - ] literal
   here - ;

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
