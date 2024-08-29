\ References:
\   The optional Cross-compiler word set
\     from http://www.mpeforth.com/arena/XCtext5.PDF

\ scope wordlists
VOCABULARY *scope
VOCABULARY *host
VOCABULARY *target
VOCABULARY *interpreter
VOCABULARY *compiler

ONLY FORTH ALSO *scope DEFINITIONS

\ x.6.1.nnnn XC CROSS
\ Select host scope
: host  ( -- )
   ONLY FORTH
   ALSO *scope
   ALSO *host DEFINITIONS ;

\ x.6.1.nnnn XC CROSS
\ Select target scope
: target  ( -- )
   ONLY *scope
   ALSO *target DEFINITIONS
   ALSO *interpreter ;

\ x.6.1.nnnn XC CROSS
\ Select compiler scope
: target-compiler  ( -- )
   ONLY *scope
   ALSO *target DEFINITIONS
   ALSO *compiler ;

\ x.6.1.nnnn XC CROSS
\ Select interpreter scope
: interpreter  ( -- )
   ONLY FORTH
   ALSO *scope
   ALSO *host
   ALSO *interpreter DEFINITIONS PREVIOUS ;
: compiler  ( -- )
   ONLY FORTH
   ALSO *scope
   ALSO *host
   ALSO *compiler DEFINITIONS PREVIOUS ;

: 'forth  ( i*x "<spaces>name" -- j*x )
   ALSO FORTH ' PREVIOUS
   EXECUTE ;

host
: tcells  ( n1 -- n2 )
   2 * ;
: tcell+  ( a-addr1 -- a-addr2 )
   1 tcells + ;
: tchars  ( n1 -- n2 ) ;
: tchar+  ( c-addr1 -- c-addr2 )
   1 tchars + ;
: taligned  ( addr -- a-addr )
   1 tcells 1- TUCK +
   SWAP INVERT AND ;
: @le  ( addr -- x )
   DUP 1 CELLS MOD
   DUP 1 tcells MOD ABORT" Unaligned address"
   TUCK - @ SWAP #8 * RSHIFT
   -1 1 tcells #8 * LSHIFT INVERT AND ;
: !le  ( x addr -- )
   DUP 1 CELLS MOD
   DUP 1 tcells MOD ABORT" Unaligned address"
   TUCK - ( x offset a-addr )
   ROT -1 1 tcells #8 * LSHIFT INVERT AND
   ROT TUCK ( a-addr offset x offset )
   #8 * LSHIFT -ROT ( x a-addr offset )
   OVER @ SWAP
   -1 1 tcells #8 * LSHIFT INVERT
   SWAP #8 * LSHIFT INVERT AND
   ROT OR SWAP ! ;
: C@le  ( addr -- x )
   DUP 1 CELLS MOD
   TUCK - @ SWAP #8 * RSHIFT
   -1 1 tchars #8 * LSHIFT INVERT AND ;
: C!le  ( x addr -- )
   DUP 1 CELLS MOD
   TUCK - ( x offset a-addr )
   ROT -1 1 tchars #8 * LSHIFT INVERT AND
   ROT TUCK ( a-addr offset x offset )
   #8 * LSHIFT -ROT ( x a-addr offset )
   OVER @ SWAP
   -1 1 tchars #8 * LSHIFT INVERT
   SWAP #8 * LSHIFT INVERT AND
   ROT OR SWAP ! ;

: icreate  ( "<spaces>name" -- )  \ create word in *interpreter wordlist
   GET-CURRENT ALSO *interpreter DEFINITIONS PREVIOUS
   CREATE  SET-CURRENT ;
: tcreate  ( "<spaces>name" -- )  \ create word in *target wordlist
   GET-CURRENT ALSO *target DEFINITIONS PREVIOUS
   CREATE  SET-CURRENT ;
: array  ( u "<spaces>name" -- )
   CREATE CELLS ALLOT
  DOES>  ( u -- a-addr )
   SWAP CELLS + ;

VARIABLE current-field
: save-field  ( -- n )
   current-field @ ;
: restore-field  ( n -- )
   current-field ! ;
: field-type!:  ( n "<spaces>name" -- )
   CREATE ,
  DOES>  ( -- )
   @ current-field ! ;

0 field-type!: cfield  \ code field
1 field-type!: nfield  \ name field
2 field-type!: dfield  \ data field

VARIABLE current-dfield-type
: dfield-type!:  ( n "<spaces>name" -- )
   CREATE ,
  DOES>  ( -- )
   @ current-dfield-type ! ;

\ section types
0 constant #cdata
1 constant #udata
2 constant #idata

#cdata dfield-type!: cdata-dfield
#udata dfield-type!: udata-dfield
#idata dfield-type!: idata-dfield

VARIABLE last-section   0 last-section !
5 ARRAY current-sections   0 current-sections 5 CELLS ERASE
: current-section  ( -- section )
   current-field @
   DUP 2 = IF
      current-dfield-type @ +
   THEN
   current-sections @
   DUP 0= ABORT" No section associated to current field" ;

\ x.6.1.nnnn XC CROSS
: section  ( taddr1 taddr2 type "<spaces>name" -- )  \ taddr1 and taddr2 inclusive
   icreate   HERE >R
   , last-section @ ,  \ store type and link  
   2DUP 2,             \ store start and end address
   OVER ,              \ store dp = start-addr
   SWAP -  CHAR+ HERE OVER CHARS ALLOT \ allocate memory for section
   SWAP ERASE          \ and initialize to 0 
   R> last-section !   \ make head of section list
  DOES>  ( -- )
   current-field @
   DUP 2 = IF
      current-dfield-type @ +
   THEN
   current-sections ! ;
: tdp  ( -- a-addr )
   current-section 4 cells + ;
: there  ( -- taddr )
   tdp @ ;
: tallot  ( n -- )
   tdp +! ;
: section-type  ( section -- type )
   @ ;
: section-trange  ( section -- taddr1 taddr2 )
   2 cells + 2@ ;
: section-start  ( -- x )
   current-section section-trange DROP ;
: section-end  ( -- x )
   current-section section-trange NIP ;
: tunused  ( -- n )
   section-end 1+ there - ;
: find-section  ( taddr -- section )
   last-section @ >R
   BEGIN  R@ WHILE
      DUP  R@ section-trange CHAR+ WITHIN IF
         DROP R> EXIT
      THEN
      R> cell+ @ >R  \ continue loop with next section
   REPEAT R> DROP  . ABORT" target address not within any target section" ;
: taddr>addr  ( taddr -- addr )
   DUP find-section
   DUP section-trange DROP ( taddr section taddr-start -- )
   SWAP 5 cells + ( taddr taddr-start addr-start )
   -ROT - + ;
: t@  ( a-taddr -- x )  taddr>addr @le ;
: t!  ( x a-taddr -- )  taddr>addr !le ;
: tc@  ( c-taddr -- x )  taddr>addr C@le ;
: tc!  ( x c-taddr -- )  taddr>addr C!le ;
: t,  ( x -- )  there t!  1 tcells tallot ;
: tc,  ( char -- )  there tc!  1 tchars tallot ;
: talign  ( -- )  there taligned tdp ! ;

VARIABLE headless  \ flag: add dictionary headers to target code?

\ setters for flag-variables a la 'headless'
: on  TRUE SWAP ! ;
: off  FALSE SWAP ! ;

headless on

VARIABLE last-dfa   0 last-dfa !  \ dfa of last created name in *target
VARIABLE last-:noname   0 last-:noname !  \ txt of last created :noname
VARIABLE last-tlfa   0 last-tlfa !  \ target last name field address
: tlink,  ( -- )
   talign there
   last-tlfa @ t,
   last-tlfa ! ;
: tname,  ( c-addr u -- )
   DUP 1 #32 WITHIN 0= ABORT" Invalid name length" 
   DUP talign tc,               \ write flags&length field
   BOUNDS DO  I C@ tc, LOOP     \ write name
   there 1 AND IF 0 tc, THEN ;  \ pad with 0 if name is not cell-aligned
: theader,  ( c-addr u -- )
   save-field >R nfield
   tlink, tname,
   R> restore-field ;
: ?theader  ( "<spaces>name" -- )
   BL WORD COUNT  headless @ IF  2DROP  ELSE  theader, then ;
: (tcreate)  ( "<spaces>name" -- )
   SAVE-INPUT ?theader RESTORE-INPUT ABORT" RESTORE-INPUT failed"
   tcreate HERE last-dfa !  \ save dfa of last created name in *target
   there ,  \ save tcfa/txt on df of last created name in *target
   0 ,      \ allocate space to save tcf size
   0 , ;    \ save name field flags
: #tcf!  ( -- )  \ save tcf size of last created name in *target
   there last-dfa @ @ - 1 tcells /
   last-dfa @ CELL+ ! ;
: #tcf@  ( -- u )  \ tcf size of last created name in *target
   last-dfa @ CELL+ @ ;
: tnfa>tlfa  ( a-taddr1 -- a-taddr2 )
   1 tcells - ;
: tlfa>tnfa  ( a-taddr1 -- a-taddr2 )
    tcell+ ;
: |tflags  ( x -- )  \ set flags by or-ing x
   $E0 AND >R
   last-dfa @ 2 CELLS +
   DUP @ ( flags ) R@ OR
   SWAP !
   headless @ 0= IF
      last-tlfa @ tlfa>tnfa
      DUP tc@ ( flags ) R@ OR
      SWAP tc!
   THEN
   R> DROP ;
: timmediate  ( -- )
   $80 |tflags ;
: tinline  ( -- )
   $40 |tflags ;

VARIABLE tstate   0 tstate !  \ tstate is initialized in interpret mode

S" ./asm.f" INCLUDED

: interpreter-comp  ( c-addr u -- ... xt )
   2DUP FIND-NAME DUP
   IF
      NIP NIP NAME>INT
   ELSE
      DROP
      2DUP 2>R SNUMBER?
      IF
         2RDROP ['] NOOP
         SWAP [ASM] ldi_asm [END-ASM]
      ELSE
         2R> INTERPRETER-NOTFOUND1
      THEN
   THEN ;

: tcompile,  ( a-addr -- )  \ dfa
   [ASM] ucode0 PC<-R R<-POP [END-ASM] >R
   DUP 2 CELLS + @ ( flags )
   OVER @ ( tcfa )
   OVER $40 AND IF  \ inline
      BEGIN
         DUP t@
         DUP R@ <>
      WHILE
            t, tcell+
      REPEAT
      ROT $80 AND IF  \ include ret instruction
         t,
      ELSE  \ skip ret instruction
         DROP
      THEN
      2DROP
   ELSE
      NIP SWAP CELL+ @  ( tcfa #instr )
      2DUP 1- tcells + t@
      R@ =
      OVER 2 < AND IF  \ 2 actually disables it
         1 ?DO
            DUP t@ t,
            tcell+
         LOOP
         DROP
      ELSE
         DROP [ASM] call_asm [END-ASM]  \ compile call to txt
      THEN
   THEN
   R> DROP ;

interpreter

: ]  ( -- )
   1 tstate !
   target-compiler
   ['] interpreter-comp IS parser1 ;

\ x.6.1.0890 XC CROSS
: cells  ( n1 -- n2 )   tcells ;

\ x.6.1.0880 XC CROSS
: cell+  ( a-taddr1 -- a-taddr2 )   tcell+ ;

\ x.6.1.0898 XC CROSS
: chars  ( n1 -- n2 )   tchars ;

\ x.6.1.0897 XC CROSS
: char+  ( c-taddr1 -- c-addr2 )   tchar+ ;

\ x.6.1.0706 XC CROSS
: aligned  ( taddr -- a-taddr )   taligned ;

\ x.6.1.0150 XC CROSS
: ,  ( x -- )
   current-section section-type
   #udata = ABORT" , cannot access udata section"
   t, ;

\ x.6.1.0860 XC CROSS
: c,  ( char -- )
   current-section section-type
   #udata = ABORT" c, cannot access udata section"
   tc, ;

\ x.6.1.0705 XC CROSS
: align  ( -- )   talign ;

\ x.6.1.1650 XC CROSS
: here  ( -- taddr )   there ;

\ x.6.1.0710 XC CROSS
: allot  ( n -- )   tallot ;

\ x.6.2.2395 XC CROSS EXT
: unused  ( -- u )   tunused ;

\ x.6.2.nnnn XC CROSS EXT
: org  ( taddr -- )   tdp ! ;

\ x.6.2.nnnn XC CROSS EXT
: equ  ( x "<spaces>name" -- )  \ create name in *interpreter/*compiler wordilists
   DUP >R SAVE-INPUT
   GET-CURRENT ALSO *interpreter DEFINITIONS PREVIOUS
   R> VALUE  SET-CURRENT
   RESTORE-INPUT ABORT" RESTORE-INPUT failed"
   GET-CURRENT ALSO *compiler DEFINITIONS PREVIOUS
   SWAP CREATE ,  SET-CURRENT
  DOES> @ [ASM] ldi_asm [END-ASM] ;

\ x.6.1.0650 XC CROSS
: @  ( a-taddr -- x )
   DUP find-section section-type
   #udata = ABORT" @ cannot access udata section"
   t@ ;

\ x.6.1.0010 XC CROSS
: !  ( x a-taddr -- )
   DUP find-section section-type
   #udata = ABORT" ! cannot access udata section"
   t! ;

\ x.6.1.0870 XC CROSS
: c@  ( c-taddr -- char )
   DUP find-section section-type
   #udata = ABORT" c@ cannot access udata section"
   tc@ ;

\ x.6.1.0850 XC CROSS
: c!  ( char c-taddr -- )
   DUP find-section section-type
   #udata = ABORT" c! cannot access udata section"
   tc! ;
: \  ['] \  EXECUTE ;
: (  ['] (  EXECUTE ;
: [IF]  ['] [IF]  EXECUTE ;
: [ELSE]  ['] [ELSE]  EXECUTE ;

: cfield  ( -- )   cfield ;
: dfield  ( -- )   dfield ;
: nfield  ( -- )   nfield ;

\ x.6.1.nnnn XC CROSS
\ Select CData as the current section type when dfield is selected
: cdata-dfield  ( -- )   cdata-dfield ;

\ x.6.1.nnnn XC CROSS
\ Select UData as the current section type when dfield is selected
: udata-dfield  ( -- )   udata-dfield ;

\ x.6.1.nnnn XC CROSS
\ Select IData as the current section type when dfield is selected
: idata-dfield  ( -- )   idata-dfield ;

: dump  ( taddr u -- )   SWAP taddr>addr SWAP DUMP ;

\ x.6.2.1350 XC CROSS EXT
: erase  ( taddr u -- )
   SWAP
   DUP find-section section-type
   #udata = ABORT" erase cannot access udata section"
   taddr>addr SWAP ERASE ;

\ x.6.2.0780 XC CROSS EXT
: blank  ( c-taddr u -- )
   SWAP
   DUP find-section section-type
   #udata = ABORT" blank cannot access udata section"
   taddr>addr SWAP BLANK ;

\ x.6.1.1540 XC CROSS
: fill  ( c-taddr u char -- )
   ROT
   DUP find-section section-type
   #udata = ABORT" fill cannot access udata section"
   taddr>addr -ROT FILL ;

\ x.6.1.1900 XC CROSS
: move  ( taddr1 taddr2 u -- )
   ROT
   DUP find-section section-type
   #udata = ABORT" move cannot access udata section"
   taddr>addr ROT
   DUP find-section section-type
   #udata = ABORT" move cannot access udata section"
   taddr>addr  ROT MOVE ;

\ x.6.2.0910 XC CROSS EXT
: cmove  ( c-taddr1 c-taddr2 u -- )
   ROT
   DUP find-section section-type
   #udata = ABORT" cmove cannot access udata section"
   taddr>addr ROT
   DUP find-section section-type
   #udata = ABORT" cmove cannot access udata section"
   taddr>addr  ROT CMOVE ;

\ x.6.2.0920 XC CROSS EXT
: cmove>  ( c-taddr1 c-taddr2 u -- )
   ROT
   DUP find-section section-type
   #udata = ABORT" cmove> cannot access udata section"
   taddr>addr ROT
   DUP find-section section-type
   #udata = ABORT" cmove> cannot access udata section"
   taddr>addr  ROT CMOVE> ;

\ data defining words
: constant  ( x "<spaces>name" -- )
   save-field >R
   cfield (tcreate)
   current-section section-type
   #cdata <> ABORT" No cdata section"
   DUP ,  \ save x on data field of last created name in *target
   [ASM] shorti? [END-ASM] IF
      $FFF AND $7000 OR t,
      [ASM] ret_asm [END-ASM]
      tinline
   ELSE
      $FFFF AND
      [ASM] ucode0 T<-IR N<-T PUSH<-N
      PC<-R R<-POP ucode, [END-ASM]
      t,
   THEN
   #tcf! \ save tcf size
   R> restore-field
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- x )
      3 CELLS + @  \ x on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;
: <builds  ( "<spaces>name" -- )
   cfield (tcreate)
   current-section section-type #cdata <> ABORT" No cdata section"
   0 last-dfa @ !  \ save tcfa/txt = 0
   dfield there ,  \ save tdfa
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- taddr )
      3 CELLS + @  \ tdfa on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.1.1250 XC CROSS
: does>  ( -- )
   cfield
   current-section section-type
   #cdata <> ABORT" No cdata section"
   last-dfa @ ?DUP IF
      @
      #tcf@ ?DUP 0= ABORT" tcf has zero size"
      1- tcells + DUP t@  ( a-taddr instr )
      [ASM] ucode0 PC<-R R<-POP [END-ASM] <> ABORT" Not ret instruction"
      DUP tcell+ there = IF
         -1 tcells tallot  \ no tdf in middle
      ELSE
         DUP there [ASM] rel? [END-ASM] 0= ABORT" No relative jump feasible"
         $4000 OR SWAP t!
      THEN
   ELSE
      there last-dfa @ !  \ save tcfa/txt
   THEN
   1 tstate !
   target-compiler
   ['] interpreter-comp IS parser1 ;

\ x.6.1.1000 XC CROSS
: create  ( "<spaces>name" -- )
   cfield (tcreate) current-section
   DUP section-type #cdata <> ABORT" No cdata section"
   dfield current-section <> IF
      there  \ data section is different from code section
   ELSE
      there 2 tcells +
      DUP [ASM] shorti? [END-ASM] NIP IF
      ELSE
         tcell+  \ long ldi_asm instruction
      THEN
   THEN
   DUP ,  \ save tdfa
   cfield
   [ASM] ldi_asm ret_asm [END-ASM]
   #tcf! \ save tcf size
   dfield
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- taddr )
      3 CELLS + @  \ tdfa on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.1.2410 XC CROSS
: variable  ( "<spaces>name" -- )
   cfield (tcreate) current-section
   DUP section-type #cdata <> ABORT" No cdata section"
   dfield current-section
   DUP section-type #idata =
   OVER section-type #udata =
   OR 0= ABORT" No idata/udata for variable"
   <> IF
      talign there  \ idata/udata section
   ELSE
      TRUE ABORT" cdata section for variable"
   THEN
   DUP ,  \ save tdfa
   cfield
   [ASM] ldi_asm ret_asm [END-ASM]
   #tcf! \ save tcf size
   dfield 1 tcells tallot
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- taddr )
      3 CELLS + @  \ tdfa on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.2.nnnn XC CROSS EXT
: cvariable  ( "<spaces>name" -- )
   cfield (tcreate) current-section
   DUP section-type #cdata <> ABORT" No cdata section"
   dfield current-section
   DUP section-type #idata =
   OVER section-type #udata =
   OR 0= ABORT" No idata/udata for cvariable"
   <> IF
      there  \ idata/udata section
   ELSE
      TRUE ABORT" cdata section for cvariable"
   THEN
   DUP ,  \ save tdfa
   cfield
   [ASM] ldi_asm ret_asm [END-ASM]
   #tcf! \ save tcf size
   dfield 1 tchars tallot
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- taddr )
      3 CELLS + @  \ tdfa on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.2.nnnn XC CROSS EXT
: buffer:  ( n "<spaces>name" -- )
   cfield (tcreate) current-section
   DUP section-type #cdata <> ABORT" No cdata section"
   dfield current-section
   DUP section-type #udata <> ABORT" No udata for buffer:"
   <> IF
      there  \ udata section
   ELSE
      TRUE ABORT" cdata section for buffer:"
   THEN
   DUP ,  \ save tdfa
   cfield
   [ASM] ldi_asm ret_asm [END-ASM]
   #tcf! \ save tcf size
   dfield tallot
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- taddr )
      3 CELLS + @  \ tdfa on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.2.2405 XC CROSS EXT
: value  ( x "<spaces>name" -- )
   cfield (tcreate) current-section
   DUP section-type #cdata <> ABORT" No cdata section"
   dfield current-section
   DUP section-type #idata <> ABORT" No idata for value"
   <> IF
      talign there  \ idata section
      SWAP t,
   ELSE
      TRUE ABORT" cdata section for value"
   THEN
   DUP ,  \ save tdfa
   cfield
   [ASM] ldi_asm @_asm ret_asm [END-ASM]
   #tcf! \ save tcf size
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE  ( -- x )
      3 CELLS + @ t@  \ x on stack
      FALSE ABORT" Attempt to execute target definition!"
   THEN ;

\ x.6.2.2295 XC CROSS EXT
: to  ( x "<spaces>name" -- )
   ALSO *target ' PREVIOUS >BODY 3 CELLS + @
   t! ;

\ x.6.1.0070 XC CROSS
: '  ( "<spaces>name" -- txt )
   ALSO *target ' PREVIOUS >BODY @ ;

: >body  ( txt -- tdfa )
   [ASM] @-ldi [END-ASM] ;

\ x.6.2.0945 XC CROSS EXT
: compile,  ( txt -- )
   ALSO *target CONTEXT @ PREVIOUS  \ *target wid
   CELL+
   BEGIN  ( txt nt )
      @ DUP
   WHILE
         OVER OVER
         NAME>INT >BODY @ = IF
            NAME>INT >BODY tcompile,
            DROP EXIT
         THEN
   REPEAT
   TRUE ABORT" txt word not found" ;

: code  ( "<spaces>name" -- )
   cfield (tcreate)
   current-section section-type
   #cdata <> ABORT" No cdata section"
   ]asm
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE
      TRUE ABORT" Attempt to execute target definition!"
   THEN ;

: end-code  ( -- )
   asm[
   #tcf@ 0= IF
      #tcf!  \ save tcf size
   THEN ;
: :  ( "<spaces>name" -- )
   cfield (tcreate)
   current-section section-type
   #cdata <> ABORT" No cdata section"
   1 tstate !
   target-compiler
   ['] interpreter-comp IS parser1
  DOES>
   tstate @ IF  ( -- )
      tcompile,
   ELSE
      TRUE ABORT" Attempt to execute target definition!"
   THEN ;
: :noname  ( -- txt )
   cfield
   current-section section-type
   #cdata <> ABORT" No cdata section"
   0 last-dfa !  there last-:noname !
   1 tstate !
   target-compiler
   ['] interpreter-comp IS parser1 ;
: :isr  ( -- txt )  \ defines Interrupt Service Routine
   cfield
   current-section section-type
   #cdata <> ABORT" No cdata section"
   0 last-dfa !  there last-:noname !
   1 tstate !
   target-compiler
   ['] interpreter-comp IS parser1 ;
: immediate  ( -- )
   timmediate ;
: inline  ( -- )
   tinline ;

compiler
: [  ( -- )
   0 tstate !
   target
   ['] interpreter1 IS parser1 ;
: ;  ( -- )
   [ASM] ret_asm [END-ASM]
   0 tstate !
   target
   ['] interpreter1 IS parser1
   last-dfa @ IF
      #tcf@ 0= IF
         #tcf!  \ save tcf size
      THEN
   ELSE
      last-:noname @
   THEN ;
: ;isr  ( -- )  \ ends Interrupt Service Routine
   [ASM] reti_asm [END-ASM]
   0 tstate !
   target
   ['] interpreter1 IS parser1
   last-:noname @ ;
: \  ['] \  EXECUTE ;
: (  ['] (  EXECUTE ;
: ahead  [ASM] ahead [END-ASM] ;
: if  [ASM] if [END-ASM] ;
: then  [ASM] then [END-ASM] ;
: else  [ASM] else [END-ASM] ;
: begin  [ASM] begin [END-ASM] ;
: again  [ASM] again [END-ASM] ;
: until  [ASM] until [END-ASM] ;
: while  [ASM] while [END-ASM] ;
: repeat  [ASM] repeat [END-ASM] ;
: for  [ASM] >r_asm for [END-ASM] ;
: next  [ASM] next rdrop_asm [END-ASM] ;
: aft  [ASM] aft [END-ASM] ;
: literal  ( x -- )   [ASM] ldi_asm [END-ASM] ;
: unext  ( x -- )   [ASM] >r_asm unext_asm rdrop_asm [END-ASM] ;

\ x.6.1.2033 XC CROSS
: postpone  ( "<spaces>name" -- )   ; \ TO BE IMPLEMENTED

\ x.6.1.2510 XC CROSS
: [']  ( "<spaces>name" -- )
   ALSO *target ' PREVIOUS >BODY @
   [ASM] ldi_asm [END-ASM] ;
: recurse  ( -- )
   last-dfa @ ?DUP IF @ ELSE last-:noname @ THEN
   [ASM] call_asm [END-ASM] ;  \ compile call to itself
: [char]  ( "<spaces>name" -- )
   CHAR [ASM] ldi_asm [END-ASM] ;

host
\ forward references
VARIABLE '(c")   0 '(c") !  \ txt of (c")
VARIABLE '(s")   0 '(s") !  \ txt of (s")
VARIABLE '(.")   0 '(.") !  \ txt of (.")
VARIABLE '(abort")   0 '(abort") !  \ txt of (abort")
CREATE forward-refs   #100 DUP , 0 ,  \ #refs, current ref
2* CELLS ALLOT  \ a-addr1 a-taddr1 a-addr2 a-taddr2 ...
: forward-ref-add  ( a-addr a-taddr -- )  \ ref, patch address
   forward-refs DUP CELL+ @ SWAP @ ( current max )
   OVER = ABORT" Error: forward-refs is full"
   1+ DUP forward-refs CELL+ !
   2* CELLS forward-refs +
   ROT OVER ! CELL+ ! ;
: refs-back-patching  ( -- )
   forward-refs CELL+ DUP @ SWAP CELL+
   OVER 0 ?DO  ( a-addr )
      DUP @ @
      DUP 0= IF
         ." Warning: forward ref not initialized"
      THEN
      SWAP CELL+ SWAP OVER @ t!
      CELL+
   LOOP
   DROP . ." forward refs patched" cr ;

: (")  ( a-taddr "ccc<quote>" -- )
   [ASM] callu_asm [END-ASM]  \ to avoid tail call
   [CHAR] " PARSE
   DUP tc, there taddr>addr SWAP  ( c-addr1 c-addr2 u )
   DUP tallot
   there #1 AND IF 0 tc, THEN  \ align with \0
   CMOVE ;

compiler
: c"  ( "ccc<quote>" -- )
   '(c") there tcell+ forward-ref-add 0 (") ;
: s"  ( "ccc<quote>" -- )
   '(s") there tcell+ forward-ref-add 0 (") ;
: ."  ( "ccc<quote>" -- )
   '(.") there tcell+ forward-ref-add 0 (") ;
: abort"  ( "ccc<quote>" -- )
   '(abort") there tcell+ forward-ref-add 0 (") ;

host
: write-addr  ( fileid taddr -- )
   1 tcells / 0 <# # # # # [CHAR] @ HOLD OVER >R #>
   R> ABORT" Address out of bound" ROT WRITE-LINE THROW ;
: write-data  ( fileid n -- )
   0 <# # # # # #> ROT WRITE-LINE THROW ;
: dump-section:  ( "filename" -- )
   BASE @ HEX
   BL WORD COUNT W/O CREATE-FILE THROW >R
   S" // UF16 image" R@ WRITE-LINE THROW
   section-end taddr>addr taligned section-start taddr>addr 0
   BEGIN  ( end-addr addr #zeros )
      OVER @le
      ?DUP IF
         SWAP ?DUP IF  \ #zeros > 0
            DUP $FFFF < IF
               R@ SWAP 0 DO
                  DUP 0 write-data
               LOOP
               DROP
            ELSE
               DROP OVER section-start taddr>addr -
               section-start +
               S" " R@ WRITE-LINE THROW
               R@ SWAP write-addr
            THEN
         THEN
         R@ SWAP write-data
         0  \ reset #zeros
      ELSE
         1+  \ increase #zeros
      THEN
      >R tcell+ OVER OVER = R> SWAP
   UNTIL
   DROP 2DROP R> CLOSE-FILE THROW
   BASE ! ;
: write-word  ( a-addr fileid -- )
   >R
   DUP NAME>STRING R@ WRITE-FILE THROW
   S"  0x" R@ WRITE-FILE THROW
   NAME>INT >BODY
   DUP @ 0 <# # # # # #> R@ WRITE-FILE THROW
   S"  " R@ WRITE-FILE THROW
   CELL+ DUP @ 0 <# #S #> R@ WRITE-FILE THROW
   S"  0x" R@ WRITE-FILE THROW
   CELL+ DUP @ 0 <# # # #> R@ WRITE-LINE THROW
   R> 2DROP ;
: write-words:  ( "filename" -- )
   BASE @ HEX
   BL WORD COUNT W/O CREATE-FILE THROW >R
   ALSO *target CONTEXT @ PREVIOUS  \ *target wid
   CELL+
   BEGIN  ( nt )
      @ DUP
   WHILE
         DUP R@ write-word
   REPEAT
   DROP R> CLOSE-FILE THROW
   BASE ! ;

host
$FFF8 CONSTANT 'RP0
$FFFA CONSTANT 'RP
$FFFC CONSTANT 'SP0
$FFFE CONSTANT 'SP
$1 CONSTANT DCACHE_DEPTH
$1 CONSTANT RCACHE_DEPTH

: fill_dcache  ( -- )  \ compile DUPs intructions to fill data cache
   DCACHE_DEPTH 0 ?DO
      [ASM] dup_asm [END-ASM]
   LOOP ;

: drop_dcache  ( -- )  \ compile DROPs intructions to empty data cache
   DCACHE_DEPTH 0 ?DO
      [ASM] drop_asm [END-ASM]
   LOOP ;

: nip_dcache  ( -- )  \ compile NIPs intructions to empty data cache
   DCACHE_DEPTH 0 ?DO
      [ASM] nip_asm [END-ASM]
   LOOP ;

: fill_rcache  ( -- )  \ compile RDUPs intructions to fill return cache
   RCACHE_DEPTH 0 ?DO
      [ASM] rdup_asm [END-ASM]
   LOOP ;

: drop_rcache  ( -- )  \ compile RDROPs intructions to empty return cache
   RCACHE_DEPTH 0 ?DO
      [ASM] rdrop_asm [END-ASM]
   LOOP ;

compiler
: fill_dcache  fill_dcache ;
: drop_dcache  drop_dcache ;
: nip_dcache  nip_dcache ;
: fill_rcache  fill_rcache ;
: drop_rcache  drop_rcache ;

interpreter
: fill_dcache  fill_dcache ;
: drop_dcache  drop_dcache ;
: nip_dcache  nip_dcache ;
: fill_rcache  fill_rcache ;
: drop_rcache  drop_rcache ;

'RP0 CONSTANT 'RP0
'RP CONSTANT 'RP
'SP0 CONSTANT 'SP0
'SP CONSTANT 'SP
$6 CONSTANT SADDR_WIDTH
#20000 CONSTANT /TICK  \ tick every 10ms @ 2MHz
#10 CONSTANT #MS  \ # ms between ticks

: sp0>  ( a-addr -- u )  \ convert data stack base address to register value
   $FFFF AND
   1 tcells - ;
: sp0!  ( u -- )  \ write both sp0 and sp for cold start
   [ASM] ucode0 T<-IR ucode, t,
   ucode0 T<-IR N<-T ucode,  'SP0 t,
   ucode0 RAM<-N ucode,
   ucode0 T<-IR ucode,  'SP t,
   ucode0 RAM<-N ucode, [END-ASM] ;
: xsp0!  ( u -- )  \ write both sp0 and sp for exception handler
   [ASM] ucode0 R<-T T<-IR ucode, t,
   ucode0 T<-IR N<-T ucode,  'SP0 t,
   ucode0 RAM<-N ucode,
   ucode0 T<-IR ucode,  'SP t,
   ucode0 RAM<-N ucode, [END-ASM] ;
: rp0>  ( a-addr -- u )  \ convert data stack base address to register value
   $FFFF AND
   1 tcells + ;
: rp0!  ( u -- )  \ write both rp0 and rp for cold start
   [ASM] ucode0 T<-IR ucode, t,
   ucode0 T<-IR N<-T ucode,  'RP0 t,
   ucode0 RAM<-N ucode,
   ucode0 T<-IR ucode,  'RP t,
   ucode0 RAM<-N ucode, [END-ASM] ;
: xrp0!  ( u -- )  \ write both rp0 and rp for exception handler
   [ASM] ucode0 T<-R R<-POP PUSH<-R ucode,
   ucode0 T<-IR N<-T ucode, t,
   'RP0 ldi_asm
   ucode0 RAM<-N ucode,
   ucode0 T<-IR ucode,  'RP t,
   ucode0 RAM<-N ucode, [END-ASM] ;

target
'RP0 equ 'RP0
'RP equ 'RP
'SP0 equ 'SP0
'SP equ 'SP
/TICK equ /TICK
#MS equ #MS
