target

: task:  ( u "<spaces>name" -- )
   create
   dup 1+ cells here + ,  \ RSB
   #3 + cells allot ;

: /task  ( a-addr -- u )  \ task size in cells
   dup @ swap - 2/ #3 + ;

: task!  ( xt a-addr -- )
   dup /task 1-
   over cell+ swap
   INVALID_INSTR a-fill
   dup cell+
   0 over !  \ carry = 0
   swap @  ( DSB+1 RSB )
   tuck !  \ DSP@ = DSB+1
   dup 1 cells -  ( RSB RSB-1 )
   tuck swap cell+ !  \ RSP@ = RSB-1
   ! ;  \ RSB-1@ = xt

host headless on target
: task>tlink  ( a-addr -- a-addr )
   @ cell+ cell+ ;
host headless off target

: twake  ( a-addr -- )
   [ DSTACK_BASE sp0> ] literal  task>tlink  \ operator task TLINK
   2dup @  \ next task
   swap task>tlink !  \ new task TLINK points to next task
   !  \ current task TLINK points to new task
   #tasks @ 1+ #tasks ! ;

: tsleep  ( a-addr -- )
   [ DSTACK_BASE sp0> ] literal  \ operator task
   [ASM] dup>r_asm [END-ASM]
   begin  ( task taski ) ( R: task0 )
      task>tlink  \ current task TLINK
      dup @ r@ <> while
         2dup @ <> while
            @
      repeat
      2dup
      swap task>tlink @
      swap !
      #tasks @ 1- #tasks !
   then
   2drop rdrop ;

host headless on target
code (pause)  ( -- )
   carry_asm
   irqdis_asm  \ save and disable irq
   fill_dcache
   'RP0 ldi_asm @_asm
   'SP ldi_asm @_asm
   swap_asm
   ucode0 RAM<-N ucode,  \ save DSP
   fill_rcache
   ucode0 PUSH<-R T<-ALU_ADD2 ucode,  \ cell+
   ucode0 T<-IR N<-T ucode,  'RP ucode,
   @_asm swap_asm
   ucode0 RAM<-N T<-ALU_ADD2 ucode,  \ save RSP and go to TLINK
   @_asm  \ new DSP0
   ucode0 T<-IR N<-T ucode,  'SP0 ucode,
   ucode0 RAM<-N T<-N N<-T ucode,  \ update DSP0 and swap
   @_asm  \ new RSP0
   ucode0 T<-IR N<-T R<-T ucode,  'RP0 ucode,  \ save RSP0
   ucode0 RAM<-N T<-R ucode,  \ update RSP0 and re-get its value
   @_asm  \ new DSP
   ucode0 T<-IR N<-T ucode,  'SP ucode,
   ucode0 RAM<-N T<-R ucode,  \ update DSP and re-get RSP0 value
   add2_asm @_asm  \ new RSP
   ucode0 T<-IR N<-T ucode,  'RP ucode,
   ucode0 RAM<-N N<-POP ucode,  \ update RSP and drop
   rdrop_asm
   drop_asm
   irqset_asm drop_asm  \ restore irq
   shr_asm drop_asm  \ update carry
   ret_asm
end-code
host headless off target

: tstop  ( -- )
   'SP0 @ tsleep  pause ;

: tmulti  ( -- )
   ['] (pause) 'pause ! ;

: tsingle  ( -- )
   0 'pause ! ;  \ NOP

: .task  ( a-addr -- )
   dup @ #6 +  \ tlink included
   over - dump ;


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
