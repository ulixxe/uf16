
S" ./kernel/meta.f" INCLUDED

target

$0000 equ ROM_BASE  $2000 equ ROM_SIZE
$0000 equ RESET_VECTOR
$0002 equ EXCEPTION_VECTOR

$8000 equ RAM_BASE  $2000 equ RAM_SIZE
$8000 equ IRAM_BASE  $200 equ MAX_IRAM_SIZE
$8200 equ URAM_BASE
#100 equ TIB_SIZE  \ in bytes
RAM_BASE RAM_SIZE 'forth + TIB_SIZE 'forth - equ TIB_BASE
TIB_BASE #8 ( TLINK RSP DSP ) 'forth - equ RSTACK_BASE
#64 equ STACKS_SIZE  \ in cells
RSTACK_BASE STACKS_SIZE 'forth 1- 'forth 2* 'forth - equ DSTACK_BASE

$F800 equ UART_BASE
$F808 equ TIMER_BASE
$F810 equ GPIO_BASE
$F820 equ IRQCK0_BASE
$F828 equ IRQCK1_BASE
$F840 equ VIC_BASE

$4C4C equ INVALID_INSTR

host
headless off

\ define memory layout
hex
target ROM_BASE ROM_BASE ROM_SIZE 'forth + 'forth 1- interpreter #cdata section rom
target IRAM_BASE IRAM_BASE MAX_IRAM_SIZE 'forth + 'forth 1- interpreter #idata section iram
target URAM_BASE RAM_BASE RAM_SIZE 'forth + 'forth 1- interpreter #udata section uram

target
cfield rom  \ set rom section for cfield field
nfield rom  \ set rom section for nfield field
dfield cdata-dfield rom  \ set rom section for cdata-dfield field
dfield idata-dfield iram  \ set iram section for idata-dfield field
dfield udata-dfield uram  \ set uram section for udata-dfield field

cfield idata-dfield  \ set currents fields

ROM_BASE ROM_SIZE INVALID_INSTR fill  \ initialize rom with INVALID_INSTR


EXCEPTION_VECTOR #2 'forth + org
'forth S" ./kernel/forth_primitives.f" 'forth INCLUDED

variable hld      \ scratch
variable >in      \ 6.1.0560 ANS94 CORE \ input buffer offset
variable #tib     \ 6.2.0060 ANS94 CORE EXT \ #chars in the input buffer
variable 'tib     \ tib
variable base     \ 6.1.0750 ANS94 CORE \ number base
variable context  \ first search vocabulary
variable cp       \ dictionary code pointer
variable last     \ ptr to last name compiled
variable 'eval    \ interpret/compile vector
variable 'abort
variable state    \ 6.1.2250 ANS94 CORE
variable 'pause   \ task switcher vector
variable #tasks   \ number of waked tasks
variable #pause   \ number of paused tasks
variable tick#   0 tick# !

'forth S" ./drivers/uart.f" 'forth INCLUDED
'forth S" ./drivers/timer.f" 'forth INCLUDED
'forth S" ./drivers/gpio.f" 'forth INCLUDED
'forth S" ./drivers/irqck.f" 'forth INCLUDED
'forth S" ./drivers/vic.f" 'forth INCLUDED
'forth S" ./kernel/forth_interpreter.f" 'forth INCLUDED
'forth S" ./kernel/tasks.f" 'forth INCLUDED
'forth S" ./test/ttester.f" 'forth INCLUDED
\ 'forth S" ./app.f" 'forth INCLUDED

variable irqck0_counter   0 irqck0_counter !
variable irqck1_counter   0 irqck1_counter !

MAX_IRAM_SIZE dfield unused cfield 'forth - 'forth 1+ 'forth 2/ 'forth 2* equ IRAM_SIZE


\ exception handler
:noname  ( x -- ) ( R: a-addr 0x0004 -- )
   [ DSTACK_BASE sp0> xsp0!
   RSTACK_BASE rp0> xrp0! ] 2drop nip
   base @ swap r>  ( x a-addr -- )
   ." Exception at 0x" hex 0 <# # # # # #> type cr
   #2 base !
   0 <#
   #3 for aft [char] _ hold then #3 for # next next
#> type cr
base !
quit
;  \ I would like to remove this ret_asm
host dup ." Exception handler @ 0x" . CR target equ EXCEPTION_HDLR

\ interrupt handler for TIMER
:isr  ( -- )
   carry
   0tmr
   #1 tick# +!
   #1 [ TIMER_BASE #4 'forth + ] literal !  \ reset irq
   vic_clear
   [ASM] shr_asm [END-ASM] drop
;isr   host dup ." TIMER interrupt handler @ 0x" . CR target equ TIMER_ISR

\ interrupt handler for IRQCK0
:isr  ( -- )
   carry
   #1 irqck0_counter +!
   irqck0_counter @ IRQCK0_BASE @ <> abort" IRQCK0 missmatch!"  \ check counter
   0 [ IRQCK0_BASE #2 'forth + ] literal !  \ reset irq
   vic_clear
   [ASM] shr_asm [END-ASM] drop
;isr   host dup ." IRQCK0 interrupt handler @ 0x" . CR target equ IRQCK0_ISR

\ interrupt handler for IRQCK1
:isr  ( -- )
   carry
   #1 irqck1_counter +!
   irqck1_counter @ IRQCK1_BASE @ <> abort" IRQCK1 missmatch!"  \ check counter
   0 [ IRQCK1_BASE #2 'forth + ] literal !  \ reset irq
   vic_clear
   [ASM] shr_asm [END-ASM] drop
;isr   host dup ." IRQCK1 interrupt handler @ 0x" . CR target equ IRQCK1_ISR

: cold  ( i*x -- ) ( R: j*x -- )
   [ DSTACK_BASE sp0> sp0!
   RSTACK_BASE rp0> rp0! ] 2drop drop rdrop

   RAM_BASE
   [ RAM_SIZE 'forth 2/ ] literal
   INVALID_INSTR
   a-fill  \ initialize ram with INVALID_INSTR

   [ ROM_BASE ROM_SIZE 'forth + IRAM_SIZE 'forth - ] literal
   IRAM_BASE
   [ IRAM_SIZE 'forth 1+ 'forth 2/ ] literal
   a-move  \ initialize ram with rom data

   [ DSTACK_BASE sp0> ] literal
   [ RSTACK_BASE rp0> ] literal
   over !  \ DSB@ = RSB
   dup task>tlink !  \ TLINK@ = DSB
   1 #tasks !
   0 #pause !
   tsingle
   $1 UART_XONOFF !  \ UART enable XONOFF
   #10 base !
   TIMER_ISR 0 vic_addr!
\   GPIO_ISR 1 vic_addr!
   IRQCK0_ISR 2 vic_addr!
   IRQCK1_ISR 3 vic_addr!
   /TICK s>d tmr!  +tmr  +tmrirq
   %1101 vic_en!
   ." UF16 v0.3" cr cr
   quit [

cfield here
RESET_VECTOR org
' cold ]ASM call_asm ASM[  \ reset vector, return stack is reset by 'cold' (take care to avoid tail call optimization!)

EXCEPTION_VECTOR org
EXCEPTION_HDLR ]ASM call_asm ASM[  \ exception vector, return stack has to be dropped by exception handler (take care to avoid tail call optimization!)

org
URAM_BASE cp !
interpreter last-tlfa @
target last !
host refs-back-patching target

cfield

IRAM_BASE
ROM_BASE ROM_SIZE 'forth + IRAM_SIZE 'forth -
IRAM_SIZE
move  \ copy iram section to last part of rom

cfield
interpreter dump-section: ./output/rom.hex
target dfield idata-dfield
interpreter dump-section: ./output/iram.hex
host
write-words: ./output/words.txt

target
ROM_SIZE cfield unused
MAX_IRAM_SIZE dfield unused cfield

host decimal
2dup
." Free IRAM: " u. ." /" u. cr
- -
." Free ROM: " u. ." /" u. cr


0 [IF]
   Local Variables:
   forth-local-words:
   (
   (("for" "aft" "then" "next") compile-only (font-lock-keyword-face . 2))
   ((":isr") definition-starter (font-lock-keyword-face . 1))
   ((";isr") definition-ender (font-lock-keyword-face . 1))
   ("quit[ \\t]+\\[" definition-ender (font-lock-keyword-face . 1))
   )
   forth-local-indent-words:
   (
   ("for[ \\t]+\\(?:[^ \\t]+[ \\t]+\\)*?aft" (0 . 2) (0 . 2))
   (("for" "aft") (0 . 2) (0 . 2))
   ("then[ \\t]+\\(?:[^ \\t]+[ \\t]+\\)*?next" (-2 . 0) (0 . -2))
   (("then" "next") (-2 . 0) (0 . -2))
   ((":isr") (0 . 2) (0 . 2))
   ((";isr") (-2 . 0) (0 . -2))
   ("quit[ \\t]+\\[" (0 . -2) (0 . -2))
   )
   End:
[THEN]
