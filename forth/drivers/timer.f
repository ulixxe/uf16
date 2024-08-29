target

\ TIMER read
\ BASE+0000[15:0]: counter[15:0]
\ BASE+0002[15:0]: counter[31:16]
\ BASE+0004[1,0]:  overflow, irq
\ BASE+0006[1,0]:  counter enable, irq enable

\ TIMER write
\ BASE+0000[15:0]:  max_counter[15:0], counter and overflow reset
\ BASE+0002[15:0]:  max_counter[31:16], counter and overflow reset
\ BASE+0004[2,1,0]: counter reset, overflow reset, irq reset
\ BASE+0006[1,0]:   counter enable, irq enable

: 0tmr  ( -- )  \ reset timer
   [ TIMER_BASE $04 host + target ] literal dup
   @ $4 or swap ! ;

: +tmr  ( -- )  \ enable timer
   [ TIMER_BASE $06 host + target ] literal dup
   @ $2 or swap ! ;

: +tmrirq  ( -- )  \ enable timer irq
   [ TIMER_BASE $06 host + target ] literal dup
   @ $1 or swap ! ;

: -tmr  ( -- )  \ disable timer
   [ TIMER_BASE $06 host + target ] literal dup
   @ $FFFD and swap ! ;

: +0tmr  ( -- )  \ start timer
   +tmr 0tmr ;

: tmr@  ( -- ud )  \ get timer
   TIMER_BASE @
   [ TIMER_BASE $02 host + target ] literal @ ;

: tmr!  ( ud -- )  \ set max counter
   [ TIMER_BASE $02 host + target ] literal !
   TIMER_BASE ! ;
