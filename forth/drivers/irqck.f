target

\ IRQCK read
\ BASE+0000[15:0]: counter[15:0]
\ BASE+0002[0]:    irq
\ BASE+0004[0]:    irq_en

\ IRQCK write
\ BASE+0000[15:0]: rnd_limit[15:0]
\ BASE+0002:       irq_clear
\ BASE+0004[0]:    irq_en

: +irqck  ( u -- )  \ enable irq
   if IRQCK1_BASE else IRQCK0_BASE then
   $04 +
   $1 swap ! ;

: -irqck  ( u -- )  \ disable irq
   if IRQCK1_BASE else IRQCK0_BASE then
   $04 +
   $0 swap ! ;

: irqck@  ( u1 -- u2 )  \ get counter
   if IRQCK1_BASE else IRQCK0_BASE then
   @ ;

: irqck!  ( u1 u2 -- )  \ set random limit
   if IRQCK1_BASE else IRQCK0_BASE then
   ! ;
