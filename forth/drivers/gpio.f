target

\ GPIO read
\ BASE+0000[15:0]: GPIO[15:0]
\ BASE+0002[15:0]: GPIO_output_enable[15:0]
\ BASE+0004[0]:    irq
\ BASE+0006[15:0]: irq_enable[15:0]

\ GPIO write
\ BASE+0000[15:0]: GPIO[15:0]
\ BASE+0002[15:0]: GPIO_output_enable[15:0]
\ BASE+0004[0]:    irq reset
\ BASE+0006[15:0]: irq_enable[15:0]

: gpio@  ( -- u )
   GPIO_BASE @ ;

: gpio!  ( u -- )
   GPIO_BASE ! ;

: gpioe!  ( u -- )
   [ GPIO_BASE $02 host + target ] literal ! ;
