target

\ VIC read
\ BASE+0000[15:0]: irq_status[15:0]
\ BASE+0002[15:0]: irq_index[15:0]
\ BASE+0004[15:0]: irq_en[15:0]
\ BASE+0020[15:0]: irqaddr0[15:0]
\ BASE+0022[15:0]: irqaddr1[15:0]
\ BASE+002X[15:0]: irqaddrX[15:0]
\ BASE+003E[15:0]: irqaddrF[15:0]


\ VIC write
\ BASE+0002[15:0]: mask_clear
\ BASE+0004[15:0]: irq_en[15:0]
\ BASE+0006[15:0]: sw_irq_set[15:0]
\ BASE+0008[15:0]: sw_irq_clear[15:0]
\ BASE+0020[15:0]: irqaddr0[15:0]
\ BASE+0022[15:0]: irqaddr1[15:0]
\ BASE+002X[15:0]: irqaddrX[15:0]
\ BASE+003E[15:0]: irqaddrF[15:0]


: vic_status  ( -- x )
   VIC_BASE @ ;

: vic_index  ( -- x )
   [ VIC_BASE $02 host + target ] literal @ ;

: vic_en@  ( -- x )
   [ VIC_BASE $04 host + target ] literal @ ;

: vic_addr@  ( u -- x )
   cells
   [ VIC_BASE $20 host + target ] literal
   + @ ;

: vic_clear  ( -- )
   0 [ VIC_BASE $02 host + target ] literal ! ;

: vic_en!  ( x -- )
   [ VIC_BASE $04 host + target ] literal ! ;

: vic_swset  ( x -- )
   [ VIC_BASE $06 host + target ] literal ! ;

: vic_swclear  ( x -- )
   [ VIC_BASE $08 host + target ] literal ! ;

: vic_addr!  ( a-addr u -- )
   cells
   [ VIC_BASE $20 host + target ] literal
   + ! ;
