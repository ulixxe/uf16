target

\ UART read
\ BASE+0000[15:0]: baudrate
\ BASE+0002[8]:    tx ready
\ BASE+0004[8,0]:  rx full, XONOFF
\ BASE+0006[7:0]:  rx

\ UART write
\ BASE+0000[15:0]: baudrate
\ BASE+0002[7:0]:  tx
\ BASE+0004[0]:    enable XONOFF

UART_BASE $02 'forth + equ ETXBUF
UART_BASE $02 'forth + equ ETXBEMPTY
UART_BASE $04 'forth + equ ERXBFULL
UART_BASE $06 'forth + equ ERXBUF

UART_BASE $04 'forth + equ UART_XONOFF

