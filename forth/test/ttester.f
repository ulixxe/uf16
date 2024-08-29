\ From: John Hayes S1I
\ Subject: tester.fr
\ Date: Mon, 27 Nov 95 13:10:09 PST  

\ (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\ MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.
\ VERSION 1.2

target

variable verbose
   false verbose !
\   true verbose !

: empty-stack  ( ... -- )
   depth for aft drop then next ;

variable #errors 0 #errors !

: error  ( c-addr u -- )  \ display an error message
   cr type source type cr  \ display line corresponding to error
   #errors @ 1+ #errors !
   abort ;

variable actual-depth  \ stack record
create actual-results 20 cells allot

: T{  ( -- )  \ syntactic sugar
;

: ->  ( ... -- )  \ record depth and content of stack
   depth dup actual-depth !
   actual-results swap
   for aft
      swap over !
      cell+
   then next
   drop ;

: }T  ( ... -- )  \ compare stack (expected) contents with saved (actual) contents
   depth actual-depth @ = if
      depth actual-results swap
      for aft
         swap over @
         = 0= if s" INCORRECT RESULT: " error then
         cell+
      then next
      drop
   else  \ depth mismatch
      s" WRONG NUMBER OF RESULTS: " error
   then ;

: testing  ( -- )  \ talking comment
   source
   verbose @ if
      dup >r type cr r> >in !
   else
      >in ! drop [char] * emit
   then ;
