# 1 "zz.S"
# 1 "<built-in>" 1
# 1 "zz.S" 2
# 32 "zz.S"
 .file "zsvjmp.S"


 .arch armv8-a
# 44 "zz.S"
 .align 2


        .globl _zsvjmp_
 .text
_zsvjmp_:
# 80 "zz.S"
 str xzr, [x1]
 str x1, [x0], 8

 mov w1, 0

 b _sigsetjmp
