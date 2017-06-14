	.file	"zsvjmp.s"

# Set the address of the MEM common to zero.
	.globl   mem_
	mem_ = 0

# Copyright (c) 2014 John Long <codeblue@inbox.lv>
# Distributable under the same license as IRAF
# This file contains the Linux s390x (64 bit) version of ZSVJMP for Debian.

	.text
	.global	zsvjmp_
	.type	zsvjmp_, %function

zsvjmp_:
        stg     %r3,0(0,%r2)         # save contents of r3 where r2 is pointing
	xc      0(8,%r3),0(%r3)      # clear doubleword where r3 is pointing
	xgr     %r3,%r3              # clear r3
	aghi    %r2,8                # r2 <- r2 + 8
	jg      __sigsetjmp@PLT      # load vcon resolved by linker

	.size	zsvjmp_, .-zsvjmp_
	.section	.note.GNU-stack,"",@progbits
