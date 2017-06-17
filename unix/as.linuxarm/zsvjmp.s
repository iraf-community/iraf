	.file	"zsvjmp.s"

@ Copyright (c) 2014 Ole Streicher <debian@liska.ath.cx>
@ Distributable under the same license as IRAF
@ This file contains the Linux armel/armhf version of ZSVJMP for Debian.

	.arch armv6
	.text
	.global	zsvjmp_
	.type	zsvjmp_, %function

zsvjmp_:
	mov	r2, #0         @
	str	r2, [r1, #0]   @ *status = 0
	str	r1, [r0, #0]   @ buf[0] = status
	add	r0, r0, #4     @ &buf[1] --> 1st arg for sigsetjmp
	mov	r1, #0         @ 0       --> 2nd arg for sigsetjmp
	b	__sigsetjmp    @ call sigsetjmp
