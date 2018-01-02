	.file	"zsvjmp.s"

	// Copyright (c) 2018 Peter Green
	// Distributable under the same license as IRAF
	// This file contains the Linux arm64 version of ZSVJMP for Debian.

	.arch armv8-a
	.text
	.global	zsvjmp_
	.type	zsvjmp_, %function

zsvjmp_:
        str     xzr, [x1]	// *status = 0;
        str     x1, [x0], 8	// ((long **)buf)[0] = status
	// also post-increment x0 by 8: 1st arg for sigsetjmp
        mov     w1, 0		// 0 --> 2nd arg for sigsetjmp
        b      __sigsetjmp	// call sigsetjmp
