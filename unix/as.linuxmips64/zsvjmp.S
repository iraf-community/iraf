#include <sys/asm.h>
#include <sys/regdef.h>

/*
 * Copyright (c) 2018 James Cowgill <jcowgill AT debian TOD org>
 * Distributable under the same license as IRAF
 */

	.file	"zsvjmp.s"
	.text

LEAF(zsvjmp_)
	SETUP_GP			/* Load gp (needed for PTR_LA) */
	SETUP_GP64(v0, zsvjmp_)

	PTR_S a1, 0(a0)			/* buf[0] = status */
	LONG_S zero, 0(a1)		/* *status = 0 */
	PTR_ADDIU a0, a0, PTRSIZE	/* a0 = &buf[1], 1st arg for sigsetjmp */
	move a1, zero			/* a1 = 0, 2nd arg */

	PTR_LA t9, __sigsetjmp		/* t9 = address of sigsetjmp */
	RESTORE_GP64			/* Restore gp */
	jr t9				/* Tail call sigsetjmp */
END(zsvjmp_)

	.section .note.GNU-stack,"",@progbits
