	.ugen	
	.verstamp	3 11
	.extern	_iob 0
	.extern	optarg 8
	.extern	optind 4
	.extern	optopt 4
	.extern	opterr 4
	.extern	zfd 0
	.comm	jmpbuf 340
	.comm	status 4
	.data	
	.align	3
	.align	0
$$18:
	.ascii	"exit status %d\X0A\X00"
	.text	
	.align	4
	.file	2 "zzdebug.c"
	.globl	main
	.loc	2 20
 #   20	{
	.ent	main 2
main:
	.option	O1
	ldgp	$gp, 0($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)
	.mask	0x04000000, -16
	.frame	$sp, 16, $26, 0
	.prologue	1
	.loc	2 20

	.loc	2 21
 #   21		zsvjmp_((char *)jmpbuf, &status);
	lda	$16, jmpbuf
	lda	$17, status
	.livereg	0x0001E002,0x00000000
	jsr	$26, zsvjmp_
	ldgp	$gp, 0($26)
	.loc	2 22
 #   22		if (status) {
	ldl	$1, status
	beq	$1, $32
	.loc	2 22

	.loc	2 23
 #   23		    printf ("exit status %d\n", status);
	lda	$16, $$18
	bis	$1, $1, $17
	.livereg	0x0001C002,0x00000000
	jsr	$26, printf
	ldgp	$gp, 0($26)
	.loc	2 24
 #   24		    exit (status);
	ldl	$16, status
	.livereg	0x00018002,0x00000000
	jsr	$26, exit
	ldgp	$gp, 0($26)
$32:
	.loc	2 27
 #   25		}
 #   26	
 #   27		a(1);
	ldil	$16, 1
	.livereg	0x00018002,0x00000000
	jsr	$26, a
	ldgp	$gp, 0($26)
	.loc	2 28
 #   28		exit (0);
	bis	$31, $31, $16
	.livereg	0x00018002,0x00000000
	jsr	$26, exit
	ldgp	$gp, 0($26)
	.loc	2 29
 #   29	}
	bis	$31, $31, $0
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end	main
	.text	
	.align	4
	.file	2 "zzdebug.c"
	.globl	a
	.loc	2 34
 #   30	
 #   31	
 #   32	a(status)
 #   33	int	status;
 #   34	{
	.ent	a 2
a:
	.option	O1
	ldgp	$gp, 0($27)
	lda	$sp, -64($sp)
	stq	$26, 0($sp)
	stq	$16, 16($sp)
	.mask	0x04000000, -64
	.frame	$sp, 64, $26, 48
	.prologue	1
	.loc	2 34

	.loc	2 35
 #   35		ZDOJMP(jmpbuf, &status);
	lda	$16, jmpbuf
	lda	$17, 16($sp)
	.livereg	0x0001E002,0x00000000
	jsr	$26, zdojmp_
	ldgp	$gp, 0($26)
	.loc	2 36
 #   36	}
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 64($sp)
	ret	$31, ($26), 1
	.end	a
	.text	
	.align	4
	.file	2 "zzdebug.c"
	.globl	zdojmp_
	.loc	2 45
 #   45	{
	.ent	zdojmp_ 2
zdojmp_:
	.option	O1
	ldgp	$gp, 0($27)
	lda	$sp, -64($sp)
	stq	$26, 0($sp)
	stq	$16, 16($sp)
	stq	$17, 24($sp)
	.mask	0x04000000, -64
	.frame	$sp, 64, $26, 48
	.prologue	1
	.loc	2 45

	.loc	2 46
 #   46		*((int *)jmpbuf[0]) = *status;
	ldq	$1, 24($sp)
	ldl	$2, 0($1)
	ldq	$3, 16($sp)
	ldl	$4, 0($3)
	stl	$2, 0($4)
	.loc	2 47
 #   47		longjmp (&jmpbuf[1], *status);
	ldq	$16, 16($sp)
	addq	$16, 4, $16
	ldq	$5, 24($sp)
	ldl	$17, 0($5)
	.livereg	0x0001C002,0x00000000
	jsr	$26, longjmp
	ldgp	$gp, 0($26)
	.loc	2 48
 #   48	}
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 64($sp)
	ret	$31, ($26), 1
	.end	zdojmp_
