	.ugen	
	.verstamp	3 11

	# The following is needed when the shared library is used, to set
	# mem=0 in the main program (zsvjmp is in the shared library).
	.globl  mem_
	mem_    =       0

	.globl	sh_debug
	.data	
	.align	3
	.align	0
sh_debug:
	.long	0 : 1
	.globl	ushlib_
	.data	
	.align	3
	.align	0
ushlib_:
	.long	0 : 1
	.long	0 : 1
	.long	0 : 1
	.globl	vshlib_
	.data	
	.align	3
	.align	0
vshlib_:
	.long	0 : 1
	.long	0 : 1
	.long	0 : 1
	.comm	vshend_ 4
	.text	
	.align	4
	.file	2 "zshlib.c"
	.globl	vlibinit_
	.loc	2 18
 #   18	void VLIBINIT(){}
	.ent	vlibinit_ 2
vlibinit_:
	# .option	O1
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 18

	.loc	2 18

	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	vlibinit_
