	.verstamp	1 31
	.text	
	.align	2
	.file	2 "amovr.c"
	.globl	amovr_
	.loc	2 11
 #  11	{
	.ent	amovr_ 2
amovr_:
	.option	O1
	subu	$sp, 24
	sw	$31, 20($sp)
	sd	$4, 24($sp)
	sw	$6, 32($sp)
	.mask	0x80000000, -4
	.frame	$sp, 24, $31
	.loc	2 12
 #  12		bcopy ((char *)a, (char *)b, *n * sizeof(*a));
	lw	$4, 24($sp)
	lw	$5, 28($sp)
	lw	$14, 32($sp)
	lw	$6, 0($14)
	mul	$6, $6, 4
	jal	bcopy
	.loc	2 13
 #  13	}
	lw	$31, 20($sp)
	addu	$sp, 24
	j	$31
	.end	amovr_
