# ALUIR -- Lookup an interpolate a vector of type real onto a real grid.
# [OBSOLETE - NO LONGER USED.  5/27/87]

	.data	0
	.align	2
	.text
	.globl	_aluir_

	.set	A,	4
	.set	B,	8
	.set	X,	12
	.set	NPIX,	16

	# ALUIR (a, b, x, npix)
	#
	# left = int (x[i])
	# tau = x[i] - left
	# b[i] = (a[left] * (1-tau)) + (a[left+1] * tau)
	#
	# registers:
	#	r0	max_b
	#	r1	a
	#	r2	b
	#	r3	x
	#	r4	x[i], tau
	#	r5	left
	#	r6

_aluir_:
	.word	0374			# save r2-r7
	subl3	$4, A(ap), r1
	movl	B(ap), r2
	movl	X(ap), r3
	mull3	$4, *NPIX(ap), r0
	addl2	r2, r0
L1:
	movf	(r3)+, r4		# get X into r4
	cvtfl	r4, r5			# r5 = left
	cvtlf	r5, r6
	subf2	r6, r4			# r4 = tau = (x[i] - left)

	movf	(r1)[r5], r6
	mulf3	r4, r6, r7
	subf2	r7, r6			# r6 = (a[left] * (1-tau))

	incl	r5
	mulf3	r4, (r1)[r5], r7	# r7 = (a[left+1] * tau)

	addf3	r6, r7, (r2)+		# output result to B

	cmpl	r2, r0
	blssu	L1

	ret
