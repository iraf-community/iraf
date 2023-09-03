# AWSUR -- Weighted sum of two type real vectors.

	.data	0
	.globl	_awsur_
	.align	2
	.text

	.set	A,	4
	.set	B,	8
	.set	C,	12
	.set	NPIX,	16
	.set	W1,	20
	.set	W2,	24

	# AWSUR (a, b, c, npix, w1, w2)
	#
	# registers:
	#	r0	max_a
	#	r1	a
	#	r2	b
	#	r3	c
	#	r4	w1	(real)
	#	r5	w2	(real)

_awsur_:
	.word	0374
	movl	A(ap), r1
	movl	B(ap), r2
	movl	C(ap), r3
	mull3	$4, *NPIX(ap), r0
	addl2	r1, r0
	movf	*W1(ap), r4
	movf	*W2(ap), r5

	# c[i] = a[i] * w1 + b[i] * w2
L1:
	mulf3	(r1)+, r4, r6
	mulf3	(r2)+, r5, r7
	addf3	r6, r7, (r3)+

	cmpl	r1, r0
	blssu	L1

	ret
