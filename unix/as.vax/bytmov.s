# BYTMOV -- Move a block of data from one area of memory to another.  The
# move is carried out (using the MOVC instruction) in such a way that
# data is not destroyed, regardless of whether or not the input and output
# arrays overlap.

	.set	MASK,	07400

	# bytmov (a, aoff, b, boff, nbytes)
	.set	A,	4
	.set	AOFF,	8
	.set	B,	12
	.set	BOFF,	16
	.set	NBYTES,	20
	.set	MAXBLK,	0177777

	.align	2
.text
	.globl	_bytmov_
_bytmov_:
	.word	MASK

	# Compute source and destination addresses and the number of bytes to
	# be moved.  If nbytes=0 or the source and destinatation are the same
	# then we are done.  If nbytes is greater than a single MOVC3 can
	# accomodate then we must branch to the more complicated code below,
	# otherwise we call MOVC3 and return.

	movl	*NBYTES(ap), r10		# nbytes
	jleq	L20
	addl3	A(ap), *AOFF(ap), r8		# fwa of A array
	decl	r8				# allow for one-indexing
	addl3	B(ap), *BOFF(ap), r9		# fwa of B array
	decl	r9				# allow for one-indexing
	cmpl	r8, r9
	jeql	L20				# A, B same array
	cmpl	r10, $MAXBLK			# too large for single movc3?
	jgtr	L30
	movc3	r10, (r8), (r9)
L20:
	ret
L30:
	# Since the array is larger than a single MOVC3 instruction can
	# accomodate we must do the move in segments of size MAXBLK.  Since
	# multiple moves are needed we cannot leave it up to MOVC3 to make
	# sure that the move is nondestructive.  If the destination is to
	# the left (lower address) of the source then the move is necessarily
	# nondestructive.  If to the right then the move is potentially
	# nondestructive, and we must solve the problem by moving the high
	# segments first.

	movl	$MAXBLK, r11
	cmpl	r8, r9
	jlssu	L50
L40:						# move high to low
	cmpl	r10, $MAXBLK
	jgtr	L41
	movl	r10, r11
L41:
	movc3	r11, (r8), (r9)
	addl2	r11, r8
	addl2	r11, r9
	subl2	r11, r10
	jgtr	L40

	ret
L50:						# move low to high
	addl2	r10, r8	
	addl2	r10, r9
L60:
	cmpl	r10, $MAXBLK
	jgtr	L61
	movl	r10, r11
L61:
	subl2	r11, r8
	subl2	r11, r9
	movc3	r11, (r8), (r9)
	subl2	r11, r10
	jgtr	L60

	ret
