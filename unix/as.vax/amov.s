# AMOV -- Move a block of data from one area of memory to another.  The
# move is carried out (using the MOVC instruction) in such a way that
# data is not destroyed, regardless of whether or not the input an output
# arrays overlap.  Note that the move is not data dependent (floating
# point data is not special).

	.set	MASK,	07400
	.set	A,	4
	.set	B,	8
	.set	NPIX,	12
	.set	MAXBLK,	0177777

	.align	2
.text
	.globl	_amovc_				# amov_ (a, b, npix)
	.globl	_amovs_
	.globl	_amovi_
	.globl	_amovl_
	.globl	_amovr_
	.globl	_amovd_
	.globl	_amovx_
_amovc_:
_amovs_:
	.word	MASK
	movl	$2, r11				# r11 = size of pixel
	jbr	L10
_amovi_:
_amovl_:
_amovr_:
	.word	MASK
	movl	$4, r11
	jbr	L10
_amovd_:
_amovx_:
	.word	MASK
	movl	$8, r11

	# Compute source and destination addresses and the number of bytes to
	# be moved.  If nbytes=0 or the source and destinatation are the same
	# then we are done.  If nbytes is greater than a single MOVC3 can
	# accomodate then we must branch to the more complicated code below,
	# otherwise we call MOVC3 and return.

L10:	mull3	r11, *NPIX(ap), r10		# nbytes
	jleq	L20
	movl	A(ap), r8			# fwa of A array
	movl	B(ap), r9			# fwa of B array
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
