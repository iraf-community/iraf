# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
# IEEER.S -- IEEE real to VAX single precision floating conversions.
#
#	 ieepakr (x)				# scalar, vax->ieee
#	 ieeupkr (x)				# scalar, ieee->vax
#	ieevpakr (native, ieee, nelem)		# vector, vax->ieee
#	ieevupkr (ieee, native, nelem)		# vector, ieee->vax
#	ieesnanr (NaN)				# set VAX NaN value
#	ieegnanr (NaN)				# get VAX NaN value
#	 ieemapr (mapin, mapout)		# enable NaN mapping
#	ieestatr (nin, nout)			# get num NaN values mapped
#      ieezstatr ()				# zero NaN counters
#
# These routines convert between the VAX and IEEE real floating formats,
# operating upon a single value or an array of values.  +/- zero is converted
# to zero.  When converting IEEE to VAX, underflow maps to zero, and exponent
# overflow and NaN input values map to the value set by IEESNANR (default 0).
# These routines are functionally equivalent to the semi-portable versions of
# the IRAF ieee/native floating conversion routines in osb$ieeer.x.
# TODO - Add a function callback option for processing NaN values.

	.data
vaxnan:	.long	0
nanin:	.long	0
nanout:	.long	0
mapin:	.long	1	# enable input NaN mapping by default for VAX
mapout:	.long	0

	.text
	.align	1
	.globl	_ieepar_
	.globl	_ieevpr_
	.globl	_ieeupr_
	.globl	_ieevur_
	.globl	_ieesnr_
	.globl	_ieegnr_
	.globl	_ieemar_
	.globl	_ieestr_
	.globl	_ieezsr_

_ieepar_:	# IEEPAKR (X)
	.word	0x0c
	movl	4(ap), r2			# data addr -> r2
	movl	r2, r3				# output clobbers input
	jsb	cvt_vax_ieee			# convert value
	ret
_ieevpr_:	# IEEVPAKR (VAX, IEEE, NELEM)
	.word	0x1c
	movl	4(ap), r2			# input vector -> r2
	movl	8(ap), r3			# output vector -> r3
	movl	*12(ap), r4			# loop counter
L1:	jsb	cvt_vax_ieee			# convert one value
	sobgtr	r4, L1				# loop
	ret
_ieeupr_:	# IEEUPKR (X)
	.word	0x0c
	movl	4(ap), r2			# data addr -> r2
	movl	r2, r3				# output clobbers input
	jsb	cvt_ieee_vax			# convert value
	ret
_ieevur_:	# IEEVUPKR (IEEE, VAX, NELEM)
	.word	0x1c
	movl	4(ap), r2			# input vector -> r2
	movl	8(ap), r3			# output vector -> r3
	movl	*12(ap), r4			# loop counter
L2:	jsb	cvt_ieee_vax			# convert one value
	sobgtr	r4, L2				# loop
	ret
_ieesnr_:	# IEESNANR (VAXNAN)
	.word	0x0
	movl	*4(ap), vaxnan
	clrl	nanin
	clrl	nanout
	ret
_ieegnr_:	# IEEGNANR (VAXNAN)
	.word	0x0
	movl	vaxnan, *4(ap)
	ret
_ieemar_:	# IEEMAPR (MAPIN, MAPOUT)
	.word	0x0
	movl	*4(ap), mapin
	movl	*8(ap), mapout
	ret
_ieestr_:	# IEESTATR (NIN, NOUT)
	.word	0x0
	movl	nanin, *4(ap)
	movl	nanout, *8(ap)
	ret
_ieezsr_:	# IEEZSTATR ()
	.word	0x0
	clrl	nanin
	clrl	nanout
	ret

cvt_vax_ieee:					# R2=in, R3=out
	movl	(r2)+, r0			# vax value -> r0

	tstl	mapout				# map NaNs on output?
	beql	L4				# no, just output value
	cmpl	r0, vaxnan			# yes, check if reserved value
	bneq	L4				# no, just output value
	clrl	r0				# generate IEEE NaN value
	insv	$255, $23, $8, r0		# insert NaN exponent (255)
	incl	nanout				# increment counter
	jbr	L5
L4:
	rotl	$16, r0, r0			# swap words -> r0
	extzv	$23, $8, r0, r1			# 8 bit exponent -> r1
	beql	L6				# branch if zero exponent 
	subw2	$2, r1				# adjust exponent bias
	bleq	L6				# return zero if underflow
	insv	r1, $23, $8, r0			# insert new exponent
L5:
	movl	sp, r1				# r3 points to input byte
	pushl	r0				# push r0 on stack
	movb	-(r1), (r3)+			# output longword, swapped
	movb	-(r1), (r3)+
	movb	-(r1), (r3)+
	movb	-(r1), (r3)+
	tstl	(sp)+				# pop stack
	rsb					# all done
L6:
	clrl	r0				# return all 32 bits zero
	jbr	L5

cvt_ieee_vax:					# R2=in, R3=out
	movb	(r2)+, -(sp)			# byte swap longword onto stack
	movb	(r2)+, -(sp)
	movb	(r2)+, -(sp)
	movb	(r2)+, -(sp)
	movl	(sp)+, r0			# pop swapped value -> r0
	extzv	$23, $8, r0, r1			# exponent -> r1
	beql	L10				# zero exponent
	tstl	mapin				# map NaNs on input?
	beql	L9				# no, don't check value
	cmpl	r1, $255			# NaN has exponent 255
	beql	L11				# yes, output vaxnan
L9:
	addw2	$2, r1				# adjust exponent bias
	cmpw	r1, $256			# compare with max VAX exponent
	bgeq	L11				# return VAX-NaN if overflow
	insv	r1, $23, $8, r0			# insert VAX-D exponent
	rotl	$16, r0, (r3)+			# output VAX value
	rsb
L10:
	clrl	(r3)+				# return all 32 bits zero
	rsb
L11:
	moval	vaxnan, r1			# return VAX equiv. of NaN
	movl	(r1)+, (r3)+
	incl	nanin
	rsb
