# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
# IEEED.S -- IEEE double to VAX double floating conversions.
#
#	 ieepakd (x)				# scalar, vax->ieee
#	 ieeupkd (x)				# scalar, ieee->vax
#	ieevpakd (native, ieee, nelem)		# vector, vax->ieee
#	ieevupkd (ieee, native, nelem)		# vector, ieee->vax
#	ieesnand (NaN)				# set VAX NaN value
#	ieegnand (NaN)				# get VAX NaN value
#        ieemapd (mapin, mapout)                # enable NaN mapping
#       ieestatd (nin, nout)                    # get num NaN values mapped
#      ieezstatd ()                             # zero NaN counters
#
# These routines convert between the VAX and IEEE double floating formats,
# operating upon a single value or an array of values.  +/- zero is converted
# to zero.  When converting IEEE to VAX, underflow maps to zero, and exponent
# overflow and NaN input values map to the value set by IEESNAND (default 0).
# These routines are functionally equivalent to the semi-portable versions of
# the IRAF ieee/native floating conversion routines in osb$ieeed.x.
# TODO - Add a function callback option for processing NaN values.

	.data
vaxnan:	.quad	0
nanin:  .long   0
nanout: .long   0
mapin:  .long   1       # enable input NaN mapping by default for VAX
mapout: .long   0

	.text
	.align	1
	.globl	_ieepad_
	.globl	_ieevpd_
	.globl	_ieeupd_
	.globl	_ieevud_
	.globl	_ieesnd_
	.globl	_ieegnd_
        .globl  _ieemad_
        .globl  _ieestd_
        .globl  _ieezsd_

_ieepad_:	# IEEPAKD (X)
	.word	0x3c
	movl	4(ap), r4			# data addr -> r4
	movl	r4, r5				# output clobbers input
	jsb	cvt_vax_ieee			# convert value
	ret
_ieevpd_:	# IEEVPAKD (VAX, IEEE, NELEM)
	.word	0x7c
	movl	4(ap), r4			# input vector -> r4
	movl	8(ap), r5			# output vector -> r5
	movl	*12(ap), r6			# loop counter
L1:	jsb	cvt_vax_ieee			# convert one value
	sobgtr	r6, L1				# loop
	ret
_ieeupd_:	# IEEUPKD (X)
	.word	0x3c
	movl	4(ap), r4			# data addr -> r4
	movl	r4, r5				# output clobbers input
	jsb	cvt_ieee_vax			# convert value
	ret
_ieevud_:	# IEEVUPKD (IEEE, VAX, NELEM)
	.word	0x7c
	movl	4(ap), r4			# input vector -> r4
	movl	8(ap), r5			# output vector -> r5
	movl	*12(ap), r6			# loop counter
L2:	jsb	cvt_ieee_vax			# convert one value
	sobgtr	r6, L2				# loop
	ret
_ieesnd_:	# IEESNAND (VAXNAN)
	.word	0x0
	movq	*4(ap), vaxnan
        clrl    nanin
        clrl    nanout
	ret
_ieegnd_:	# IEEGNAND (VAXNAN)
	.word	0x0
	movq	vaxnan, *4(ap)
	ret
_ieemad_:       # IEEMAPD (MAPIN, MAPOUT)
        .word   0x0
        movl    *4(ap), mapin
        movl    *8(ap), mapout
        ret
_ieestd_:       # IEESTATD (NIN, NOUT)
        .word   0x0
        movl    nanin, *4(ap)
        movl    nanout, *8(ap)
        ret
_ieezsd_:       # IEEZSTATD ()
        .word   0x0
        clrl    nanin
        clrl    nanout
        ret

cvt_vax_ieee:					# R4=in, R5=out
	movl	(r4)+, r1			# get vax double
	movl	(r4)+, r0			# get vax double

        tstl    mapout                          # map NaNs on output?
        beql    L4                              # no, just output value
        cmpl    r0, vaxnan                      # yes, check if reserved value
        bneq    L4                              # no, just output value
        cmpl    r1, vaxnan+4                    # yes, check if reserved value
        bneq    L4                              # no, just output value
        clrl    r0                              # generate IEEE NaN value
        clrl    r1                              # generate IEEE NaN value
        insv    $2047, $20, $11, r1             # insert NaN exponent (2047)
        incl    nanout                          # increment counter
        jbr     L5
L4:
	rotl	$16, r0, r0			# swap words -> r0
	rotl	$16, r1, r1			# swap words -> r1
	extzv	$23, $8, r1, r2			# 8 bit exponent -> r2
	beql	L6				# branch if zero exponent 
	extzv	$2, $1, r0, r3			# get round bit -> r3
	ashq	$-3, r0, r0			# shift 64 data bits by 3
	addw2	$(1024-130), r2			# adjust exponent bias
	insv	r2, $20, $11, r1		# insert new exponent
	blbc	r3, L5				# branch if round bit clear
	incl	r0				# round low longword
	adwc	$0, r1				# carry to high longword
L5:
	movl	sp, r3				# r3 points to input byte
	pushl	r1				# push r1 on stack
	pushl	r0				# push r0 on stack
	movb	-(r3), (r5)+			# output quadword, swapped
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	movb	-(r3), (r5)+
	addl2	$8, sp				# pop stack
	rsb					# all done
L6:
	clrl	r0				# return all 64 bits zero
	clrl	r1
	jbr	L5

cvt_ieee_vax:					# R4=in, R5=out
	movb	(r4)+, -(sp)			# byte swap quadword onto stack
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)
	movb	(r4)+, -(sp)

	movl	(sp)+, r0			# pop low bits
	movl	(sp)+, r1			# pop high bits
	extzv	$20, $11, r1, r2		# exponent -> r2
	beql	L10				# zero exponent
        tstl    mapin                           # map NaNs on input?
        beql    L9                              # no, don't check value
        cmpl    r2, $2047                       # NaN double has exponent 2047
        beql    L11                             # yes, output vaxnan
L9:
	extzv	$31, $1, r1, r3			# save sign bit
	ashq	$3, r0, r0			# shift 64 bits left 3 bits
	subw2	$(1024-130), r2			# adjust exponent bias
	bleq	L10				# return zero if underflow
	cmpw	r2, $256			# compare with max VAX exponent
	bgeq	L11				# return VAX-NaN if overflow
	insv	r2, $23, $8, r1			# insert VAX-D exponent
	insv	r3, $31, $1, r1			# restore sign bit

	rotl	$16, r1, (r5)+			# output VAX double
	rotl	$16, r0, (r5)+			# output VAX double
	rsb
L10:
	clrl	(r5)+				# return all 64 bits zero
	clrl	(r5)+
	rsb
L11:
	moval	vaxnan, r3			# return VAX equiv. of NaN
	movl	(r3)+, (r5)+
	movl	(r3)+, (r5)+
	incl	nanin
	rsb
