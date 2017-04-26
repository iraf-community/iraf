	.data	0
	.set	LWM1,0xfc0
	.data	2
	.data	1
	.data	0
	.globl	_aadds_
	.data	2
v.2:
	.space	4
	.set	v.1,v.2

	.stabs	"aadds.f",0x64,0,0,0
	.text
	.globl	_aadds_
	.set	LF1,16
_aadds_:
	.word	LWM1
	subl2	$LF1,sp
	jbr	L12
	.align	1
L12:
	moval	v.1,r11
	movl	*16(ap),-4(fp)
	subl2	$2,4(ap)
	movl	*16(ap),-8(fp)
	subl2	$2,8(ap)
	movl	*16(ap),-12(fp)
	subl2	$2,12(ap)
	movl	*16(ap),-16(fp)
	movl	v.2-v.1(r11),r10
	movl	4(ap),r9
	movl	8(ap),r8
	movl	12(ap),r7
	movl	-16(fp),r6
	movl	$1,r10
	cmpl	r6,r10
	jlss	L21
L22:
	addw3	(r9)[r10],(r8)[r10],(r7)[r10]
	aobleq	r6,r10,L22
L21:
	ret
