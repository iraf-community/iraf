	.toc
	.extern setjmp[DS]
	.extern .setjmp
.csect [PR]
	.align 2
	.globl zsvjmp
	.globl .zsvjmp
	.csect zsvjmp[DS]
zsvjmp:
	.long .zsvjmp, TOC[tc0], 0
	.csect [PR]
.zsvjmp:
	st 4, 0(3)
	cal 0, 0(0)
	st 0, 0(4)
	ai 3, 3, 4
	b .setjmp
	cror 15,15,15
LT..zsvjmp:
	.long 0
	.byte 0,0,32,97,128,1,2,1
	.long 0
	.long LT..zsvjmp-.zsvjmp
	.short 6
	.byte "zsvjmp"
	.byte 31
_section_.text:
.csect .data[RW]
	.long _section_.text
