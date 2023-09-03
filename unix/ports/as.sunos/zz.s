	.file	"zz.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl zsvjmp_
	.type	 zsvjmp_,@function
zsvjmp_:
	pushl %ebp
	movl %esp,%ebp
	movl 12(%ebp),%eax
	movl $0,(%eax)
	movl 8(%ebp),%eax
	movl 12(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,(%eax)
	movl 8(%ebp),%eax
	addl $4,%eax
	pushl %eax
	call setjmp
	addl $4,%esp
.L1:
	leave
	ret
.Lfe1:
	.size	 zsvjmp_,.Lfe1-zsvjmp_
	.ident	"GCC: (GNU) 2.7.2.3"
