	.file	"zzz.c"
gcc2_compiled.:
___gnu_compiled_c:
.text
	.align 2
.globl _exit_
	.type	 _exit_,@function
_exit_:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	movl (%eax),%edx
	pushl %edx
	call _exit
	addl $4,%esp
	.align 2,0x90
L1:
	leave
	ret
Lfe1:
	.size	 _exit_,Lfe1-_exit_
