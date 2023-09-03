	.file	"amods.o"
/#
/# AMODS -- Assembler version of the VOPS routine to work around a compiler
/* bug.
/#
	.data
	.data
	.bss
	.data
	.align 4
	.bss
	.align	4
VAR_SEG1:	.set	.,.+4
	.text
	.globl	amods_
	.set	LF1,12
amods_:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$12,%esp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	20(%ebp),%eax
	movl	(%eax),%ebx
	leal	-1(%ebx),%eax
	movl	%eax,-8(%ebp)
	xorl	%eax,%eax
	cmpl	%eax,-8(%ebp)
	jl	.LE1
	movl	12(%ebp),%eax
	leal	-2(%eax),%eax
	movl	%eax,%esi
	movl	8(%ebp),%eax
	leal	-2(%eax),%eax
	movl	%eax,-12(%ebp)
	movl	16(%ebp),%eax
	leal	-2(%eax),%eax
	movl	%eax,%edi
	movl	$2,%ebx
	addl	%ebx,%esi
	addl	%ebx,%edi
	addl	-12(%ebp),%ebx
.L77003:
	movw	(%ebx),%ax
	cwtd	
	idivw	(%esi)			/# buggy code in original
	movw	%ax,(%edi)		/#
	movl	-8(%ebp),%eax
	addl	$2,%ebx
	addl	$2,%edi
	addl	$2,%esi
	decl	%eax
	movl	%eax,-8(%ebp)
	testl	%eax,%eax
	jge	.L77003
.LE1:
/ASMQ
	movl	-24(%ebp),%ebx
	movl	-20(%ebp),%esi
	movl	-16(%ebp),%edi
/ASMEND0
	leave	
	ret	
/FUNCEND

	.data
	.text
