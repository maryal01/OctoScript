	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movl	$0, 48(%rsp)
	movl	$100, 40(%rsp)
	movl	$108, 32(%rsp)
	movl	$114, 24(%rsp)
	movl	$111, 16(%rsp)
	movl	$119, 8(%rsp)
	movl	$32, (%rsp)
	leaq	L_fmt(%rip), %rdi
	movl	$104, %esi
	movl	$101, %edx
	movl	$108, %ecx
	movl	$108, %r8d
	movl	$111, %r9d
	xorl	%eax, %eax
	callq	_printf
	addq	$56, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"hello world\n"

.subsections_via_symbols
