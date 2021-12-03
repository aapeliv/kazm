	.text
	.file	"kazm"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	.Larg(%rip), %rdi
	callq	println@PLT
	popq	%rax
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Larg,@object           # @arg
	.section	.rodata.str1.1,"aMS",@progbits,1
.Larg:
	.asciz	"hello world\n"
	.size	.Larg, 13


	.section	".note.GNU-stack","",@progbits
