	.text
	.file	"kazm"
	.globl	test_loops              # -- Begin function test_loops
	.p2align	4, 0x90
	.type	test_loops,@function
test_loops:                             # @test_loops
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	leaq	.Lglobalstring(%rip), %rdi
	callq	println@PLT
	leaq	.Lglobalstring.1(%rip), %rbx
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_2:                                # %loop
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %rdi
	callq	println@PLT
.LBB0_1:                                # %start
                                        # =>This Inner Loop Header: Depth=1
	callq	next_int@PLT
	cmpl	$2, %eax
	jle	.LBB0_2
# %bb.3:                                # %end
	leaq	.Lglobalstring.2(%rip), %rdi
	callq	println@PLT
	popq	%rbx
	retq
.Lfunc_end0:
	.size	test_loops, .Lfunc_end0-test_loops
	.cfi_endproc
                                        # -- End function
	.globl	test                    # -- Begin function test
	.p2align	4, 0x90
	.type	test,@function
test:                                   # @test
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	.Lglobalstring.3(%rip), %rdi
	callq	println@PLT
	movl	$4, %edi
	callq	int_println@PLT
	popq	%rax
	retq
.Lfunc_end1:
	.size	test, .Lfunc_end1-test
	.cfi_endproc
                                        # -- End function
	.globl	thing                   # -- Begin function thing
	.p2align	4, 0x90
	.type	thing,@function
thing:                                  # @thing
	.cfi_startproc
# %bb.0:                                # %entry
	movl	$5, %eax
	retq
.Lfunc_end2:
	.size	thing, .Lfunc_end2-thing
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI3_0:
	.quad	4614253070214989087     # double 3.1400000000000001
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %join
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	test_loops@PLT
	leaq	.Lglobalstring.4(%rip), %rdi
	callq	println@PLT
	leaq	.Lglobalstring.5(%rip), %rdi
	callq	println@PLT
	leaq	.Lglobalstring.7(%rip), %rdi
	callq	println@PLT
	leaq	.Lglobalstring.8(%rip), %rdi
	callq	print@PLT
	callq	thing@PLT
	movl	%eax, %edi
	callq	int_println@PLT
	leaq	.Lglobalstring.9(%rip), %rdi
	callq	print@PLT
	leaq	.Lglobalstring.10(%rip), %rdi
	callq	println@PLT
	movl	$27, %edi
	callq	int_println@PLT
	leaq	.Lglobalstring.11(%rip), %rdi
	callq	print@PLT
	movl	$6, %edi
	callq	int_println@PLT
	movl	$8, %edi
	callq	int_println@PLT
	movl	$20, 12(%rsp)
	movabsq	$4613937818241073152, %rax # imm = 0x4008000000000000
	movq	%rax, 16(%rsp)
	leaq	.Lglobalstring.12(%rip), %rdi
	callq	print@PLT
	movsd	.LCPI3_0(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	double_println@PLT
	callq	test@PLT
	leaq	.Lglobalstring.13(%rip), %rdi
	callq	println@PLT
	callq	next_int@PLT
	movl	%eax, %edi
	callq	int_println@PLT
	callq	next_int@PLT
	cmpl	$6, %eax
	jge	.LBB3_2
# %bb.1:                                # %take2
	leaq	.Lglobalstring.14(%rip), %rdi
	jmp	.LBB3_3
.LBB3_2:                                # %dont_take3
	leaq	.Lglobalstring.15(%rip), %rdi
.LBB3_3:                                # %join1
	callq	println@PLT
	callq	next_int@PLT
	movl	%eax, %edi
	callq	int_println@PLT
	callq	next_int@PLT
	cmpl	$6, %eax
	jge	.LBB3_5
# %bb.4:                                # %take6
	leaq	.Lglobalstring.16(%rip), %rdi
	jmp	.LBB3_6
.LBB3_5:                                # %dont_take7
	leaq	.Lglobalstring.17(%rip), %rdi
.LBB3_6:                                # %join5
	callq	println@PLT
	callq	next_int@PLT
	movl	%eax, %edi
	callq	int_println@PLT
	addq	$24, %rsp
	retq
.Lfunc_end3:
	.size	main, .Lfunc_end3-main
	.cfi_endproc
                                        # -- End function
	.type	.Lglobalstring,@object  # @globalstring
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lglobalstring:
	.asciz	"Before while"
	.size	.Lglobalstring, 13

	.type	.Lglobalstring.1,@object # @globalstring.1
.Lglobalstring.1:
	.asciz	"next_int() < 3"
	.size	.Lglobalstring.1, 15

	.type	.Lglobalstring.2,@object # @globalstring.2
.Lglobalstring.2:
	.asciz	"After while"
	.size	.Lglobalstring.2, 12

	.type	.Lglobalstring.3,@object # @globalstring.3
.Lglobalstring.3:
	.asciz	"In test"
	.size	.Lglobalstring.3, 8

	.type	.Lglobalstring.4,@object # @globalstring.4
.Lglobalstring.4:
	.asciz	"Start of fn"
	.size	.Lglobalstring.4, 12

	.type	.Lglobalstring.5,@object # @globalstring.5
.Lglobalstring.5:
	.asciz	"Took if"
	.size	.Lglobalstring.5, 8

	.type	.Lglobalstring.6,@object # @globalstring.6
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4
.Lglobalstring.6:
	.asciz	"Took second branch"
	.size	.Lglobalstring.6, 19

	.type	.Lglobalstring.7,@object # @globalstring.7
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lglobalstring.7:
	.asciz	"After if"
	.size	.Lglobalstring.7, 9

	.type	.Lglobalstring.8,@object # @globalstring.8
.Lglobalstring.8:
	.asciz	"thing() = "
	.size	.Lglobalstring.8, 11

	.type	.Lglobalstring.9,@object # @globalstring.9
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4
.Lglobalstring.9:
	.asciz	"without newline: "
	.size	.Lglobalstring.9, 18

	.type	.Lglobalstring.10,@object # @globalstring.10
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lglobalstring.10:
	.asciz	"! hello world"
	.size	.Lglobalstring.10, 14

	.type	.Lglobalstring.11,@object # @globalstring.11
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4
.Lglobalstring.11:
	.asciz	"Integer arithmetic: 6 should equal "
	.size	.Lglobalstring.11, 36

	.type	.Lglobalstring.12,@object # @globalstring.12
	.p2align	4
.Lglobalstring.12:
	.asciz	"Double 3.14 is: "
	.size	.Lglobalstring.12, 17

	.type	.Lglobalstring.13,@object # @globalstring.13
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lglobalstring.13:
	.asciz	"Next ints: "
	.size	.Lglobalstring.13, 12

	.type	.Lglobalstring.14,@object # @globalstring.14
.Lglobalstring.14:
	.asciz	"was less than 6"
	.size	.Lglobalstring.14, 16

	.type	.Lglobalstring.15,@object # @globalstring.15
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4
.Lglobalstring.15:
	.asciz	"was not less than 6"
	.size	.Lglobalstring.15, 20

	.type	.Lglobalstring.16,@object # @globalstring.16
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lglobalstring.16:
	.asciz	"was less than 6"
	.size	.Lglobalstring.16, 16

	.type	.Lglobalstring.17,@object # @globalstring.17
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4
.Lglobalstring.17:
	.asciz	"was not less than 6"
	.size	.Lglobalstring.17, 20


	.section	".note.GNU-stack","",@progbits
