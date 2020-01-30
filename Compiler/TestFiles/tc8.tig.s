
.globl main
.data
L76: .string "-"
L61: .string "9"
L60: .string "0"
L53: .string "\n"
L52: .string " "

.text
main:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -20
L105:
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -20(s0)
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s6, s6
	mv s7, s7
	mv s8, s8
	mv s9, s9
	mv s10, s10
	mv s11, s11
	addi s1, s0, -4
	mv s1, s1
	jal getchar
	mv a0, a0
	sw a0, 0(s1)
	sw s0, -4(sp)
	jal readlist
	mv a0, a0
	addi s1, s0, -4
	mv s1, s1
	sw s1, -16(s0)
	jal getchar
	mv s1, a0
	lw s2, -16(s0)
	mv s2, s2
	sw s1, 0(s2)
	sw s0, -4(sp)
	jal readlist
	mv a1, a0
	mv s0, s0
	sw s0, -4(sp)
	mv a0, a0
	mv a1, a1
	jal merge
	mv a0, a0
	sw s0, -4(sp)
	mv a0, a0
	jal printlist
	li a0, 0
	jal exit
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s8, s8
	mv s7, s7
	mv s6, s6
	mv s1, s3
	mv s1, s4
	mv s1, s5
	lw s1, -20(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L104
L104:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
readlist:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -20
L107:
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s1, s1
	sw s1, -20(s0)
	mv s5, s1
	mv s4, s1
	mv s6, s6
	mv s7, s7
	mv s8, s8
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li a0, 4
	jal allocRecord
	mv a0, a0
	li s1, 0
	sw s1, 0(a0)
	mv a0, a0
	lw s1, 4(s0)
	sw s1, -4(sp)
	mv a0, a0
	jal readInt
	mv s1, a0
	sw s1, -4(s0)
	li s1, 0
	li s2, 0
	li s3, 4
	mul s2, s2, s3
	add s2, a0, s2
	lw s2, 0(s2)
	beq s1, s2, L99
	bne s1, s2, L98
L98:
	li a0, 8
	jal allocRecord
	mv a0, a0
	lw s1, -4(s0)
	mv s1, s1
	sw s1, 0(a0)
	addi s2, a0, 4
	mv s2, s2
	lw s1, 4(s0)
	sw s1, -4(sp)
	jal readlist
	mv s1, a0
	sw s1, 0(s2)
	mv a0, a0
L100:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s8, s8
	mv s7, s7
	mv s6, s6
	mv s1, s4
	mv s1, s5
	lw s1, -20(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L106
L99:
	li a0, 0
	j L100
L106:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
merge:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -28
L109:
	mv a0, a0
	sw a0, -4(s0)
	mv a1, a1
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s1, s1
	sw s1, -20(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	beq s2, s1, L95
	bne s2, s1, L96
L96:
	li s1, 0
	beq a1, s1, L92
	bne a1, s1, L93
L93:
	li s1, 0
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw s1, 0(s1)
	li s2, 0
	li s3, 4
	mul s2, s2, s3
	add s2, a1, s2
	lw s2, 0(s2)
	blt s1, s2, L89
	bge s1, s2, L90
L90:
	li a0, 8
	jal allocRecord
	mv s3, a0
	li s1, 0
	li s2, 4
	mul s1, s1, s2
	add s1, a1, s1
	lw s1, 0(s1)
	sw s1, 0(s3)
	addi s1, s3, 4
	mv s1, s1
	sw s1, -28(s0)
	lw s1, 4(s0)
	sw s1, -4(sp)
	lw a0, -4(s0)
	mv a0, a0
	mv a0, a0
	li s1, 1
	li s2, 4
	mul s1, s1, s2
	add s1, a1, s1
	lw a1, 0(s1)
	mv a1, a1
	jal merge
	mv a0, a0
	lw s1, -28(s0)
	mv s1, s1
	sw a0, 0(s1)
	mv s3, s3
L91:
	mv s3, s3
L94:
	mv a0, s3
L97:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -20(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L108
L95:
	mv a0, a1
	j L97
L92:
	lw s3, -4(s0)
	mv s3, s3
	mv s3, s3
	j L94
L89:
	li a0, 8
	jal allocRecord
	mv s3, a0
	li s1, 0
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw s1, 0(s1)
	sw s1, 0(s3)
	addi s1, s3, 4
	mv s1, s1
	sw s1, -24(s0)
	lw s1, 4(s0)
	sw s1, -4(sp)
	li s1, 1
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw a0, 0(s1)
	mv a0, a0
	mv a1, a1
	jal merge
	mv a0, a0
	lw s1, -24(s0)
	mv s1, s1
	sw a0, 0(s1)
	mv s3, s3
	j L91
L108:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
printint:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -16
L111:
	mv a0, a0
	sw a0, -4(s0)
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	blt s2, s1, L80
	bge s2, s1, L81
L81:
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	bgt s2, s1, L77
	ble s2, s1, L78
L78:
	la a0, L60
	mv a0, a0
	jal print
	mv a0, a0
L79:
	mv a0, a0
L82:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L110
L80:
	la a0, L76
	mv a0, a0
	jal print
	sw s0, -4(sp)
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	sub a0, s1, s2
	mv a0, a0
	jal f
	mv a0, a0
	j L82
L77:
	sw s0, -4(sp)
	lw a0, -4(s0)
	mv a0, a0
	mv a0, a0
	jal f
	mv a0, a0
	j L79
L110:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
f:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -16
L113:
	mv a0, a0
	sw a0, -4(s0)
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	bgt s2, s1, L73
	ble s2, s1, L74
L74:
	li a0, 0
L75:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L112
L73:
	lw s1, 4(s0)
	sw s1, -4(sp)
	li s1, 10
	lw s2, -4(s0)
	mv s2, s2
	div a0, s2, s1
	mv a0, a0
	jal f
	li s1, 10
	lw s2, -4(s0)
	mv s2, s2
	div s1, s2, s1
	li s2, 10
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	sub s1, s2, s1
	mv s1, s1
	la a0, L60
	mv a0, a0
	jal ord
	mv a0, a0
	add a0, s1, a0
	mv a0, a0
	jal chr
	mv a0, a0
	mv a0, a0
	jal print
	mv a0, a0
	j L75
L112:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
printlist:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -16
L115:
	mv a0, a0
	sw a0, -4(s0)
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li s1, 0
	lw s2, -4(s0)
	mv s2, s2
	beq s2, s1, L70
	bne s2, s1, L71
L71:
	lw s1, 4(s0)
	sw s1, -4(sp)
	li s1, 0
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw a0, 0(s1)
	mv a0, a0
	jal printint
	la a0, L52
	mv a0, a0
	jal print
	lw s1, 4(s0)
	sw s1, -4(sp)
	li s1, 1
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw a0, 0(s1)
	mv a0, a0
	jal printlist
	mv a0, a0
L72:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L114
L70:
	la a0, L53
	mv a0, a0
	jal print
	mv a0, a0
	j L72
L114:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
readInt:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -20
L117:
	mv a0, a0
	sw a0, -4(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s1, s1
	sw s1, -16(s0)
	mv s1, s1
	sw s1, -20(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	li s1, 0
	sw s1, -8(s0)
	sw s0, -4(sp)
	jal skipto
L68:
	sw s0, -4(sp)
	lw s1, 4(s0)
	lw a0, -4(s1)
	mv a0, a0
	jal isdigit
	mv a0, a0
	li s1, 0
	beq a0, s1, L67
	bne a0, s1, L69
L69:
	li s1, 10
	lw s2, -8(s0)
	mv s2, s2
	mul s2, s2, s1
	mv s2, s2
	lw s1, 4(s0)
	lw a0, -4(s1)
	mv a0, a0
	jal ord
	mv a0, a0
	add s1, s2, a0
	mv s1, s1
	la a0, L60
	mv a0, a0
	jal ord
	mv a0, a0
	sub s1, s1, a0
	mv s1, s1
	sw s1, -8(s0)
	lw s1, 4(s0)
	addi s1, s1, -4
	mv s1, s1
	jal getchar
	mv a0, a0
	sw a0, 0(s1)
	j L68
L67:
	li s1, 0
	li s2, 4
	mul s1, s1, s2
	lw s2, -4(s0)
	mv s2, s2
	add s1, s2, s1
	lw s2, -8(s0)
	mv s2, s2
	sw s2, 0(s1)
	lw a0, -8(s0)
	mv a0, a0
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -20(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -16(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	j L116
L116:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
isdigit:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -12
L119:
	mv a0, a0
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s2, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
	lw s1, 4(s0)
	lw s1, 4(s1)
	lw a0, -4(s1)
	mv a0, a0
	jal ord
	mv a0, a0
	mv s1, a0
	la a0, L60
	mv a0, a0
	jal ord
	mv a0, a0
	bge s1, a0, L64
	blt s1, a0, L65
L65:
	li a0, 0
L66:
	mv a0, a0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s2
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	j L118
L64:
	li s1, 1
	sw s1, -4(s0)
	lw s1, 4(s0)
	lw s1, 4(s1)
	lw a0, -4(s1)
	mv a0, a0
	jal ord
	mv a0, a0
	mv s1, a0
	la a0, L61
	mv a0, a0
	jal ord
	mv a0, a0
	ble s1, a0, L62
	bgt s1, a0, L63
L63:
	li s1, 0
	sw s1, -4(s0)
L62:
	lw a0, -4(s0)
	mv a0, a0
	mv a0, a0
	j L66
L118:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
skipto:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -12
L121:
	mv s1, s1
	sw s1, -4(s0)
	mv s1, s1
	sw s1, -8(s0)
	mv s1, s1
	sw s1, -12(s0)
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s5, s1
	mv s4, s1
	mv s3, s1
	mv s9, s9
	mv s10, s10
	mv s11, s11
L58:
	lw s1, 4(s0)
	lw s1, 4(s1)
	lw a0, -4(s1)
	mv a0, a0
	la a1, L52
	mv a1, a1
	jal stringEqual
	mv a0, a0
	li s1, 0
	beq s1, a0, L55
	bne s1, a0, L54
L54:
	li s2, 1
L56:
	li s1, 0
	beq s2, s1, L57
	bne s2, s1, L59
L59:
	lw s1, 4(s0)
	lw s1, 4(s1)
	addi s1, s1, -4
	mv s1, s1
	jal getchar
	mv a0, a0
	sw a0, 0(s1)
	j L58
L55:
	lw s1, 4(s0)
	lw s1, 4(s1)
	lw a0, -4(s1)
	mv a0, a0
	la a1, L53
	mv a1, a1
	jal stringEqual
	mv s2, a0
	j L56
L57:
	li a0, 0
	mv s11, s11
	mv s10, s10
	mv s9, s9
	mv s1, s3
	mv s1, s4
	mv s1, s5
	mv s1, s6
	mv s1, s7
	mv s1, s8
	lw s1, -12(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -8(s0)
	mv s1, s1
	mv s1, s1
	lw s1, -4(s0)
	mv s1, s1
	mv s1, s1
	j L120
L120:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
.data 

__exitMessage: .string "Exited with code: "
__newLine: .string "\n"

.text

# Many of the below written functions assume that the given input is correct. May augment with more information later.  
# Will need to modify env.sml (Take care)

# Given the exit code (in a0 ofc), terminate with that exit code.
exit:
    mv t0, a0
    # Print exit message
    la a0, __exitMessage
    li a7, 4 
    ecall 
    # Print code
    mv a0, t0 
    li a7, 1 
    ecall 
    # Print new line
    la a0, __newLine 
    li a7, 4 
    ecall  
    # Exit
    li a7, 10
    ecall

# Not of non zero integer is 0 whereas not of 0 is 1.
not:
    beqz a0, retOne
    li a0, 0
    jr ra 
    retOne:
        li a0, 1
        jr ra 

# ------------------ Function for Real Type Begin ----------------------

printR:
    flw fa0, (a0)
    li a7, 2
    ecall
    jr ra

radd:
    flw fa0, (a0)
    flw fa1, (a1) 
    li a0, 4
    li a7, 9
    ecall
    fadd.s fa0, fa0, fa1
    fsw fa0, (a0)
    jr ra

rsub:
    flw fa0, (a0)
    flw fa1, (a1) 
    li a0, 4
    li a7, 9
    ecall
    fsub.s fa0, fa0, fa1
    fsw fa0, (a0)
    jr ra

rmul:
    flw fa0, (a0)
    flw fa1, (a1) 
    li a0, 4
    li a7, 9
    ecall
    fmul.s fa0, fa0, fa1
    fsw fa0, (a0)
    jr ra

rdiv:
    flw fa0, (a0)
    flw fa1, (a1) 
    li a0, 4
    li a7, 9
    ecall
    fdiv.s fa0, fa0, fa1
    fsw fa0, (a0)
    jr ra

realGreat:
    flw fa0, (a0)
    flw fa1, (a1) 
    flt.s a0, fa1, fa0
    jr ra

realLess:
    flw fa0, (a0)
    flw fa1, (a1) 
    flt.s a0, fa0, fa1
    jr ra

realEqual:
    flw fa0, (a0)
    flw fa1, (a1) 
    feq.s a0, fa0, fa1
    jr ra

# ------------------ Function for Real Type END ------------------------

# Given the string s in a0, return the number of characters in it.
# This is aswell needed for string concatenation.
size:
    mv t0, a0
    mv a0, zero
    sizeLoop:
        lb t1, (t0)
        beqz t1, sizeExit
        addi a0, a0, 1
        addi t0, t0, 1
        j sizeLoop
    sizeExit:
        jr ra

# Copy the string completely (i.e. including zero / null character) whose address is at a1, to the address starting at a0, returning the address of the last character of copied string.
stringCopy:
    stringCopyLoop:
        lb t0, (a1)
        sb t0, (a0)
        beqz t0, stringCopyExit
        addi a0, a0, 1
        addi a1, a1, 1
        j stringCopyLoop
    stringCopyExit:
        jr ra

# Concatenate str1 present in a0 with str2 present in a1.
concat:
    addi sp, sp, -12
    sw a0, (sp)
    sw a1, 4(sp)
    sw ra, 8(sp)
    jal size
    li t0, 1  # Will contain len(str1) + len(str2) + 1. '+1' for null character.
    add t0, t0, a0
    addi sp, sp, -4
    sw t0, (sp)
    lw a0, 8(sp)  # offset is changed
    jal size
    lw t0, (sp)
    add t0, t0, a0
    addi sp, sp, 4
    mv a0, t0
    li a7, 9
    ecall
    addi sp, sp, -4
    sw a0, (sp)
    lw a1, 4(sp)
    jal stringCopy
    lw a1, 8(sp)
    jal stringCopy
    lw a0, (sp)
    lw ra, 12(sp)
    addi sp, sp, 16
    jr ra

# "function substring (s: string, first : int, n : int) : string" Substring of string s, starting with character first, n characters long.
# Hoping that given input is valid.
substring:
    # Allocate space
    mv a3, a0  # saving a0
    mv a0, a2
    addi a0, a0, 1  # for null character 
    li a7, 9
    ecall
    # making a3 point to the desired substring
    add a3, a3, a1 
    mv t0, a0  # we need to return this
    substringLoop:
        lb t1, (a3) 
        sb t1, (a0)
        beqz a2, substringExit
        addi a0, a0, 1 
        addi a3, a3, 1
        addi a2, a2, -1
        j substringLoop
    substringExit: 
        mv a0, t0
        jr ra

# str1 > str2 ?
stringGreat:
    stringGreatLoop:
        lb a2 (a0)
        lb a3 (a1)
        bgt a2, a3  stringGreatA
        blt a2, a3  stringGreatB
        # If we have reached this point that means both are equal and if one of them is zero that means other is aswell 0, so in case strings are equal, I must return 0.
        beqz a2, stringGreatB
        addi a0, a0, 1
        addi a1, a1, 1
        j stringGreatLoop
    stringGreatA:
        li a0, 1
        jr ra
    stringGreatB:
        li a0, 0 
        jr ra 
    
# str1 < str2 ?
stringLess:
    stringLessLoop:
        lb a2 (a0)
        lb a3 (a1)
        blt a2, a3  stringLessA
        bgt a2, a3  stringLessB
        # If we have reached this point that means both are equal and if one of them is zero that means other is aswell 0, so in case strings are equal, I must return 0.
        beqz a2, stringLessB
        addi a0, a0, 1
        addi a1, a1, 1
        j stringLessLoop
    stringLessA:
        li a0, 1
        jr ra
    stringLessB:
        li a0, 0 
        jr ra 

# str1 == str2 ?
stringEqual:
    addi sp, sp, -12
    sw a0, (sp)
    sw a1, 4(sp)
    sw ra, 8(sp)
    jal stringGreat
    bnez a0, stringEqualExit
    lw a0, (sp)
    lw a1, 4(sp)
    jal stringLess
    bnez a0, stringEqualExit 
    li a0, 1 
    lw ra, 8(sp)
    addi sp, sp, 12 
    jr ra 
    stringEqualExit:
        li a0, 0    
        lw ra, 8(sp)
        addi sp, sp, 12 
        jr ra 


# Single-character string from ASCII value given in a0; halt program if a0 out of range.
chr:
    # Handling the error part 
    addi t0, zero, 127  
    bgt a0, t0, chrError
    bltz a0, chrError
    # Allocating
    mv t0, a0 
    li a0, 2 
    li a7, 9 
    ecall 
    # Putting the character
    sb t0 (a0)
    sb zero 1(a0)
    jr ra
    chrError:
        addi a0, zero, -1
        j exit

# Given a string in a0, return ASCII value of the first character of it, return -1 if the string is empty.
ord:
    lb t0, (a0)
    beqz t0, ordEmpty
    mv a0, t0 
    jr ra 
    ordEmpty:
        li a0, -1 
        jr ra 


# This version of getchar isn't preferred as it was getting difficult to read from file. (See Benjamin's email)
# getchar:
#     # Allocate space
#     li a0, 2
#     li a7, 9
#     ecall
#     sb zero, 1(a0)  # Null character 
#     # Read the character 
#     mv t0, a0 
#     li a7, 12 
#     ecall 
#     sb a0, (t0)  # Store the character 
#     mv a0, t0 
#     jr ra

# Read a character from standard input and return it as a string
getchar:
    # Allocate space
	li a0, 2 
	li a7, 9 
	ecall 
	mv t0, a0 
	li a0, 0   # Linux read system call, a0 = file descriptor
	mv a1, t0  # Address
	li a2, 1  # Length
	li a7, 63
	ecall 
	mv a0, t0 
	sb zero, 1(a0)  # Null character
	jr ra 

# Absolete as of now
flush:
    jr ra

# Print the string whose address is in a0
print:
    li a7, 4
    ecall
    jr ra

printI:
    # Examples in book do complex computation to print an integer, here I am putting an inbuilt function
    # Print the integer in a0
    li a7, 1
    ecall
    jr ra

# a0 contains the number of bytes we need to allocate. So, multiply it by 4 and allocate that much space from heap (system call 9). Return value is in a0 which tells the address to the allocated block (lower address value) and remember that in going downwards address decreases. Rest of the code is easy to follow.
initArray:
	li t0, 4
	mul a0, a0, t0
    mv t1, a0
	li a7, 9
	ecall
	mv t0, a0
	add t1, t1, t0
	initArrayLoop:
        sw a1, (t0)
        addi t0, t0, 4
        beq t0, t1, initArrayExit
        j initArrayLoop
    initArrayExit:
        jr ra

# Very similar to initArray
# We just need to allocate memory, no need to initialize it with 0.
allocRecord:
    li t0, 4
    mul a0, a0, t0
    li a7, 9
    ecall
    jr ra

