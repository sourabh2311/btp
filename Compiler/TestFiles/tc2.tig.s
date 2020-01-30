
.globl main
.data
L1: .string "\n"

.text
main:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -8
L3:
	sw s1, -4(s0)
	mv s9, s1
	mv s8, s1
	mv s7, s1
	mv s6, s1
	mv s4, s1
	sw s1, -8(s0)
	mv s3, s1
	mv s2, s1
	sw s0, -20(sp)
	li a0, 1
	li a1, 2
	li a2, 3
	li a3, 4
	li a4, 5
	li a5, 6
	li a6, 7
	li a7, 8
	li s5, 9
	sw s5, -16(sp)
	li s5, 10
	sw s5, -12(sp)
	li s5, 11
	sw s5, -8(sp)
	li s5, 12
	sw s5, -4(sp)
	jal last
	jal printI
	la a0, L1
	jal print
	li a0, 0
	jal exit
	mv s1, s2
	mv s1, s3
	lw s1, -8(s0)
	mv s1, s4
	mv s1, s6
	mv s1, s7
	mv s1, s8
	mv s1, s9
	lw s1, -4(s0)
	j L2
L2:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 8
jr ra
last:
addi sp, sp, -24
sw s0 0(sp)
mv s0 sp
addi sp sp -0
L5:
	lw a0, 20(s0)
	j L4
L4:
	mv sp s0
lw s0 0(sp)
addi sp, sp, 24
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

