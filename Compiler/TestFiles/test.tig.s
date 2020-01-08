
.globl main
.data
L12: .string "B"
L11: .string "A"

.text
main:
addi sp, sp, -8
sw s0 0(sp)
mv s0 sp
addi sp sp -32
L23:
	mv t0, ra
	sw t0, -4(s0)
	sw s1, -8(s0)
	mv s1, s2
	sw s1, -12(s0)
	mv s1, s3
	sw s1, -16(s0)
	mv s1, s4
	sw s1, -20(s0)
	mv s1, s5
	sw s1, -24(s0)
	mv s1, s6
	sw s1, -28(s0)
	mv s1, s7
	sw s1, -32(s0)
	mv s7, s9
	mv s6, s10
	mv s5, s11
	la s1, L11
	mv a0, s1
	la s1, L12
	mv a1, s1
	jal stringGreat
	mv s4, a0
	la s1, L12
	mv a0, s1
	la s1, L11
	mv a1, s1
	jal stringGreat
	mv s3, a0
	la s1, L12
	mv a0, s1
	la s1, L12
	mv a1, s1
	jal stringGreat
	mv s10, a0
	li s1, 0
	beq s1, s10, L14
	bne s1, s10, L13
L13:
	li s1, 1
L15:
	mv s2, s1
	la s1, L12
	mv a0, s1
	la s1, L11
	mv a1, s1
	jal stringGreat
	mv s10, a0
	li s1, 0
	beq s1, s10, L17
	bne s1, s10, L16
L16:
	li s1, 1
L18:
	mv s11, s1
	la s1, L11
	mv a0, s1
	la s1, L11
	mv a1, s1
	jal stringEqual
	mv s10, a0
	la s1, L12
	mv a0, s1
	la s1, L11
	mv a1, s1
	jal stringEqual
	mv s9, a0
	li s1, 0
	beq s1, s9, L20
	bne s1, s9, L19
L19:
	li s1, 0
L21:
	mv a0, s4
	jal printI
	mv a0, s3
	jal printI
	mv a0, s2
	jal printI
	mv a0, s11
	jal printI
	mv a0, s10
	jal printI
	mv a0, s1
	jal printI
	li a0, 0
	jal exit
	mv s11, s5
	mv s10, s6
	mv s9, s7
	lw s1, -32(s0)
	mv s7, s1
	lw s1, -28(s0)
	mv s6, s1
	lw s1, -24(s0)
	mv s5, s1
	lw s1, -20(s0)
	mv s4, s1
	lw s1, -16(s0)
	mv s3, s1
	lw s1, -12(s0)
	mv s2, s1
	lw s1, -8(s0)
	lw t0, -4(s0)
	mv ra, t0
	j L22
L14:
	la s1, L12
	mv a0, s1
	la s1, L12
	mv a1, s1
	jal stringEqual
	mv s1, a0
	j L15
L17:
	la s1, L12
	mv a0, s1
	la s1, L11
	mv a1, s1
	jal stringEqual
	mv s1, a0
	j L18
L20:
	li s1, 1
	j L21
L22:
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

