.data 

__exitMessage: .string "Exited with code: "
__newLine: .string "\n"

.text

# Many of the below written functions assume that the given input is correct. May augment with more information later.  
# Will need to modify env.sml (Take care)

exit:
    # Given the exit code (in a0 ofc), terminate with that exit code.
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

not:
    # Not of non zero integer is 0 whereas not of 0 is 1.
    beqz a0, retOne
    li a0, 0
    jr ra 
    retOne:
        li a0, 1
        jr ra 

size:
    # Given the string s in a0, return the number of characters in it.
    # This is aswell needed for string concatenation.
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

stringCopy:
    # Copy the string completely (i.e. including zero / null character) whose address is at a1, to the address starting at a0, returning the address of the last character of copied string.
    stringCopyLoop:
        lb t0, (a1)
        sb t0, (a0)
        beqz t0, stringCopyExit
        addi a0, a0, 1
        addi a1, a1, 1
        j stringCopyLoop
    stringCopyExit:
        jr ra

concat:
    # Contain str1 present in a0 with str2 present in a1.
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

substring:
    # "function substring (s: string, first : int, n : int) : string" Substring of string s, starting with character first, n characters long.
    # Hoping that given input is valid.
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

chr:
    # Single-character string from ASCII value given in a0; halt program if a0 out of range.
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

ord:
    # Given a string in a0, return ASCII value of the first character of it, return -1 if the string is empty.
    lb t0, (a0)
    beqz t0, ordEmpty
    mv a0, t0 
    jr ra 
    ordEmpty:
        li a0, -1 
        jr ra 

getchar:
    # Read a character from standard input and return it as a string; return empty string on end of file.
    # Allocate space
    li a0, 2
    li a7, 9
    ecall
    sb zero, 1(a0)  # Null character 
    # Read the character 
    mv t0, a0 
    li a7, 12 
    ecall 
    sb a0, (t0)  # Store the character 
    mv a0, t0 
    jr ra

flush:
    # Absolete as of now
    jr ra

print:
    # Print the string whose address is in a0
    li a7, 4
    ecall
    jr ra

# printInt:
#     # Examples in book do complex computation to print an integer, here I am putting an inbuilt function
#     # Print the integer in a0
#     li a7, 1
#     ecall
#     jr ra

initArray:
    # a0 contains the number of bytes we need to allocate. So, multiply it by 4 and allocate that much space from heap (system call 9). Return value is in a0 which tells the address to the allocated block (lower address value) and remember that in going downwards address decreases. Rest of the code is easy to follow.
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
        bne t0, t1, initArrayLoop
        jr ra

allocRecord:
    # Very similar to initArray
    # We just need to allocate memory, no need to initialize it with 0.
    li t0, 4
    mul a0, a0, t0
    li a7, 9
    ecall
    jr ra

strcmp:
    # Authors runtime.s contains stringEqual function, which I am replacing with a more powerful strcmp function.
    # To compare two strings s1, s2 whose address is in a0, a1. Return 1 if s1 > s2, 0 if s1 = s2 and -1 o/w.
    strcmpTest:
        lb a2 (a0)
        lb a3 (a1)
        beqz a2, strcmpEnd
        beqz a3, strcmpEnd
        bgt a2, a3  strcmpGreat
        blt a2, a3  strcmpLess
        addi a0, a0, 1
        addi a1, a1, 1
        j strcmpTest
    strcmpGreat:
        li a0, 1
        jr ra
    strcmpLess:
        li a0, -1
        jr ra
    strcmpEnd:
        bne a2 zero strcmpGreat
        bne a3 zero strcmpLess
        li a0, 0
        jr ra









