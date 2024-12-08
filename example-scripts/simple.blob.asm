section .text
	global _start
main:
	mov r8, 1
	not r8
	cmp r8, 1
	jne _ifEndElseStart_0
	jmp _ifElseEnd_1
_ifEndElseStart_0:
	mov r8, 9
	push r8
	mov r8, 2
	pop r9
	add r8, r9
	mov rax, r8
	ret
_ifElseEnd_1:
	mov r8, 20
	push r8
	mov r8, 2
	neg r8
	pop r9
	sub r8, r9
	neg r8
	mov rax, r8
	ret
_start:
	call main
	mov rdi, rax
	mov rax, 60
	syscall
