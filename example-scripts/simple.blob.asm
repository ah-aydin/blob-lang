
section .text
	global _start
main:
	mov r8, 8
	push r8
	mov r8, 10
	pop r9
	add r8, r9
	push r8
	mov r8, 2
	pop r9
	mov rax, r9
	idiv r8
	mov r8, rax
	mov rax, r8
	ret
_start:
	call main
	mov rdi, rax
	mov rax, 60
	syscall
