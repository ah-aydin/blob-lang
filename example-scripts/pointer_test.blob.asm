section .text
	global _start
main:
	mov r8, 3
	mov rax, r8
	ret
_start:
	call main
	mov rdi, rax
	mov rax, 60
	syscall
