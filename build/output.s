.text
.global main
main:
	push {ip,lr}
	mov r0, #44
	add r0, r0, #2
	pop {ip,lr}
	bx lr
