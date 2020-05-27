; GLOBL
global	$_exp:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_exp:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 0
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 10
; LOCV
	push	dword [ebp+12]
; CALL
	call	$_poweri
; TRASH
	add	esp, 8
; PUSH
	push	eax
; MUL
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
; LEAVE
	leave
; RET
	ret
; EXTRN
extern	$_poweri
