; GLOBL
global	$_printlns:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_printlns:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 0
; LOCV
	push	dword [ebp+8]
; CALL
	call	$_prints
; TRASH
	add	esp, 4
; PUSH
	push	eax
; RODATA
segment	.rodata
; ALIGN
align	4
; LABEL
$_i1:
; CHAR
	db	0x0A
; CHAR
	db	0x00
; TEXT
segment	.text
; ADDR
	push	dword $_i1
; CALL
	call	$_prints
; TRASH
	add	esp, 4
; PUSH
	push	eax
; LEAVE
	leave
; RET
	ret
; EXTRN
extern	$_prints
