; GLOBL
global	$_factorial:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_factorial:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 4
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 2
; JGE
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jge	near $_i1
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 0
; JGE
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jge	near $_i2
; IMM
	push	dword 1
; NEG
	neg	dword [esp]
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
; JMP
	jmp	dword $_i3
; LABEL
$_i2:
; LABEL
$_i3:
; IMM
	push	dword 1
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
; JMP
	jmp	dword $_i4
; LABEL
$_i1:
; LABEL
$_i4:
; IMM
	push	dword 1
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-4], eax
; TRASH
	add	esp, 4
; LABEL
$_i5:
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 2
; JLT
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jl	near $_i7
; LOCV
	push	dword [ebp+-4]
; LOCV
	push	dword [ebp+8]
; MUL
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-4], eax
; TRASH
	add	esp, 4
; LABEL
$_i6:
; LOCAL
	lea	eax, [ebp+8]
	push	eax
; COPY
	push	dword [esp]
; DECR
	pop	eax
	sub	dword [eax], 1
; LOAD
	pop	eax
	push	dword [eax]
; TRASH
	add	esp, 4
; JMP
	jmp	dword $_i5
; LABEL
$_i7:
; LOCV
	push	dword [ebp+-4]
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
