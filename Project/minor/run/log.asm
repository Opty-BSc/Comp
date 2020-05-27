; GLOBL
global	$_log:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_log:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 4
; LOCV
	push	dword [ebp+12]
; IMM
	push	dword 1
; LT
	pop	eax
	xor	ecx, ecx
	cmp	[esp], eax
	setl	cl
	mov	[esp], ecx
; COPY
	push	dword [esp]
; JNZ
	pop	eax
	cmp	eax, byte 0
	jne	near $_i2
; TRASH
	add	esp, 4
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 1
; LT
	pop	eax
	xor	ecx, ecx
	cmp	[esp], eax
	setl	cl
	mov	[esp], ecx
; LABEL
$_i2:
; JZ
	pop	eax
	cmp	eax, byte 0
	je	near $_i1
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
$_i1:
; LABEL
$_i3:
; LOCV
	push	dword [ebp+12]
; LOCV
	push	dword [ebp+8]
; JGE
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jge	near $_i4
; IMM
	push	dword 0
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
; JMP
	jmp	dword $_i5
; LABEL
$_i4:
; LABEL
$_i5:
; IMM
	push	dword 0
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-4], eax
; TRASH
	add	esp, 4
; LABEL
$_i6:
; LOCV
	push	dword [ebp+12]
; IMM
	push	dword 2
; JLT
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jl	near $_i8
; LOCV
	push	dword [ebp+12]
; LOCV
	push	dword [ebp+8]
; DIV
	pop	ecx
	pop	eax
	cdq
	idiv	ecx
	push	eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+12], eax
; TRASH
	add	esp, 4
; LABEL
$_i7:
; LOCAL
	lea	eax, [ebp+-4]
	push	eax
; COPY
	push	dword [esp]
; INCR
	pop	eax
	add	dword [eax], 1
; LOAD
	pop	eax
	push	dword [eax]
; TRASH
	add	esp, 4
; JMP
	jmp	dword $_i6
; LABEL
$_i8:
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
