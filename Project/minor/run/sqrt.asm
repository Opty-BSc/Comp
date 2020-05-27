; GLOBL
global	$_sqrt:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_sqrt:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 4
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
$_i1:
; LOCV
	push	dword [ebp+8]
; IMM
	push	dword 0
; JLT
	pop	eax
	pop	ecx
	cmp	ecx, eax
	jl	near $_i3
; LOCV
	push	dword [ebp+8]
; LOCV
	push	dword [ebp+-4]
; SUB
	pop	eax
	sub	dword [esp], eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+8], eax
; TRASH
	add	esp, 4
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
; LABEL
$_i2:
; LOCV
	push	dword [ebp+8]
; LOCV
	push	dword [ebp+-4]
; SUB
	pop	eax
	sub	dword [esp], eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+8], eax
; TRASH
	add	esp, 4
; JMP
	jmp	dword $_i1
; LABEL
$_i3:
; LOCV
	push	dword [ebp+-4]
; IMM
	push	dword 1
; SUB
	pop	eax
	sub	dword [esp], eax
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
