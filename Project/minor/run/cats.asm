; GLOBL
global	$_cats:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_cats:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 16
; LOCV
	push	dword [ebp+12]
; CALL
	call	$_strlen
; TRASH
	add	esp, 4
; PUSH
	push	eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-12], eax
; TRASH
	add	esp, 4
; LOCV
	push	dword [ebp+8]
; CALL
	call	$_strlen
; TRASH
	add	esp, 4
; PUSH
	push	eax
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-16], eax
; TRASH
	add	esp, 4
; LOCV
	push	dword [ebp+-12]
; LOCV
	push	dword [ebp+-16]
; ADD
	pop	eax
	add	dword [esp], eax
; IMM
	push	dword 1
; ADD
	pop	eax
	add	dword [esp], eax
; ALLOC
	pop	eax
	sub	esp, eax
; SP
	push	esp
; LOCA
	pop	eax
	mov	[ebp+-4], eax
; IMM
	push	dword 0
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-8], eax
; TRASH
	add	esp, 4
; LABEL
$_i1:
; LOCV
	push	dword [ebp+-8]
; LOCV
	push	dword [ebp+-12]
; JEQ
	pop	eax
	pop	ecx
	cmp	ecx, eax
	je	near $_i3
; LOCV
	push	dword [ebp+12]
; LOCV
	push	dword [ebp+-8]
; ADD
	pop	eax
	add	dword [esp], eax
; LDCHR
	pop	ecx
	movsx	eax,byte [ecx]
	push	eax
; COPY
	push	dword [esp]
; LOCV
	push	dword [ebp+-4]
; LOCV
	push	dword [ebp+-8]
; ADD
	pop	eax
	add	dword [esp], eax
; STCHR
	pop	ecx
	pop	eax
	mov	[ecx], al
; TRASH
	add	esp, 4
; LABEL
$_i2:
; LOCAL
	lea	eax, [ebp+-8]
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
	jmp	dword $_i1
; LABEL
$_i3:
; IMM
	push	dword 0
; COPY
	push	dword [esp]
; LOCA
	pop	eax
	mov	[ebp+-8], eax
; TRASH
	add	esp, 4
; LABEL
$_i4:
; LOCV
	push	dword [ebp+-8]
; LOCV
	push	dword [ebp+-16]
; JEQ
	pop	eax
	pop	ecx
	cmp	ecx, eax
	je	near $_i6
; LOCV
	push	dword [ebp+8]
; LOCV
	push	dword [ebp+-8]
; ADD
	pop	eax
	add	dword [esp], eax
; LDCHR
	pop	ecx
	movsx	eax,byte [ecx]
	push	eax
; COPY
	push	dword [esp]
; LOCV
	push	dword [ebp+-4]
; LOCV
	push	dword [ebp+-12]
; LOCV
	push	dword [ebp+-8]
; ADD
	pop	eax
	add	dword [esp], eax
; ADD
	pop	eax
	add	dword [esp], eax
; STCHR
	pop	ecx
	pop	eax
	mov	[ecx], al
; TRASH
	add	esp, 4
; LABEL
$_i5:
; LOCAL
	lea	eax, [ebp+-8]
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
	jmp	dword $_i4
; LABEL
$_i6:
; IMM
	push	dword 0
; COPY
	push	dword [esp]
; LOCV
	push	dword [ebp+-4]
; LOCV
	push	dword [ebp+-12]
; LOCV
	push	dword [ebp+-16]
; ADD
	pop	eax
	add	dword [esp], eax
; ADD
	pop	eax
	add	dword [esp], eax
; STCHR
	pop	ecx
	pop	eax
	mov	[ecx], al
; TRASH
	add	esp, 4
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
; EXTRN
extern	$_strlen
