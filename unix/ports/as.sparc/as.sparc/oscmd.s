	.seg	"text"			! [internal]
	.proc	4
	.global	_oscmd_
_oscmd_:
!#PROLOGUE# 0
!#PROLOGUE# 1
	save	%sp,-104,%sp
	sethi	%hi(VAR_SEG1+16),%l0	! [internal]
	or	%l0,%lo(VAR_SEG1+16),%l0 ! [internal]
	st	%i1,[%fp+72]
	st	%i3,[%fp+80]
	call	_smark_,1
	mov	%l0,%o0
	sethi	%hi(L1D168),%o0
	add	%o0,%lo(L1D168),%i3
	sethi	%hi(L1D169),%o1
	add	%l0,16,%o0
	add	%o1,%lo(L1D169),%i4
	mov	%i4,%o1
	call	_salloc_,3
	mov	%i3,%o2
	sethi	%hi(VAR_SEG1+32),%o2
	ld	[%o2+%lo(VAR_SEG1+32)],%l5
	sethi	%hi(L1D164),%o3
	add	%o3,%lo(L1D164),%i5
	inc	20,%l0			! [internal]
	mov	%l0,%o0
	mov	%i5,%o1
	call	_salloc_,3
	mov	%i3,%o2
	add	%l0,-8,%o0
	mov	%i5,%o1
	call	_salloc_,3
	mov	%i3,%o2
	add	%l0,-12,%o0
	mov	%i5,%o1
	call	_salloc_,3
	mov	%i3,%o2
	add	%l0,-16,%o0
	mov	%i5,%o1
	call	_salloc_,3
	mov	%i3,%o2
	sethi	%hi(L1D148),%o0
	call	_clstai_,1
	or	%o0,%lo(L1D148),%o0	! [internal]
	cmp	%o0,1
	be	L77048
	nop
	ld	[%fp+72],%l6
	sethi	%hi(_mem_-2),%o5
	or	%o5,%lo(_mem_-2),%o5	! [internal]
	sll	%l5,1,%o4
	add	%o5,%o4,%o7
	mov	%o7,%l7
	mov	%l7,%o1
	mov	%i4,%o2
	call	_strpak_,3
	mov	%i0,%o0
	ldsh	[%l6],%l0
	tst	%l0
	bne,a	LY14
	sethi	%hi(VAR_SEG1+36),%o0
	sethi	%hi(VAR_SEG1+36),%l1
	ld	[%l1+%lo(VAR_SEG1+36)],%l1
	sethi	%hi(_mem_-2),%l3
	or	%l3,%lo(_mem_-2),%l3	! [internal]
	sll	%l1,1,%i0
	add	%l3,%i0,%i0
	sethi	%hi(v.16),%o0
	or	%o0,%lo(v.16),%o0	! [internal]
	mov	%i0,%o1
	call	_strpak_,3
	mov	%i5,%o2
	b	LY13
	ld	[%fp+80],%i1
LY14:					! [internal]
	ld	[%o0+%lo(VAR_SEG1+36)],%o0
	sethi	%hi(_mem_-2),%o2
	sll	%o0,1,%o1
	or	%o2,%lo(_mem_-2),%o2	! [internal]
	add	%o2,%o1,%o3
	mov	%o3,%i0
	mov	%i0,%o1
	mov	%i5,%o2
	call	_fmapfn_,3
	mov	%l6,%o0
	ld	[%fp+80],%i1
LY13:					! [internal]
	call	_fnulle_,1
	mov	%i2,%o0
	tst	%o0
	bne,a	LY12
	sethi	%hi(VAR_SEG1+20),%o4
	call	_fnulle_,1
	mov	%i1,%o0
	tst	%o0
	be,a	LY11
	sethi	%hi(VAR_SEG1+20),%l1
	sethi	%hi(VAR_SEG1+20),%o4
LY12:					! [internal]
	ld	[%o4+%lo(VAR_SEG1+20)],%o4
	sethi	%hi(_mem_-2),%o7
	sll	%o4,1,%o5
	or	%o7,%lo(_mem_-2),%o7	! [internal]
	add	%o7,%o5,%l0
	mov	%l0,%i3
	sethi	%hi(v.17),%o0
	or	%o0,%lo(v.17),%o0	! [internal]
	mov	%i3,%o1
	call	_xmktep_,3
	mov	%i5,%o2
	b	LY10
	ldsh	[%i2],%o0
LY11:					! [internal]
	ld	[%l1+%lo(VAR_SEG1+20)],%l1
	sethi	%hi(_mem_-2),%l3
	or	%l3,%lo(_mem_-2),%l3	! [internal]
	sll	%l1,1,%l2
	add	%l3,%l2,%l2
	mov	%l2,%i3
	sth	%g0,[%i3]
	ldsh	[%i2],%o0
LY10:					! [internal]
	tst	%o0
	bne	L77021
	sethi	%hi(VAR_SEG1+28),%o1
	ld	[%o1+%lo(VAR_SEG1+28)],%o1
	sethi	%hi(_mem_-2),%o3
	or	%o3,%lo(_mem_-2),%o3	! [internal]
	sll	%o1,1,%i4
	add	%o3,%i4,%i4
	sethi	%hi(v.18),%o0
	or	%o0,%lo(v.18),%o0	! [internal]
	mov	%i4,%o1
	call	_strpak_,3
	mov	%i5,%o2
	b	LY9
	ldsh	[%i1],%o1
L77021:
	call	_fnulle_,1
	mov	%i2,%o0
	tst	%o0
	be,a	LY8
	sethi	%hi(VAR_SEG1+28),%l2
	sethi	%hi(VAR_SEG1+28),%o5
	ld	[%o5+%lo(VAR_SEG1+28)],%o5
	sethi	%hi(_mem_-2),%l0
	or	%l0,%lo(_mem_-2),%l0	! [internal]
	sll	%o5,1,%o7
	add	%l0,%o7,%l1
	mov	%i3,%o0
	b	LY1
	mov	%l1,%i4
LY8:					! [internal]
	ld	[%l2+%lo(VAR_SEG1+28)],%l2
	sethi	%hi(_mem_-2),%l4
	or	%l4,%lo(_mem_-2),%l4	! [internal]
	sll	%l2,1,%l3
	add	%l4,%l3,%i4
	mov	%i2,%o0
LY1:					! [internal]
	mov	%i5,%o2
	call	_fmapfn_,3
	mov	%i4,%o1
	ldsh	[%i1],%o1
LY9:					! [internal]
	tst	%o1
	bne	L77031
	sethi	%hi(VAR_SEG1+24),%o2
	ld	[%o2+%lo(VAR_SEG1+24)],%o2
	sethi	%hi(_mem_-2),%o4
	sll	%o2,1,%o3
	or	%o4,%lo(_mem_-2),%o4	! [internal]
	add	%o4,%o3,%o5
	mov	%o5,%i2
	sethi	%hi(v.19),%o0
	or	%o0,%lo(v.19),%o0	! [internal]
	mov	%i2,%o1
	call	_strpak_,3
	mov	%i5,%o2
	b	LY7
	sethi	%hi(VAR_SEG1),%o4
L77031:
	call	_fnulle_,1
	mov	%i1,%o0
	tst	%o0
	be,a	LY6
	sethi	%hi(VAR_SEG1+24),%l3
	sethi	%hi(VAR_SEG1+24),%o7
	ld	[%o7+%lo(VAR_SEG1+24)],%o7
	sethi	%hi(_mem_-2),%l1
	or	%l1,%lo(_mem_-2),%l1	! [internal]
	sll	%o7,1,%i2
	mov	%i3,%o0
	b	LY2
	add	%l1,%i2,%i2
LY6:					! [internal]
	ld	[%l3+%lo(VAR_SEG1+24)],%l3
	sethi	%hi(_mem_-2),%o0
	or	%o0,%lo(_mem_-2),%o0	! [internal]
	sll	%l3,1,%l4
	add	%o0,%l4,%o1
	mov	%o1,%i2
	mov	%i1,%o0
LY2:					! [internal]
	mov	%i5,%o2
	call	_fmapfn_,3
	mov	%i2,%o1
	sethi	%hi(VAR_SEG1),%o4
LY7:					! [internal]
	or	%o4,%lo(VAR_SEG1),%o4	! [internal]
	mov	%i2,%o3
	mov	%i4,%o2
	mov	%i0,%o1
	call	_koscmd_,5
	mov	%l7,%o0
	ldsh	[%i3],%o3
	sethi	%hi(VAR_SEG1),%o2
	ld	[%o2+%lo(VAR_SEG1)],%i5
	tst	%o3
	be,a	LY3
	sethi	%hi(VAR_SEG1+16),%o0
	call	_xerpsh_,0
	nop
	call	_xfdele_,1
	mov	%i3,%o0
	call	_xerpop_,0
	nop
	tst	%o0
	be,a	LY3
	sethi	%hi(VAR_SEG1+16),%o0
	sethi	%hi(L1D54),%o0
	call	_erract_,1
	or	%o0,%lo(L1D54),%o0	! [internal]
	sethi	%hi(_xercom_),%o4
	ld	[%o4+%lo(_xercom_)],%o4
	tst	%o4
	be,a	LY3
	sethi	%hi(VAR_SEG1+16),%o0
	b	LY5
	sethi	%hi(VAR_SEG1),%o0	! [internal]
L77048:
	call	_xffluh_,1
	mov	%i3,%o0
	sethi	%hi(_mem_-2),%o0	! [internal]
	add	%l5,1,%l1
	mov	%l1,%i2
	or	%o0,%lo(_mem_-2),%o0	! [internal]
	sll	%i2,1,%l3
	mov	%l3,%i3
	mov	2,%i5
	inc	-2,%i0
	add	%i5,%i0,%i0
	add	%i3,%o0,%o1
	mov	%o0,%o7
	sll	%l5,1,%o5
	mov	33,%l0
	sth	%l0,[%o5+%o7]
	mov	%o1,%i3
	mov	%i0,%i5
L77049:
	ldsh	[%i5],%i4
	tst	%i4
	be,a	LY4
	sethi	%hi(_mem_-2),%o0	! [internal]
	ldsh	[%i5],%i0
	cmp	%i4,10
	be,a	LY4
	sethi	%hi(_mem_-2),%o0	! [internal]
	sth	%i0,[%i3]
	inc	%i2
	inc	2,%i5
	b	L77049
	inc	2,%i3
LY4:					! [internal]
	or	%o0,%lo(_mem_-2),%o0	! [internal]
	sll	%i2,1,%i2
	mov	%i2,%i5
	mov	%o0,%o3
	mov	10,%o4
	sth	%o4,[%i5+%o3]
	add	%o0,2,%o5
	sth	%g0,[%i5+%o5]
	mov	%o0,%o1
	sethi	%hi(L1D168),%o7
	add	%o7,%lo(L1D168),%i5
	sll	%l5,1,%l0
	add	%o1,%l0,%o1
	call	_putlie_,2
	mov	%i5,%o0
	call	_xffluh_,1
	mov	%i5,%o0
	sethi	%hi(L1D148),%l1
	add	%l1,%lo(L1D148),%i3
	mov	0,%i5
L77055:
	sethi	%hi(VAR_SEG1+4),%o1
	or	%o1,%lo(VAR_SEG1+4),%o1	! [internal]
	call	_getci_,2
	mov	%i3,%o0
	cmp	%o0,-2
	be,a	LY3
	sethi	%hi(VAR_SEG1+16),%o0
	sethi	%hi(VAR_SEG1+4),%l2
	ld	[%l2+%lo(VAR_SEG1+4)],%l2
	cmp	%l2,10
	be,a	LY3
	sethi	%hi(VAR_SEG1+16),%o0
	mov	%i5,%o0
	sll	%o0,1,%o0
	mov	%o0,%o1
	sethi	%hi(VAR_SEG1+4),%l3
	ld	[%l3+%lo(VAR_SEG1+4)],%l3
	sll	%o1,2,%o1
	add	%o0,%o1,%o0
	add	%l3,-48,%l4
	add	%o0,%l4,%o0
	b	L77055
	mov	%o0,%i5
LY3:					! [internal]
	call	_sfree_,1
	or	%o0,%lo(VAR_SEG1+16),%o0 ! [internal]
	mov	%i5,%i3
	sethi	%hi(VAR_SEG1),%o0	! [internal]
LY5:					! [internal]
	or	%o0,%lo(VAR_SEG1),%o0	! [internal]
	st	%i5,[%o0]
	st	%l5,[%o0+32]
	ret
	restore	%g0,%i3,%o0
	.seg	"data"			! [internal]
	.common	_mem_,8
	.common	_xercom_,4
	.align	8
	.align	4
L1D168:
	.word	2
	.align	4
L1D169:
	.word	0x400
	.align	4
L1D164:
	.word	127
	.align	4
L1D148:
	.word	1
	.align	4
L1D54:
	.word	3
	.align	4
v.16:
	.half	0
	.align	4
v.17:
	.word	0x74006d
	.word	0x700024
	.word	0x6e0075
	.word	0x6c006c
	.skip	2
	.align	4
v.18:
	.skip	2
	.align	4
v.19:
	.skip	2
	.seg	"bss"			! [internal]
	.align	8
VAR_SEG1:
	.skip	40
