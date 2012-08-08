	.seg	"text"
	.global	_ieee_enbint

! _IEEE_ENBINT -- Enable the floating point exceptions indicated by the
! bitmask passed as the only argument.  The current bitmask is returned as
! the function value.

_ieee_enbint:
	set	0x0f800000,%o4
	sll	%o0,23,%o1
	st	%fsr,[%sp+0x44]
	ld	[%sp+0x44],%o0
	and	%o1,%o4,%o1
	andn	%o0,%o4,%o2
	or	%o1,%o2,%o1
	st	%o1,[%sp+0x44]
	ld	[%sp+0x44],%fsr
	and	%o0,%o4,%o0
	retl
	srl	%o0,23,%o0
