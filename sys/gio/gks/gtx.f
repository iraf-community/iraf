c GTX -- Text.  Unpack an f77 string and call gx_gtx to output the string.
c
	subroutine gtx (px, py, f77chars)
c
	real		px, py
	character*(*) 	f77chars
	integer*2     	sppchars(161)
c
c
c	Unpack characters from packed input array
c
	call f77upk (f77chars, sppchars, min (len(f77chars), 161))
	call gxgtx (px, py, sppchars)
c
c
	end
