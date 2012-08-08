c CHRPAK -- Pack XCHAR (integer*2) into bytes.  Should work on most byte
c addressable machines.  The input and output arrays may be the same.

	subroutine chrpak (a, aoff, b, boff, nchars)

	integer*2 a(*)
	character*1 b(*)
	integer	aoff, boff, nchars, i

 		do 10 i = 0, nchars-1
		    b(boff+i) = char (a(aoff+i))
 10		continue
	end
