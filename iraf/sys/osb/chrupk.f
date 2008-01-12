c CHRUPK -- Unpack bytes into XCHAR (integer*2).  Should work on most byte
c addressable machines.  The input and output arrays may be the same.

	subroutine chrupk (a, aoff, b, boff, nchars)

	character*1 a(*)
	integer*2 b(*)
	integer	aoff, boff, nchars, i

 		do 10 i = 0, nchars-1
		    b(boff+i) = ichar (a(aoff+i))
 10		continue
	end
