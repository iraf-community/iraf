c BSWAP2 - Move bytes from array "a" to array "b", swapping successive
c pairs of bytes.

	subroutine bswap2 (a, aoff, b, boff, nbytes)

	character*1 a(*), b(*), temp
	integer	aoff, boff, nbytes, i
	integer aoff1, boff1

		aoff1 = aoff + 1
		boff1 = boff + 1

 		do 10 i = 0, nbytes-1, 2
		    temp = a(aoff1+i)
		    if (i .ne. nbytes) then
			b(boff1+i) = a(aoff+i)
		    endif
		    b(boff+i) = temp
 10		continue
	end
