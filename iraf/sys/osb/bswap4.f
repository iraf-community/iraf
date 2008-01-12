c BSWAP4 - Move bytes from array "a" to array "b", swapping the four bytes
c in each successive 4 byte group, i.e., 12345678 becomes 43218765.

	subroutine bswap4 (a, aoff, b, boff, nbytes)

	character*1 a(*), b(*), temp
	integer	aoff, boff, nbytes, i
	integer aoff1, boff1, aoff2, boff2, aoff3, boff3

		if (nbytes .le. 4) then
		    return
		endif

		aoff1 = aoff + 1
		boff1 = boff + 1
		aoff2 = aoff + 2
		boff2 = boff + 2
		aoff3 = aoff + 3
		boff3 = boff + 3

 		do 10 i = 0, nbytes-3, 4
		    temp = a(aoff1+i)
		    b(boff1+i) = a(aoff2+i)
		    b(boff2+i) = temp
		    temp = a(aoff3+i)
		    b(boff3+i) = a(aoff+i)
		    b(boff+i) = temp
 10		continue
	end
