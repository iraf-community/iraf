c BYTMOV -- Byte move from array "a" to array "b".  The move must be
c nondestructive, allowing a byte array to be shifted left or right a
c few bytes, hence calls to zlocva() are required to get the addresses of 
c the arrays.

	subroutine bytmov (a, aoff, b, boff, nbytes)

	character*1 a(*), b(*)
	integer	aoff, boff, nbytes
	integer	fwaa, lwaa, fwab, i

		call zlocva (a(aoff), fwaa)
		call zlocva (a(aoff+nbytes-1), lwaa)
		call zlocva (b(boff), fwab)

		if (fwaa .eq. fwab) then
		    return
		else if (fwab .ge. fwaa .and. fwab .le. lwaa) then
		    do 10 i = nbytes-1, 0, -1
			b(boff+i) = a(aoff+i)
 10		    continue
		else
		    do 20 i = 0, nbytes-1
			b(boff+i) = a(aoff+i)
 20		    continue
		endif
	end
