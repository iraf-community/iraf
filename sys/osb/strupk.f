c STRUPK -- Unpack a kernel (C style) string into an SPP string.  The unpacking
c operation can be performed in place.  A kernel string consists of a sequence
c of host characters stored one character per byte, delimited by EOS='\0'.
c We assume here that the host character set is ASCII.  If this is not the
c case code must be added to convert from the host character set to ASCII in
c the unpacked string.
c
c N.B.: If sizeof(XCHAR)=1, XEOS=EOS, and the host character set is ASCII,
c and the operation is being performed in place, then this procedure should
c do nothing.
c 
c N.B.: This code ASSUMES that XCHAR is implemented as INTEGER*2 and that
c both XEOS and EOS are 0.

	subroutine strupk (instr, outstr, maxch)

	character*1 instr(*)
	integer*2 outstr(*)
	integer maxch, EOS
	parameter (EOS=0)
	integer i

		    
c Determine length of string so that we can unpack it in the reverse
c direction.
		i = 1
 10		continue
		if (ichar (instr(i)) .eq. EOS .or. i .gt. maxch) goto 20
		    i = i + 1
		    goto 10
 20		continue

c Unpack the string from right to left.
c
		outstr(i) = EOS
		do 30 i=i, 1, -1
		    outstr(i) = ichar (instr(i))
 30		continue
	end
