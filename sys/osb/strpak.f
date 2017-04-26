c STRPAK -- Pack an SPP character string into a C string, i.e., a sequence
c of characters stored one per byte, delimited by EOS='\0'.  The operation
c may be performed in place.  This version assumes that the host character
c set is ASCII and hence no lookup table reference to map character sets is
c needed.  If this is not the case, code must be added to convert to the host
c character set.
c
c N.B.: If sizeof(XCHAR)=1, XEOS=EOS, and the host character set is ASCII,
c and the operation is being performed in place, then this procedure should
c do nothing.
c 
c N.B.: This code ASSUMES that XCHAR is implemented as INTEGER*2 and that
c both XEOS and EOS are 0.

	subroutine strpak (instr, outstr, maxch)

	integer*2 instr(*), ch, EOS
	character*1 outstr(*)
	integer maxch
	parameter (EOS=0)
	integer i

		do 10 i = 1, maxch
		    ch = instr(i)
		    outstr(i) = char (ch)
		    if (ch .eq. EOS) return
 10		continue
		outstr(maxch+1) = char (EOS)
	end
