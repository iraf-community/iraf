c F77PAK -- Convert an SPP string into a Fortran 77 string.
c
        subroutine f77pak (sppstr, f77str, maxch)
c
        integer*2 sppstr(*)
        character*(*) f77str
        integer maxch
        integer i, ch, last, maxout, EOS
        parameter (EOS=0)
c
        maxout = min (maxch, len(f77str))
c
c       # Unpack the EOS delimited SPP string.
	last = maxout
        do 10 i = 1, maxout
            ch = sppstr(i)
            if (ch .eq. EOS) then
		last = i - 1
		goto 20
	    endif
            f77str(i:i) = char (ch)
 10     continue
 20     continue
c
c       # Pad on the right with blanks.
	if (last .gt. maxch) last = maxch
	if (last .le. 0) then
	    f77str = ' '
	else
	    f77str = f77str(1:last)
	endif
        end
