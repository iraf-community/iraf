c F77UPK -- Convert a Fortran 77 string into an SPP string.  Unpack
c each Fortran character into an SPP char and trim the blank padding
c at the right.
c
        subroutine f77upk (f77str, sppstr, maxch)
c
        character*(*)	f77str
        integer*2	sppstr(*)
        integer		maxch
        integer		lastch, nchars, i
        integer		EOS, BLANK
        parameter	(EOS=0, BLANK=32)
c
c -- Unpack string.
 	nchars = min (maxch, len(f77str))
	lastch = 0
        do 10 i = 1, nchars
            sppstr(i) = ichar (f77str(i:i))
	    if (sppstr(i) .gt. BLANK) lastch = i
 10     continue
c
c -- Add EOS delimiter to SPP string, trimming blank padding at right.
	if (lastch .gt. maxch) lastch = maxch
	sppstr(lastch+1) = EOS
c
        end
