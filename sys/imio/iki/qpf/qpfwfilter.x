include	"qpf.h"

# QPF_WFILTER -- Record the QPIO filter used to generate an image as a series
# of FITS cards in the image header.  Note: excessively long filters are
# truncated to avoid overfilling the image header.

procedure qpf_wfilter (qpf, im)

pointer	qpf				#I QPF descriptor
pointer	im				#I image descriptor

int	nchars, nleft, index
pointer	io, sp, bp, ip, kw, strval
errchk	qpio_getfilter, impstr
int	qpio_getfilter()

begin
	io = QPF_IO(qpf)
	if (io == NULL)
	    return

	call smark (sp)
	call salloc (kw, SZ_KWNAME, TY_CHAR)
	call salloc (bp, SZ_MAXFILTER, TY_CHAR)
	call salloc (strval, SZ_BIGSTR, TY_CHAR)

	# Get the filter as as string from QPIO.
	nchars = qpio_getfilter (io, Memc[bp], SZ_MAXFILTER)

	# If the filter is longer than our string buffer, write a "..." at
	# the end of the filter to indicate that it is being truncated.

	if (nchars == SZ_MAXFILTER)
	    call strcpy ("...", Memc[bp+nchars-3], 3)

	index = 1
	ip = bp

	# Output a series of QPFILTnn cards to record the full filter.
	for (nleft = nchars;  nleft > 0;  nleft = nleft - SZ_BIGSTR) {
	    call strcpy (Memc[ip], Memc[strval], SZ_BIGSTR)
	    call sprintf (Memc[kw], SZ_KWNAME, "QPFILT%02d")
		call pargi (index)
	    iferr (call imaddf (im, Memc[kw], "c"))
		;
	    call impstr (im, Memc[kw], Memc[strval])

	    ip = ip + SZ_BIGSTR
	    index = index + 1
	}

	call sfree (sp)
end
