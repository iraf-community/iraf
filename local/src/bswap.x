include	<mach.h>

define	SZ_IOBUF	2048


# BSWAP -- Copy a file, swapping the bytes.

procedure t_bswap()

bool	wordswap
int	in, out, nchars
pointer	sp, input, output, bp
int	open(), read()
bool	clgetb()

begin
	call smark (sp)
	call salloc (input,  SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (bp,     SZ_IOBUF, TY_CHAR)

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	wordswap = clgetb ("wordswap")

	in  = open (Memc[input],  READ_ONLY, BINARY_FILE)
	out = open (Memc[output],  NEW_FILE, BINARY_FILE)

	repeat {
	    nchars = read (in, Memc[bp], SZ_IOBUF)
	    if (nchars > 0) {
		if (wordswap)
		    call bswap4 (Memc[bp], 1, Memc[bp], 1, nchars * SZB_CHAR)
		else
		    call bswap2 (Memc[bp], 1, Memc[bp], 1, nchars * SZB_CHAR)
		call write (out, Memc[bp], nchars)
	    }
	} until (nchars == EOF)

	call close (in)
	call close (out)
end
