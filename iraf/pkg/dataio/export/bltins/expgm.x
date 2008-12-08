include <mach.h>
include "../export.h"


# EX_PGM - Write the evaluated expressions as a PGM format file.

procedure ex_pgm (ex)

pointer	ex					#i task struct pointer

size_t	sz_val
long	l_val
pointer	sp, hdr
int	flags
size_t	len

int	strlen()
long	modl()

begin
	# Check to see that we have the correct number of expressions to
	# write this format.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 1 && !bitset(flags, OF_BAND))
	    call error (7, "Invalid number of expressions for PGM file.")
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for PGM file.")

	# Write the header to the file.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (hdr, sz_val, TY_CHAR)
	call aclrc (Memc[hdr], sz_val)

	l_val = 2
	call sprintf (Memc[hdr], SZ_LINE, "P5\n%-6d  %-6d\n255\n")
	    call pargl (EX_OCOLS(ex) - modl(EX_OCOLS(ex),l_val))
	    call pargl (EX_OROWS(ex))
	len = strlen (Memc[hdr])
	sz_val = SZ_LINE
	call strpak (Memc[hdr], Memc[hdr], sz_val)
	call write (EX_FD(ex), Memc[hdr], len/SZB_CHAR)
	call sfree (sp)

	# Fix the output pixel type to single bytes.
	call ex_do_outtype (ex, "b1")
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Finally, evaluate the expressions and write the image.
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND))
	    call ex_no_interleave (ex)
	else
	    call error (7, "Shouldn't be here.")
end
