include <mach.h>
include "../export.h"


# EX_PPM - Write the evaluated expressions as a PPM format file.

procedure ex_ppm (ex)

pointer	ex					#i task struct pointer

pointer	sp, hdr
int	len, flags

int	strlen()

begin
	# Check to see that we have the correct number of expressions to
	# write this format.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 3)
	    call error (7, "Invalid number of expressions for PPM file.")
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for PPM file.")

        # Write the header to the file.
        call smark (sp)
        call salloc (hdr, SZ_LINE, TY_CHAR)
        call aclrc (Memc[hdr], SZ_LINE)

	# If we have an odd number of pixels we can't correctly write the
	# last column to the file, so truncate the column in the output image.
        if (mod (EX_NCOLS(ex),2) == 1)
            EX_OCOLS(ex) = EX_OCOLS(ex) - 1

        call sprintf (Memc[hdr], SZ_LINE, "P6\n%-6d  %-6d\n255\n")
            call pargi (EX_OCOLS(ex))
            call pargi (EX_OROWS(ex))
	len = strlen (Memc[hdr])
        call strpak (Memc[hdr], Memc[hdr], SZ_LINE)
        call write (EX_FD(ex), Memc[hdr], len/SZB_CHAR)
        call sfree (sp)

	# Fix the output pixel type to single bytes.
	call ex_do_outtype (ex, "b1")
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Finally, evaluate the expressions and write the image.
	call ex_px_interleave (ex)
end
