include <mach.h>
include "../export.h"


# EX_MIFF - Write the evaluated expressions as an ImageMagick MIFF format file.

procedure ex_miff (ex)

pointer	ex					#i task struct pointer

pointer	sp, hdr, cmap
int	i, j, flags
char	ncols[6]

int	strlen()

begin
	# Check to see that we have the correct number of expressions to
	# write this format.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 3 && EX_NEXPR(ex) != 1)
	    call error (7, "Invalid number of expressions for MIFF file.")
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for MIFF file.")

        # Write the header to the file.
        call smark (sp)
        call salloc (hdr, SZ_COMMAND, TY_CHAR)
        call aclrc (Memc[hdr], SZ_COMMAND)

	call sprintf (ncols, 6, "%d")
	    call pargi (EX_NCOLORS(ex))
        call sprintf (Memc[hdr], SZ_COMMAND,
	     "{\nCreated by IRAF EXPORT Task\n}\nid=ImageMagick\nclass=%s %s%s\ncolumns=%-5d  rows=%-5d\n\f\n:\n")

	    if (EX_NEXPR(ex) == 3) {
		call pargstr ("DirectClass")
		call pargstr ("")
		call pargstr ("")
	    } else {
		call pargstr ("PseudoClass")
		if (bitset (flags,OF_CMAP)) {
		    call pargstr ("colors=")
		    call pargstr (ncols)
		} else {
		    call pargstr ("")
		    call pargstr ("")
		}
	    }
                call pargi (EX_OCOLS(ex))
                call pargi (EX_OROWS(ex))

	if (mod(strlen(Memc[hdr]),2) == 1)
	    call strcat ("\n", Memc[hdr], SZ_COMMAND)
        call strpak (Memc[hdr], Memc[hdr], SZ_COMMAND)
        call write (EX_FD(ex), Memc[hdr], strlen(Memc[hdr])/SZB_CHAR)

	# Finally, evaluate the expressions and write the image.
	call ex_do_outtype (ex, "b1")
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	if (bitset (flags,OF_CMAP)) {
	    # Write out the colormap.
            call salloc (cmap, 3*CMAP_SIZE, TY_CHAR)
	    j = 1
	    do i = 0, (3*CMAP_SIZE-1), 3 {
		Memc[cmap+i+0] = CMAP(EX_CMAP(ex), EX_RED,   j)
		Memc[cmap+i+1] = CMAP(EX_CMAP(ex), EX_GREEN, j)
		Memc[cmap+i+2] = CMAP(EX_CMAP(ex), EX_BLUE,  j)
		j = j + 1
	    }
            call achtcb (Memc[cmap], Memc[cmap], (3 * CMAP_SIZE))
            call write (EX_FD(ex), Memc[cmap], ((3 * CMAP_SIZE) / SZB_CHAR))

	    call ex_no_interleave (ex) 			# write the pixels

	} else
	    call ex_px_interleave (ex)

        call sfree (sp)
end
