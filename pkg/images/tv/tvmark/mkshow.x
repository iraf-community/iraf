include "tvmark.h"

# MK_SHOW -- Procedure to show the immark parameters

procedure mk_show (mk)

pointer	mk		# pointer to the immark structure

pointer	sp, str
bool	itob()
int	mk_stati()
real	mk_statr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Print a blank line.
	call printf ("\n")

	# Print the frame info.
	call printf ("%s: %d  %s: %s\n")
	    call pargstr (KY_FRAME)
	    call pargi (mk_stati (mk, FRAME))
	    call pargstr (KY_COORDS)
	    call mk_stats (mk, COORDS, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])

	# Print the output info.
	call printf ("    %s: %s  %s: %s  %s: %b\n")
	    call pargstr (KY_OUTIMAGE)
	    call mk_stats (mk, OUTIMAGE, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call mk_stats (mk, LOGFILE, Memc[str], SZ_FNAME)
	    call pargstr (KY_LOGFILE)
	    call pargstr (Memc[str])
	    call pargstr (KY_AUTOLOG)
	    call pargb (itob (mk_stati (mk, AUTOLOG)))
	    
	# Print the deletions file info.
	call printf ("    %s: %s  %s: %g\n")
	    call pargstr (KY_DELETIONS)
	    call mk_stats (mk, DELETIONS, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call pargstr (KY_TOLERANCE)
	    call pargr (mk_statr (mk, TOLERANCE))

	# Print the font info.
	call printf ("    %s: %s  %s: %d\n")
	    call pargstr (KY_FONT)
	    call mk_stats (mk, FONT, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call pargstr (KY_GRAYLEVEL)
	    call pargi (mk_stati (mk, GRAYLEVEL))

	# Print the mark type info.
	call printf ("    %s: %s  ")
	    call pargstr (KY_MARK)
	call mk_stats (mk, MARK, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])

	call printf ("%s: %s  ")
	    call pargstr (KY_CIRCLES)
	call mk_stats (mk, CSTRING, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])

	call printf ("%s: %s %g\n")
	    call pargstr (KY_RECTANGLE)
	call mk_stats (mk, RSTRING, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call pargr (mk_statr (mk, RATIO))

	call printf ("    %s: %d  %s: %d\n")
	    call pargstr (KY_SZPOINT)
	    call pargi (2 * mk_stati (mk, SZPOINT) + 1)
	    call pargstr (KY_SIZE)
	    call pargi (mk_stati (mk, SIZE))

	call printf ("    %s: %b  ")
	    call pargstr (KY_LABEL)
	    call pargb (itob (mk_stati (mk, LABEL)))
	call printf ("%s: %b  ")
	    call pargstr (KY_NUMBER)
	    call pargb (itob (mk_stati (mk, NUMBER)))
	call printf ("  %s: %d  %s: %d\n")
	    call pargstr (KY_NXOFFSET)
	    call pargi (mk_stati (mk, NXOFFSET))
	    call pargstr (KY_NYOFFSET)
	    call pargi (mk_stati (mk, NYOFFSET))

	# Print a blank line.
	call printf ("\n")

	call sfree (sp)
end
