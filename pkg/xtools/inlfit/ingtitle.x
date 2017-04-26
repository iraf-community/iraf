include <pkg/gtools.h>

# ING_TITLE -- Write out the time stamp and the title of the current fit.

procedure ing_title (in, file, gt)

pointer	in		# pointer to the inlfit structure (not used yet)
char	file[ARB]	# arbitrary file name
pointer	gt		# pointer to the gtools structure

int	fd, sfd
pointer	sp, str
int	open(), stropen(), fscan()
long	clktime()

begin
	if (file[1] == EOS)
	    return
	fd = open (file, APPEND, TEXT_FILE)

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Put time stamp in.
	call cnvtime (clktime(0), Memc[str], SZ_LINE)
	call fprintf (fd, "\n#%s\n")
	    call pargstr (Memc[str])

	# Print plot title.
	call gt_gets (gt, GTTITLE, Memc[str], SZ_LINE)
	sfd = stropen (Memc[str], SZ_LINE, READ_ONLY)
	while (fscan (sfd) != EOF) {
	    call gargstr (Memc[str], SZ_LINE)
	    call fprintf (fd, "#%s\n")
	        call pargstr (Memc[str])
	} 
	call fprintf (fd, "\n")
	call strclose (sfd)

	# Print fit units.
	#call gt_gets (gt, GTYUNITS, Memc[str], SZ_LINE)
	#if (Memc[str] != EOS) {
	    #call fprintf (fd, "fit_units    %s\n")
	        #call pargstr (Memc[str])
	#}

	call sfree (sp)
	call close (fd)
end
