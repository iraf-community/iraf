include	<time.h>

# ANS_SAVE -- Save answer string in answer file

procedure ans_save (image, nline, title, ans, ans_file, newimage)

char	image[SZ_FNAME]
int	nline
char	title[SZ_LINE]
char	ans[SZ_LINE]
char	ans_file[SZ_FNAME]
int	newimage

pointer	sp, time
long	clktime()
int	fd, open()
errchk	open

begin
	# Check for valid file name
	if (ans_file[1] == EOS)
	    return

	# Open file for append to allow multiple cases
	fd = open (ans_file, APPEND, TEXT_FILE)

	# Compare current image name and last to see if a new
	# image stamp and header is needed.
	if (newimage == YES) {
	    call smark (sp)
	    call salloc (time, SZ_DATE, TY_CHAR)
	    call cnvdate (clktime(0), Memc[time], SZ_DATE)

	    if (nline > 0) {
	        call fprintf (fd, "\n%s [%s[*,%d]]: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (image)
		    call pargi (nline)
		    call pargstr (title)
	    } else {
	        call fprintf (fd, "\n%s [%s]: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (image)
		    call pargstr (title)
	    }
	    call fprintf (fd, "%10s%10s%10s%10s%10s%10s%10s\n")
	        call pargstr ("center")
	        call pargstr ("cont")
		call pargstr ("flux")
	        call pargstr ("eqw")
	        call pargstr ("core")
	        call pargstr ("sigma")
	        call pargstr ("fwhm")

	    call sfree (sp)
	}

	# Put out answer line
	call fprintf (fd, "%s")
	    call pargstr (ans)

	call close (fd)
end
