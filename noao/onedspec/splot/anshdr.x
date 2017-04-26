include	<time.h>
include	<fset.h>
include	<smw.h>

# ANS_HDR -- Add answer header in answer file

procedure ans_hdr (sh, newimage, key, fname1, fname2, fd1, fd2)

pointer	sh
int	newimage
int	key
char	fname1[SZ_FNAME]
char	fname2[SZ_FNAME]
int	fd1, fd2

pointer	sp, time
long	clktime()
int	key1, open()
errchk	open
data	key1/0/

begin
	# Check for valid file name
	if (fd1 == NULL && fname1[1] != EOS) {
	    fd1 = open (fname1, APPEND, TEXT_FILE)
	    call fseti (fd1, F_FLUSHNL, YES)
	}
	if (fd2 == NULL && fname2[1] != EOS) {
	    fd2 = open (fname2, APPEND, TEXT_FILE)
	    call fseti (fd2, F_FLUSHNL, YES)
	}

	# Print image name.
	if (newimage == YES) {
	    call smark (sp)
	    call salloc (time, SZ_DATE, TY_CHAR)
	    call cnvdate (clktime(0), Memc[time], SZ_DATE)

	    if (fd1 != NULL) {
		call fprintf (fd1, "\n%s [%s%s]: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (IMNAME(sh))
		    call pargstr (IMSEC(sh))
		    call pargstr (TITLE(sh))
	    }
	    if (fd2 != NULL) {
		call fprintf (fd2, "\n%s [%s%s]: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (IMNAME(sh))
		    call pargstr (IMSEC(sh))
		    call pargstr (TITLE(sh))
	    }
	    call sfree (sp)
	}

	# Print key dependent header.
	if (key != key1) {
	    if (key != 'm') {
		if (fd1 != NULL) {
		    call fprintf (fd1, "%10s%10s%10s%10s%10s%10s%10s\n")
			call pargstr ("center")
			call pargstr ("cont")
			call pargstr ("flux")
			call pargstr ("eqw")
			call pargstr ("core")
			call pargstr ("gfwhm")
			call pargstr ("lfwhm")
		    call flush (fd1)
		}
		if (fd2 != NULL) {
		    call fprintf (fd2, "%10s%10s%10s%10s%10s%10s%10s\n")
			call pargstr ("center")
			call pargstr ("cont")
			call pargstr ("flux")
			call pargstr ("eqw")
			call pargstr ("core")
			call pargstr ("gfwhm")
			call pargstr ("lfwhm")
		    call flush (fd2)
		}
	    }
	    key1 = key
	}
end
