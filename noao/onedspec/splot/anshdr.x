include	<time.h>
include	"../shdr.h"

# ANS_HDR -- Add answer header in answer file

procedure ans_hdr (sh, newimage, fname1, fname2, fd1, fd2)

pointer	sh
int	newimage
char	fname1[SZ_FNAME]
char	fname2[SZ_FNAME]
int	fd1, fd2

pointer	sp, time
long	clktime()
int	open()
errchk	open

begin
	# Check for valid file name
	if (fd1 == NULL && fname1[1] != EOS)
	    fd1 = open (fname1, APPEND, TEXT_FILE)
	if (fd2 == NULL && fname2[1] != EOS)
	    fd2 = open (fname2, APPEND, TEXT_FILE)

	# Compare current image name and last to see if a new
	# image stamp and header is needed.
	if (newimage == YES) {
	    call smark (sp)
	    call salloc (time, SZ_DATE, TY_CHAR)
	    call cnvdate (clktime(0), Memc[time], SZ_DATE)

	    if (fd1 != NULL) {
		if (NDIM(sh) > 2) {
		    call fprintf (fd1, "\n%s [%s[*,%d,%d]]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargi (INDEX1(sh))
			call pargi (INDEX2(sh))
			call pargstr (TITLE(sh))
		} else if (NDIM(sh) > 1) {
		    call fprintf (fd1, "\n%s [%s[*,%d]]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargi (INDEX1(sh))
			call pargstr (TITLE(sh))
		} else {
		    call fprintf (fd1, "\n%s [%s]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargstr (TITLE(sh))
		}
		call fprintf (fd1, "%10s%10s%10s%10s%10s%10s%10s\n")
		    call pargstr ("center")
		    call pargstr ("cont")
		    call pargstr ("flux")
		    call pargstr ("eqw")
		    call pargstr ("core")
		    call pargstr ("sigma")
		    call pargstr ("fwhm")
	    }
	    if (fd2 != NULL) {
		if (NDIM(sh) > 2) {
		    call fprintf (fd2, "\n%s [%s[*,%d,%d]]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargi (INDEX1(sh))
			call pargi (INDEX2(sh))
			call pargstr (TITLE(sh))
		} else if (NDIM(sh) > 1) {
		    call fprintf (fd2, "\n%s [%s[*,%d]]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargi (INDEX1(sh))
			call pargstr (TITLE(sh))
		} else {
		    call fprintf (fd2, "\n%s [%s]: %s\n")
			call pargstr (Memc[time])
			call pargstr (SPECTRUM(sh))
			call pargstr (TITLE(sh))
		}
		call fprintf (fd2, "%10s%10s%10s%10s%10s%10s%10s\n")
		    call pargstr ("center")
		    call pargstr ("cont")
		    call pargstr ("flux")
		    call pargstr ("eqw")
		    call pargstr ("core")
		    call pargstr ("sigma")
		    call pargstr ("fwhm")
	    }

	    call sfree (sp)
	}
end
