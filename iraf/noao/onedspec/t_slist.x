include	<error.h>
include	<imhdr.h>
include	<fset.h>
include	<smw.h>


# T_SLIST --  Lists header information from MULTISPEC format header

procedure t_slist ()

int	list			# Input list
pointer	aps			# Aperture range list
int	long_header		# Long header?

int	i
pointer	sp, image, im, mw, sh, ptr

bool	clgetb(), rng_elementi()
int	 imtopenp(), imtgetim(), btoi()
pointer	rng_open(), immap(), smw_openim()
errchk	immap, smw_openim, shdr_open

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Parameters
	list = imtopenp ("images")
	call clgstr ("apertures", Memc[image], SZ_LINE)
	long_header = btoi (clgetb ("long_header"))

	# Initialize
	call fseti (STDOUT, F_FLUSHNL, YES)
	iferr (aps = rng_open (Memc[image], INDEF, INDEF, INDEF))
	    call error (0, "Bad range specification")

	# Loop over all input images.
	while (imtgetim (list, Memc[image], SZ_LINE) != EOF) {
	    iferr {
		im = NULL
		mw = NULL
		ptr = immap (Memc[image], READ_ONLY, 0); im = ptr
		ptr = smw_openim (im); mw = ptr
		#if (SMW_FORMAT(mw) != SMW_ES && SMW_FORMAT(mw) != SMW_MS)
		#    call error (1, "Invalid spectrum format")
		call shdr_open (im, mw, 1, 1, INDEFI, SHHDR, sh)
	    } then {
		if (mw != NULL) {
		    call smw_close (mw)
		    if (sh != NULL)
			MW(sh) = NULL
		}
		if (im != NULL)
		    call imunmap (im)
		call erract (EA_WARN)
		next
	    }

	    if (long_header == YES) {
		call printf ("%s: %s\n")
		    call pargstr (IMNAME(sh))
		    call pargstr (IM_TITLE(im))
		call printf (
		    "    EXPTIME = %.2f%24tUT = %0.1h%44tST = %0.1h\n")
		    call pargr (IT(sh))
		    call pargr (UT(sh))
		    call pargr (ST(sh))
		call printf (
	"    RA = %0.2h%24tDEC = %0.1h%44tHA = %0.2h%64tAIRMASS = %5.3f\n")
		    call pargr (RA(sh))
		    call pargr (DEC(sh))
		    call pargr (HA(sh))
		    call pargr (AM(sh))
	    }
	    do i = 1, IM_LEN(im, SMW_LAXIS(MW(sh),2)) {
		call shdr_open (im, mw, i, 1, INDEFI, SHHDR, sh)
		if (!rng_elementi (aps, AP(sh)))
		    next
		if (long_header == NO)
		    call printf (IMNAME(sh))
		else
		    call printf ("   ")
		call printf (" %d %d %d %d %g %g %d %s\n")
			call pargi (i)
			call pargi (AP(sh))
			call pargi (BEAM(sh))
			call pargi (DC(sh))
			call pargr (W0(sh))
			call pargr (WP(sh))
			call pargi (SN(sh))
			call pargstr (TITLE(sh))
	    }

	    call smw_close (MW(sh))
	    if (sh != NULL)
		MW(sh) = NULL
	    call imunmap (im)
	}

	# Free space
	call shdr_close (sh)
	call rng_close (aps)
	call imtclose (list)
	call sfree (sp)
end
