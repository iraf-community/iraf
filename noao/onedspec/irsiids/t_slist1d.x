include	<error.h>
include	<imhdr.h>
include	<fset.h>
include <smw.h>


# SLIST1D --  Lists header information from IIDS/IRS format header
# This is the original T_SLIST.

procedure t_slist1d ()

int	root
int	long_header
pointer	sp, image, im, mw, sh, ptr
int	i, nl, df, sm, qf, qd, bs, co

int	btoi(), imtgetim(), imgeti()
bool	clgetb()
pointer	imtopenp(), immap(), smw_openim()
errchk	immap, smw_openim, shdr_open

begin
	call smark (sp)
	call salloc (image, SZ_LINE, TY_CHAR)

	# Parameters
	root = imtopenp ("input")
	call clgstr ("records", Memc[image], SZ_LINE)
	call odr_openp (root, Memc[image])
	long_header = btoi (clgetb ("long_header"))

	# Initialize
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Loop over all input images by subsets
	while (imtgetim (root, Memc[image], SZ_FNAME) != EOF) {

	    # Open image
	    iferr {
		im = NULL
		mw = NULL
		ptr = immap (Memc[image], READ_ONLY, 0); im = ptr
		ptr = smw_openim (im); mw = ptr
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

	    nl = IM_LEN(im,2)
	    do i = 1, nl {
		call shdr_open (im, mw, i, 1, INDEFI, SHHDR, sh)

		if (long_header == YES) {
		    call printf ("[%s] %4dpts %s\n")
			call pargstr (IMNAME(sh))
			call pargi (SN(sh))
			call pargstr (TITLE(sh))

		    if (OFLAG(sh) == 1) {
			call printf ("oflag = OBJECT, beam_number = %d")
			    call pargi (BEAM(sh))
		    } else if (OFLAG (sh) == 0) {
			call printf ("oflag = SKY,    beam_number = %d")
			    call pargi (BEAM(sh))
		    }
		    call printf (",\n")

		    iferr (df = imgeti (im, "DF-FLAG"))
			df = -1
		    iferr (sm = imgeti (im, "SM-FLAG"))
			sm = -1
		    iferr (qf = imgeti (im, "QF-FLAG"))
			qf = -1
		    iferr (qd = imgeti (im, "QD-FLAG"))
			qd = -1
		    iferr (bs = imgeti (im, "BS-FLAG"))
			bs = -1
		    iferr (co = imgeti (im, "CO-FLAG"))
			co = -1

		    # Airmass may not be in header. It could be computed if
		    # if the observatory latitude were available.

		    call printf ("airmass = %5.3f,%25tW0 = %0.3f,")
			call pargr (AM(sh))
			call pargr (W0(sh))
		    call printf ("   WPC = %0.5g,      ITM = %.2f,\n")
			call pargr (WP(sh))
			call pargr (IT(sh))
		    call printf ("NP1 = %d, NP2 = %d,")
			call pargi (NP1(sh))
			call pargi (NP2(sh))
		    call printf ("    UT = %0.1h,   ST = %0.1h,\n")
			call pargr (UT(sh))
			call pargr (ST(sh))
		    call printf ("HA = %0.2h,")
			call pargr (HA(sh))
		    call printf ("        RA = %0.2h,   DEC = %0.1h,\n")
			call pargr (RA(sh))
			call pargr (DEC(sh))
		    call printf (
			"df = %d, sm = %d, qf = %d, dc = %d, qd = %d, ") 
			call pargi (df)
			call pargi (sm)
			call pargi (qf)
			call pargi (DC(sh))
			call pargi (qd)
		    call printf ("ex = %d, bs = %d, ca = %d, co = %d")
			call pargi (EC(sh))
			call pargi (bs)
			call pargi (FC(sh))
			call pargi (co)

		    call printf ("\n\n")
		} else {
		    if (nl == 1) {
			call printf ("[%s]:%s %4ds %4dpts   %s\n")
			    call pargstr (IMNAME(sh))
			    if (OFLAG(sh) == 1)
				call pargstr ("o")
			    else
				call pargstr ("s")
			    call pargr (IT(sh))
			    call pargi (SN(sh))
			    call pargstr (TITLE(sh))
		    } else {
			call printf ("[%s]:%s %6.2fs %4dpts %dspectra   %s\n")
			    call pargstr (IMNAME(sh))
			    if (OFLAG(sh) == 1)
				call pargstr ("o")
			    else
				call pargstr ("s")
			    call pargr (IT(sh))
			    call pargi (SN(sh))
			    call pargi (nl)
			    call pargstr (TITLE(sh))
		    }
		    break
		}
	    }

	    call smw_close (mw)
	    if (sh != NULL)
		MW(sh) = NULL
	    call imunmap (im)
	}

	# Null out record string to avoid learn mode
	call clpstr ("records", "")

	# Free space
	call shdr_close (sh)
	call imtclose (root)
	call sfree (sp)
end
