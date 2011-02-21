include	<error.h>
include	<imhdr.h>
include	<smw.h>

define	LEN_SAP		52		# Length of structure
define	LEN_SAPTITLE	79		# Length of title

define	AP	Memi[$1]		# Aperture number
define	BEAM	Memi[$1+1]		# Beam number
define	DTYPE	Memi[$1+2]		# Dispersion type
define	W1	Memd[P2D($1+4)]		# Starting wavelength
define	DW	Memd[P2D($1+6)]		# Wavelength per pixel
define	Z	Memd[P2D($1+8)]		# Doppler factor
define	APLOW	Memr[P2R($1+10)]	# Low aperture
define	APHIGH	Memr[P2R($1+11)]	# High aperture
define	TITLE	Memc[P2C($1+12)]	# Title
 
 
# T_SAPERTURES -- Set aperture beam numbers and titles.
 
procedure t_sapertures()
 
int	list			# Input list
bool	wcsreset		# Reset WCS?
bool	verbose			# Verbose?
pointer	saps			# Pointer to array of aperture structures
 
int	imtopenp(), imtgetim()
bool	clgetb()
pointer	sp, input, ranges, tmp, im, mw, rng_open(), immap(), smw_openim()
errchk	sap_gids, immap, smw_openim
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)

	list = imtopenp ("input")
	wcsreset = clgetb ("wcsreset")
	verbose = clgetb ("verbose")
	call clgstr ("apertures", Memc[input], SZ_FNAME)
	iferr (ranges = rng_open (Memc[input], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")

	call sap_gids (saps, wcsreset, verbose)
	
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    im = NULL
	    mw = NULL
	    iferr {
	        tmp = immap (Memc[input], READ_WRITE, 0); im = tmp
		tmp = smw_openim (im); mw = tmp
		if (SMW_FORMAT(mw) != SMW_ES && SMW_FORMAT(mw) != SMW_MS)
		    call error (1, "Wrong spectrum format")
		call sap_ms (im, mw, Memc[input], ranges, saps, verbose)
	    } then
		call erract (EA_WARN)

	    if (mw != NULL) {
		call smw_saveim (mw, im)
		call smw_close (mw)
	    }
	    if (im != NULL)
		call imunmap (im)
	}

	call rng_close (ranges)
	call imtclose (list)
	call sap_fids (saps)
	call sfree (sp)
end


# SAP_MS -- Set aperture information

procedure sap_ms (im, mw, input, ranges, saps, verbose)

pointer	im			# IMIO pointer
pointer	mw			# SMW pointer
char	input[ARB]		# Image name
pointer	ranges			# Aperture range list
pointer	saps			# Pointer to array of aperture structures
bool	verbose			# Verbose?

int	i, naps, ap, beam, dtype, nw, obeam, odtype
double	w1, dw, z, ow1, odw, oz
real	aplow[2], aphigh[2], oaplow[2], oaphigh[2]
bool	newtitle, streq(), rng_elementi()
pointer	sp, title, coeff, sap

begin
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	coeff = NULL
 
	# Go through each spectrum and change the selected apertures.
	naps = -1
	do i = 1, SMW_NSPEC(mw) {
	    # Get aperture info
	    iferr (call smw_gwattrs (mw, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff))
		break

	    # Check if aperture is to be changed
	    if (!rng_elementi (ranges, ap))
		next

	    # Check for aperture info
	    for (sap = saps; Memi[sap] != NULL; sap = sap + 1)
		if (ap == AP(Memi[sap]))
		    break
	    if (Memi[sap] == NULL) {
		for (sap = saps; Memi[sap] != NULL; sap = sap + 1)
		    if (IS_INDEFI (AP(Memi[sap])))
			break
	    }
	    if (Memi[sap] == NULL)
		next

	    # Get aperture title
	    call smw_gapid (mw, i, 1, Memc[title], SZ_LINE)

	    # Set new aperture values
	    sap = Memi[sap]
	    obeam = BEAM(sap)
	    odtype = DTYPE(sap)
	    ow1 = W1(sap)
	    odw = DW(sap)
	    oz = Z(sap)
	    oaplow[1] = APLOW(sap)
	    oaphigh[1] = APHIGH(sap)
	    oaplow[2] = INDEF
	    oaphigh[2] = INDEF

	    if (IS_INDEFI (obeam))
		obeam = beam
	    if (IS_INDEFI (odtype))
		odtype = dtype
	    else
		odtype = max (-1, min (1, odtype))
	    if (IS_INDEFD (ow1))
		ow1 = w1
	    if (IS_INDEFD (odw))
		odw = dw
	    if (IS_INDEFD (oz))
		oz = z
	    if (IS_INDEF (oaplow[1]))
		oaplow[1] = aplow[1]
	    if (IS_INDEF (oaphigh[1]))
		oaphigh[1] = aphigh[1]
	    if (streq (TITLE(sap), "INDEF") || TITLE(sap) == EOS)
		newtitle = false
	    else
		newtitle = !streq (TITLE(sap), Memc[title])

	    if (dtype == 2 && odtype != 2)
		Memc[coeff] = EOS

	    # Make change if needed
	    if (obeam!=beam || odtype!=dtype || ow1!=w1 || odw !=dw || oz!=z ||
		oaplow[1]!=aplow[1] || oaphigh[1]!=aphigh[1] || newtitle) {
		call smw_swattrs (mw, i, 1, ap, obeam, odtype, ow1, odw, nw,
		    oz, oaplow, oaphigh, Memc[coeff])
		if (newtitle)
		    call smw_sapid (mw, i, 1, TITLE(sap))
		naps = naps + 1

		# Make record
		if (verbose) {
		    if (naps == 0) {
			call printf ("%s:\n")
			    call pargstr (input)
			naps = naps + 1
		    }
		    call printf ("  Aperture %d:\n")
			call pargi (ap)
		    if (obeam != beam) {
			call printf ("    beam %d --> %d\n")
			    call pargi (beam)
			    call pargi (obeam)
		    }
		    if (odtype != dtype) {
			call printf ("    dtype %d --> %d\n")
			    call pargi (dtype)
			    call pargi (odtype)
		    }
		    if (ow1 != w1) {
			call printf ("    w1 %g --> %g\n")
			    call pargd (w1)
			    call pargd (ow1)
		    }
		    if (odw != dw) {
			call printf ("    dw %g --> %g\n")
			    call pargd (dw)
			    call pargd (odw)
		    }
		    if (oz != z) {
			call printf ("    z %g --> %g\n")
			    call pargd (z)
			    call pargd (oz)
		    }
		    if (oaplow[1] != aplow[1]) {
			call printf ("    aplow %g --> %g\n")
			    call pargr (aplow[1])
			    call pargr (oaplow[1])
		    }
		    if (oaphigh[1] != aphigh[1]) {
			call printf ("    aphigh %g --> %g\n")
			    call pargr (aphigh[1])
			    call pargr (oaphigh[1])
		    }
		    if (newtitle) {
			call printf ("    apid %s --> %s\n")
			    call pargstr (Memc[title])
			    call pargstr (TITLE(sap))
		    }
		}
	    }
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SA_GIDS -- Get user aperture ID's.

procedure sap_gids (saps, wcsreset, verbose)

pointer	saps			# Pointer to array of aperture structures
bool	wcsreset		# Reset WCS?
bool	verbose			# Verbose (negative beam warning)?
pointer	sap

int	naps, ap, beam, fd, nalloc
double	ra, dec
pointer	sp, str, key, im, list

real	clgetr()
double	clgetd()
int	nowhite(), open(), fscan(), nscan(), clgeti()
pointer	immap(), imofnlu(), imgnfn()
errchk	open

begin

	# If resetting ignore the APIDTABLE and the task parameters.
	if (wcsreset) {
	    call malloc (saps, 2, TY_POINTER)
	    call malloc (Memi[saps], LEN_SAP, TY_STRUCT)
	    Memi[saps+1] = NULL

	    sap = Memi[saps]
	    AP(sap) = INDEFI
	    BEAM(sap) = INDEFI
	    DTYPE(sap) = -1
	    W1(sap) = 1.
	    DW(sap) = 1.
	    Z(sap) = 0.
	    APLOW(sap) = INDEF
	    APHIGH(sap) = INDEF
	    TITLE(sap) = EOS
	    return
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call clgstr ("apidtable", Memc[str], SZ_LINE)

	# Set parameters from an APIDTABLE if given.
	naps = 0
	if (nowhite (Memc[str], Memc[str], SZ_LINE) > 0) {
	    iferr {
		# Read aperture information from an image.
		ifnoerr (im = immap (Memc[str], READ_ONLY, 0)) {
		    list = imofnlu (im, "SLFIB[0-9]*")
		    while (imgnfn (list, Memc[key], SZ_FNAME) != EOF) {
			call imgstr (im, Memc[key], Memc[str], SZ_LINE)
			call sscan (Memc[str])
			call gargi (ap)
			call gargi (beam)
			if (nscan() < 2)
			    next
			if (!IS_INDEFI(beam) && beam < 0 && verbose) {
			    call eprintf (
			    "Negative beam number for aperture %d ignored.\n") 
				call pargi (ap)
			    beam = INDEFI
			}
			if (naps == 0) {
			    nalloc = 50
			    call malloc (saps, nalloc, TY_POINTER)
			} else if (naps == nalloc) {
			    nalloc = nalloc + 50
			    call realloc (saps, nalloc, TY_POINTER)
			}
			call malloc (Memi[saps+naps], LEN_SAP, TY_STRUCT)

			sap = Memi[saps+naps]
			AP(sap) = ap
			BEAM(sap) = beam
			call gargd (ra)
			call gargd (dec)
                        if (nscan() != 4) {
                            call reset_scan ()
                            call gargi (ap)
                            call gargi (beam)
			    call gargstr (TITLE(sap), LEN_SAPTITLE)
                        } else {
                            Memc[str] = EOS
                            call gargstr (Memc[str], SZ_LINE)
                            call xt_stripwhite (Memc[str])
                            if (Memc[str] == EOS) {
                                call sprintf (TITLE(sap), LEN_SAPTITLE,
                                    "(%.2h %.2h)")
                                    call pargd (ra)
                                    call pargd (dec)
                            } else {
                                call sprintf (TITLE(sap), LEN_SAPTITLE,
                                    "%s (%.2h %.2h)")
                                    call pargstr (Memc[str])
                                    call pargd (ra)
                                    call pargd (dec)
                            }
                        }
			DTYPE(sap) = INDEFI
			W1(sap) = INDEFD
			DW(sap) = INDEFD
			Z(sap) = INDEFD
			APLOW(sap) = INDEF
			APHIGH(sap) = INDEF
			call xt_stripwhite (TITLE(sap))
			naps = naps + 1
		    }	
		    call imcfnl (list)
		    call imunmap (im)

		# Read aperture information from a file.
		} else {
		    fd = open (Memc[str], READ_ONLY, TEXT_FILE)
		    while (fscan (fd) != EOF) {
			call gargi (ap)
			call gargi (beam)
			if (nscan() < 2)
			    next
			if (!IS_INDEFI(beam) && beam < 0 && verbose) {
			    call eprintf (
			    "Negative beam number for aperture %d ignored.\n") 
				call pargi (ap)
			    beam = INDEFI
			}
			if (naps == 0) {
			    nalloc = 50
			    call malloc (saps, nalloc, TY_POINTER)
			} else if (naps == nalloc) {
			    nalloc = nalloc + 50
			    call realloc (saps, nalloc, TY_POINTER)
			}
			call malloc (Memi[saps+naps], LEN_SAP, TY_STRUCT)

			sap = Memi[saps+naps]
			AP(sap) = ap
			BEAM(sap) = beam
			call gargi (DTYPE(sap))
			call gargd (W1(sap))
			call gargd (DW(sap))
			call gargd (Z(sap))
			call gargr (APLOW(sap))
			call gargr (APHIGH(sap))
			call gargstr (TITLE(sap), LEN_SAPTITLE)
			if (nscan() < 9) {
			    call reset_scan()
			    call gargi (AP(sap))
			    call gargi (BEAM(sap))
			    if (!IS_INDEFI(BEAM(sap)) && BEAM(sap) < 0)
				BEAM(sap) = INDEFI
			    call gargstr (TITLE(sap), LEN_SAPTITLE)
			    DTYPE(sap) = INDEFI
			    W1(sap) = INDEFD
			    DW(sap) = INDEFD
			    Z(sap) = INDEFD
			    APLOW(sap) = INDEF
			    APHIGH(sap) = INDEF
			}
			call xt_stripwhite (TITLE(sap))
			naps = naps + 1
		    }	
		    call close (fd)
		}
	    } then
		call erract (EA_WARN)
	}

	# Set remaining default parameters and the list terminator.
	call realloc (saps, naps+2, TY_INT)
	call malloc (Memi[saps+naps], LEN_SAP, TY_STRUCT)
	Memi[saps+naps+1] = NULL

	sap = Memi[saps+naps]
	AP(sap) = INDEFI
	BEAM(sap) = clgeti ("beam")
	if (!IS_INDEFI(BEAM(sap)) && BEAM(sap) < 0 && verbose) {
	    call eprintf (
		"Negative default beam number ignored.\n") 
	    BEAM(sap) = INDEFI
	}
	DTYPE(sap) = clgeti ("dtype")
	W1(sap) = clgetd ("w1")
	DW(sap) = clgetd ("dw")
	Z(sap) = clgetd ("z")
	APLOW(sap) = clgetr ("aplow")
	APHIGH(sap) = clgetr ("aphigh")
	call clgstr ("title", TITLE(sap), LEN_SAPTITLE)

	call sfree (sp)
end


procedure sap_fids (saps)

pointer	saps			# Pointer to array of aperture structures
pointer	sap

begin
	for (sap=saps; Memi[sap] != NULL; sap = sap + 1)
	    call mfree (Memi[sap], TY_STRUCT)
	call mfree (saps, TY_POINTER)
end
