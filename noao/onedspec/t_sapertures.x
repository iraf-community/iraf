include	<error.h>
include	<imhdr.h>
 
 
# T_SAPERTURES -- Set aperture beam numbers and titles.
 
procedure t_sapertures()
 
int	list			# Input list
pointer	aps			# Aperture numbers
pointer	beams			# Beam numbers
pointer	titles			# Aperture titles
int	naps			# Number of apertures
bool	verbose			# Verbose?
 
int	i, imtopenp(), imtgetim(), decode_ranges()
bool	clgetb()
pointer	sp, input, ranges, tmp, im, mw, immap(), smw_openim()
errchk	immap, smw_openim
 
begin
	call sap_gids (aps, beams, titles, naps)
	if (naps == 0)
	    return

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (ranges, 300, TY_INT)

	list = imtopenp ("input")
	verbose = clgetb ("verbose")
	call clgstr ("apertures", Memc[input], SZ_FNAME)

	if (decode_ranges (Memc[input], Memi[ranges], 100, i) == ERR)
	    call error (0, "Bad aperture list")
	
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    im = NULL
	    mw = NULL
	    iferr {
	        tmp = immap (Memc[input], READ_WRITE, 0); im = tmp
		tmp = smw_openim (im); mw = tmp
		call sap_ms (im, mw, Memc[input], Memi[ranges], Memi[aps],
		    Memi[beams], Memi[titles], naps, verbose)
	    } then
		call erract (EA_WARN)

	    if (mw != NULL) {
		call smw_saveim (mw, im)
		call mw_close (mw)
	    }
	    if (im != NULL)
		call imunmap (im)
	}

	call imtclose (list)
	call sap_fids (aps, beams, titles, naps)
	call sfree (sp)
end


# SAP_MS -- Set aperture information

procedure sap_ms (im, mw, input, ranges, aps, beams, titles, naps, verbose)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer
char	input[ARB]		# Image name
int	ranges[ARB]		# Aperture range list
int	aps[naps]		# Aperture numbers
int	beams[naps]		# Beam numbers
pointer	titles[naps]		# Titles (pointers)
int	naps			# Number of apertures
bool	verbose			# Verbose?

int	i, j, k, ap, beam, dtype, nw
double	w1, dw, z, aplow, aphigh
pointer	sp, key, title, coeff

bool	is_in_range(), strne()

begin
	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	coeff = NULL
 
	j = 0
	for (k=1;;k=k+1) {
	    iferr (call shdr_gwattrs (mw, k, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff))
		break

	    if (!is_in_range (ranges, ap))
		next

	    for (i = 1; i <= naps && aps[i] != ap; i = i + 1)
		;
	    if (i <= naps) {
		if (beam != beams[i]) {
		    call shdr_swattrs (mw, k, ap, beams[i], dtype, w1, dw, nw,
			z, aplow, aphigh, Memc[coeff])
		    j = j + 1
		}

		call sprintf (Memc[key], SZ_FNAME, "APID%d")
		    call pargi (k)
		iferr (call imgstr (im, Memc[key], Memc[title], SZ_LINE))
		    Memc[title] = EOS
		if (titles[i] != NULL &&
		    strne (Memc[title], Memc[titles[i]])) {
		    call imastr (im, Memc[key], Memc[titles[i]])
		    j = j + 1
		}

		if (verbose && j != 0) {
		    if (j < 3) {
			call printf ("%s:\n")
			    call pargstr (input)
			j = 3
		    }
		    if (beam != beams[i]) {
			call printf ("  Aperture %d: beam %d --> beam %d\n")
			    call pargi (ap)
			    call pargi (beam)
			    call pargi (beams[i])
		    }
		    if (titles[i] != NULL &&
			strne (Memc[title], Memc[titles[i]])) {
			call printf ("  Aperture %d: %s --> %s\n")
			    call pargi (ap)
			    call pargstr (Memc[title])
			    call pargstr (Memc[titles[i]])
		    }
		}
	    }
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SA_GIDS -- Get user aperture ID's.

procedure sap_gids (aps, beams, titles, naps)

pointer	aps			# Aperture numbers
pointer	beams			# Beam numbers
pointer	titles			# Titles
int	naps			# Number of apertures

int	ap, beam, fd, nalloc, nowhite(), open(), fscan(), nscan()
pointer	sp, str
errchk	open

begin
	call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)

	naps = 0
	nalloc = 0

	call clgstr ("apidtable", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_LINE) > 0) {
	    iferr {
		fd = open (Memc[str], READ_ONLY, TEXT_FILE)
		while (fscan (fd) != EOF) {
		    call gargi (ap)
		    if (nscan() == 0)
			next
		    if (nalloc == 0) {
			nalloc = 50
			call malloc (aps, nalloc, TY_INT)
			call malloc (beams, nalloc, TY_INT)
			call malloc (titles, nalloc, TY_POINTER)
		    } else if (naps == nalloc) {
			nalloc = nalloc + 50
			call realloc (aps, nalloc, TY_INT)
			call realloc (beams, nalloc, TY_INT)
			call realloc (titles, nalloc, TY_POINTER)
		    }
		    Memi[aps+naps] = ap
		    Memi[beams+naps] = ap
		    call gargi (beam)
		    if (nscan() == 2)
		        Memi[beams+naps] = beam
		    call gargstr (Memc[str], SZ_LINE)	
		    call xt_stripwhite (Memc[str])
		    if (Memc[str] == EOS)
			Memi[titles+naps] = NULL
		    else {
		        call malloc (Memi[titles+naps], SZ_LINE, TY_CHAR)
			call strcpy (Memc[str], Memc[Memi[titles+naps]],
			    SZ_LINE)
		    }
		    naps = naps + 1
		}	
		call close (fd)
	    } then
		call erract (EA_WARN)
	}

	if (nalloc < naps) {
	    call realloc (aps, naps, TY_INT)
	    call realloc (beams, naps, TY_INT)
	    call realloc (titles, naps, TY_INT)
	}

	call sfree (sp)
end


procedure sap_fids (aps, beams, titles, naps)

pointer	aps			# Aperture numbers
pointer	beams			# Beam numbers
pointer	titles			# Titles
int	naps			# Number of apertures

int	i

begin
	if (naps > 0) {
	    do i = 1, naps
		call mfree (Memi[titles+i-1], TY_CHAR)
	    call mfree (aps, TY_INT)
	    call mfree (beams, TY_INT)
	    call mfree (titles, TY_POINTER)
	}
end
