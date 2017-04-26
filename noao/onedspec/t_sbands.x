include	<error.h>
include	<smw.h>

# Band structure
define	LEN_BAND	9			# length of structure
define	BAND_ID		Memi[$1]		# ptr to band id string
define	BAND_FILTER	Memi[$1+1]		# ptr to filter string
define	BAND_WC		Memd[P2D($1+2)]		# center wavelength
define	BAND_DW		Memd[P2D($1+4)]		# wavelength width
define	BAND_FN		Memi[$1+6]		# no. of filter points
define	BAND_FW		Memi[$1+7]		# ptr to filter wavelengths
define	BAND_FR		Memi[$1+8]		# ptr to filter responses

# Multiple bands for indices and equivalent widths.
define	NBANDS		3			# maximum number of bands
define	BAND1		1
define	BAND2		2
define	BAND3		3
define	BAND		Memi[$1+($2-1)*NBANDS+($3-1)]


# T_SBANDS -- Compute band fluxes, indices, and equivalent widths.
# A list of bandpasses is supplied in a text file, and all of them are applied
# to each spectrum in the list.  The output is written to an output file
# in multicolumn format.

procedure t_sbands ()

pointer	inlist			# Input list of spectra
pointer	output			# Output file name
pointer	fbands			# Band file name
pointer	apertures		# Aperture list string
bool	norm			# Normalize bands by response?
bool	mag			# Output magnitudes instead of fluxes?
double	magzero			# Magnitude zeropoint for magnitude output
bool	verbose			# Verbose header?

int	i, nbands, nsubbands, nimages, fd
pointer	bands, aps, im, smw, sh
pointer	sp, input

int	open(), imtgetim()
bool	clgetb(), rng_elementi()
double	clgetd()
pointer	imtopenp(), immap(), smw_openim(), rng_open()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (fbands, SZ_FNAME, TY_CHAR)
	call salloc (apertures, SZ_LINE, TY_CHAR)

	# Get task parameters.
	inlist = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("bands", Memc[fbands], SZ_FNAME)
	call clgstr ("apertures", Memc[apertures], SZ_LINE)
	norm = clgetb ("normalize")
	mag = clgetb ("mag")
	magzero = clgetd ("magzero")
	verbose = clgetb ("verbose")

	# Read bands from the band file.
	fd = open (Memc[fbands], READ_ONLY, TEXT_FILE)
	call sb_bands (fd, bands, nbands, nsubbands)
	call close (fd)

	# Open the aperture list.
	iferr (aps = rng_open (Memc[apertures], INDEF, INDEF, INDEF))
	    call error (1, "Bad aperture list")

	# Loop over the input spectra.
	fd = 0
	nimages = 0
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    nimages = nimages + 1

	    # Open the input image and get the WCS.
	    iferr (im = immap (Memc[input], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    iferr (smw = smw_openim (im)) {
		call imunmap (im)
		call erract (EA_WARN)
		next
	    }

	    # Open output file and write a verbose header if desired.
	    # It is delayed until now to avoid output if an error occurs
	    # such as image not found.

	    if (nimages == 1) {
		fd = open (Memc[output], APPEND, TEXT_FILE)
		if (verbose)
		    call sb_header (fd, norm, mag, magzero,
			Memc[fbands], bands, nbands, nsubbands)
	    }

	    # Measure selected apertures.
	    do i = 1, SMW_NSPEC(smw) {
		call shdr_open (im, smw, i, 1, INDEFI, SHHDR, sh)
		if (!rng_elementi (aps, AP(sh)))
		    next
		call shdr_open (im, smw, i, 1, INDEFI, SHDATA, sh)

		call sb_proc (fd, sh, bands, nbands, norm, mag, magzero)
	    }

	    # Finish with image.
	    call shdr_close (sh)
	    call smw_close (smw)
	    call imunmap (im)
	}

	# Finish up.
	call sb_free (bands, nbands)
	if (fd != 0)
	    call close (fd)
	call imtclose (inlist)
	call sfree (sp)
end


# SB_BANDS - Read bands from the band file and put them into an array
# of band pointers.

procedure sb_bands (fd, bands, nbands, nsubbands)

int	fd			#I Bandpass file descriptor
pointer	bands			#O Bandpass table descriptor
int	nbands			#O Number of bandpasses
int	nsubbands		#O Number of individual bands

bool	bandok
int	ip
double	center, width
pointer	sp, line, id, filter

int	getline(), ctowrd(), ctod()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (id, SZ_FNAME, TY_CHAR)
	call salloc (filter, SZ_FNAME, TY_CHAR)

	# Read the bands.  If the first band is not seen
	# skip the line.  Check for 1, 2, or 3 bandpasses.
	# Can't use fscan() because fscan() will be called later to
	# read any filter file.

	bands = NULL
	nbands = 0
	nsubbands = 0
	while (getline (fd, Memc[line]) != EOF) {
	    ip = 1
	    bandok = (ctowrd (Memc[line], ip, Memc[id], SZ_FNAME) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, center) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, width) > 0)
	    bandok = (bandok && ctowrd (Memc[line],ip,Memc[filter],SZ_FNAME)>0)
	    if (!bandok || Memc[id] == '#')
		next

	    # Allocate and reallocate the array of band pointers.
	    if (nbands == 0)
		call malloc (bands, 10 * NBANDS, TY_POINTER)
	    else if (mod (nbands, 10) == 0)
		call realloc (bands, (nbands + 10) * NBANDS, TY_POINTER)
	    nbands = nbands + 1

	    call sb_alloc (BAND(bands,nbands,BAND1),
		Memc[id], Memc[filter], center, width)
	    nsubbands = nsubbands + 1

	    bandok = (ctowrd (Memc[line], ip, Memc[id], SZ_FNAME) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, center) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, width) > 0)
	    bandok = (bandok && ctowrd (Memc[line],ip,Memc[filter],SZ_FNAME)>0)
	    if (bandok) {
		call sb_alloc (BAND(bands,nbands,BAND2),
		    Memc[id], Memc[filter], center, width)
		nsubbands = nsubbands + 1
	    } else
		BAND(bands,nbands,BAND2) = NULL

	    bandok = (ctowrd (Memc[line], ip, Memc[id], SZ_FNAME) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, center) > 0)
	    bandok = (bandok && ctod (Memc[line], ip, width) > 0)
	    bandok = (bandok && ctowrd (Memc[line],ip,Memc[filter],SZ_FNAME)>0)
	    if (bandok) {
		call sb_alloc (BAND(bands,nbands,BAND3),
		    Memc[id], Memc[filter], center, width)
		nsubbands = nsubbands + 1
	    } else
		BAND(bands,nbands,BAND3) = NULL
	}

	call sfree (sp)
end


# SB_ALLOC -- Allocate a band structure.

procedure sb_alloc (band, id, filter, center, width)

pointer	band			#O Band pointer
char	id[ARB]			#I Band id
char	filter[ARB]		#I Band filter
double	center			#I Band wavelength
double	width			#I Band width

int	fn, fd, strlen(), open(), fscan(), nscan()
double	w, r
pointer	fw, fr
bool	streq()
errchk	open()

begin
	call calloc (band, LEN_BAND, TY_STRUCT)
	call malloc (BAND_ID(band), strlen(id), TY_CHAR)
	call malloc (BAND_FILTER(band), strlen(filter), TY_CHAR)
	call strcpy (id, Memc[BAND_ID(band)], ARB)
	call strcpy (filter, Memc[BAND_FILTER(band)], ARB)
	BAND_WC(band) = center
	BAND_DW(band) = width
	BAND_FN(band) = 0
	BAND_FW(band) = NULL
	BAND_FR(band) = NULL

	if (streq (filter, "none"))
	    return

	# Read the filter file.
	fd = open (filter, READ_ONLY, TEXT_FILE)
	fn = 0
	while (fscan (fd) != EOF) {
	    call gargd (w)
	    call gargd (r)
	    if (nscan() != 2)
		next
	    if (fn == 0) {
		call malloc (fw, 100, TY_DOUBLE)
		call malloc (fr, 100, TY_DOUBLE)
	    } else if (mod (fn, 100) == 0) {
		call realloc (fw, fn+100, TY_DOUBLE)
		call realloc (fr, fn+100, TY_DOUBLE)
	    }
	    Memd[fw+fn] = w
	    Memd[fr+fn] = r
	    fn = fn + 1
	}
	call close (fd)

	BAND_FN(band) = fn
	BAND_FW(band) = fw
	BAND_FR(band) = fr
end


# SB_FREE -- Free band structures.

procedure sb_free (bands, nbands)

pointer	bands			#I bands descriptor
int	nbands			#I number of bands

int	i, j
pointer	band

begin
	do i = 1, nbands {
	    do j = 1, NBANDS {
		band = BAND(bands,i,j)
		if (band != NULL) {
		    call mfree (BAND_ID(band), TY_CHAR)
		    call mfree (BAND_FILTER(band), TY_CHAR)
		    call mfree (BAND_FW(band), TY_DOUBLE)
		    call mfree (BAND_FR(band), TY_DOUBLE)
		    call mfree (band, TY_STRUCT)
		}
	    }
	}
	call mfree (bands, TY_POINTER)
end


# SB_HEADER -- Print output header.

procedure sb_header (fd, norm, mag, magzero, fbands, bands, nbands, nsubbands)

pointer	fd			#I Output file descriptor
bool	norm			#I Normalization flag
bool	mag			#I Magnitude flag
double	magzero			#I Magnitude zeropoint
char	fbands[ARB]		#I Band file
pointer	bands			#I Pointer to array of bands
int	nbands			#I Number of bands
int	nsubbands		#I Number of subbands

int	i, j
pointer	sp, str, band

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Output a banner and task parameters.
	call sysid (Memc[str], SZ_LINE)
	call fprintf (fd, "\n# SBANDS: %s\n#  ")
	    call pargstr (Memc[str])
	if (fbands[1] != EOS) {
	    call fprintf (fd, " bands = %s,")
		call pargstr (fbands)
	}
	call fprintf (fd, " norm = %b, mag = %b")
	    call pargb (norm)
	    call pargb (mag)
	if (mag) {
	    call fprintf (fd, ", magzero = %.2f")
		call pargd (magzero)
	    call strcpy ("mag", Memc[str], SZ_LINE)
	} else
	    call strcpy ("flux", Memc[str], SZ_LINE)

	# Output the bands.
	call fprintf (fd, "\n# %10s %10s %10s %10s\n")
	    call pargstr ("band")
	    call pargstr ("filter")
	    call pargstr ("wavelength")
	    call pargstr ("width")
	do i = 1, nbands {
	    do j = 1, NBANDS {
		band = BAND(bands,i,j)
		if (band == NULL)
		    next
		call fprintf (fd, "# %10s %10s %10g %10g\n")
		    call pargstr (Memc[BAND_ID(band)])
		    call pargstr (Memc[BAND_FILTER(band)])
		    call pargd (BAND_WC(band))
		    call pargd (BAND_DW(band))
	    }
	}

	# Output column headings.
	call fprintf (fd,
	    "#\n# %24s %7.7s %11.11s")
	    call pargstr ("spectrum")
	    call pargstr ("band")
	    call pargstr (Memc[str])
	if (nsubbands > nbands) {
	    call fprintf (fd, " %7.7s %11.11s %9.9s %9.9s")
		call pargstr ("band")
		call pargstr (Memc[str])
		call pargstr ("index")
		call pargstr ("eqwidth")
	}
	call fprintf (fd, "\n")

	call sfree (sp)
end


# SB_PROC -- Measure the band fluxes and possibly a band index and eq. width.

procedure sb_proc (fd, sh, bands, nbands, norm, mag, magzero)

int	fd			#I Output file descriptor
pointer	sh			#I Spectrum descriptor
pointer	bands			#I Bandpass table pointer
int	nbands			#I Number of bandpasses
bool	norm			#I Normalize?
bool	mag			#I Magnitude output?
double	magzero			#I Magnitude zero point

int	i
double	flux, contval, index, eqwidth
double	flux1, norm1, flux2, norm2, flux3, norm3, a, b
pointer	sp, imname, band1, band2, band3

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[imname], SZ_FNAME, "%s%s(%d)")
	    call pargstr (IMNAME(sh))
	    call pargstr (IMSEC(sh))
	    call pargi (AP(sh))

	# Loop over all bandpasses
	do i = 1, nbands {
	    # Measure primary band flux, normalize, and print result.
	    band1 = BAND(bands,i,BAND1)
	    call sb_flux (sh, band1, flux1, norm1)
	    if (IS_INDEFD(flux1))
		next

	    if (norm) {
		flux1 = flux1 / norm1
		norm1 = 1
	    }
	    if (mag && flux1 > 0.)
		flux = magzero - 2.5 * log10 (flux1)
	    else
		flux = flux1

	    call fprintf (fd, "%26s %7.7s %11.6g")
		call pargstr (Memc[imname])
		call pargstr (Memc[BAND_ID(band1)])
		call pargd (flux)
		
	    # Measure the alternate band fluxes and compute and output
	    # the band index and equivalent width.

	    band2 = BAND(bands,i,BAND2)
	    band3 = BAND(bands,i,BAND3)
	    call sb_flux (sh, band2, flux2, norm2)
	    call sb_flux (sh, band3, flux3, norm3)
	    if (IS_INDEFD(flux2) && IS_INDEFD(flux3)) {
		call fprintf (fd, "\n")
		next
	    }

	    if (norm) {
		if (!IS_INDEFD(flux2)) {
		    flux2 = flux2 / norm2
		    norm2 = 1
		}
		if (!IS_INDEFD(flux3)) {
		    flux3 = flux3 / norm3
		    norm3 = 1
		}
	    }

	    contval = INDEFD
	    index = INDEFD
	    eqwidth = INDEFD
	    if (!IS_INDEFD(flux2) && !IS_INDEFD(flux3)) {
		# Interpolate to the center of the primary band.
		a = (flux2 / norm2 - flux3 / norm3) /
		    (BAND_WC(band2) - BAND_WC(band3))
		b = flux2 / norm2 - a * BAND_WC(band2)
		contval = (a * BAND_WC(band1) + b) * norm1
		call fprintf (fd, " %7.7s")
		    call pargstr ("cont")
	    } else if (!IS_INDEFD(flux2)) {
		contval = flux2
		call fprintf (fd, " %7.7s")
		    call pargstr (Memc[BAND_ID(band2)])
	    } else if (!IS_INDEFD(flux3)) {
		contval = flux3
		call fprintf (fd, " %7.7s")
		    call pargstr (Memc[BAND_ID(band3)])
	    }

	    if (mag && contval > 0.)
		flux = magzero - 2.5 * log10 (contval)
	    else
		flux = contval
	    call fprintf (fd, " %11.6g")
		call pargd (flux)

	    if (flux1 > 0. && contval > 0.) {
		index = flux1 / contval
		eqwidth = (1 - index) * BAND_DW(band1)
	    }
	    if (mag) {
		if (!IS_INDEFD(contval) && contval > 0.)
		    contval = magzero - 2.5 * log10 (contval)
		if (!IS_INDEFD(index))
		    index = -2.5 * log10 (index)
	    }

	    call fprintf (fd, " %9.6g %9.6g\n")
		call pargd (index)
		call pargd (eqwidth)
	}

	# Flush output and finish up.
	call flush (fd)
	call sfree (sp)
end


# SB_FLUX - Compute the flux and total response in a given band.
# Return INDEF if the band is outside of the spectrum.

procedure sb_flux (sh, band, flux, norm)

pointer	sh			#I spectrum descriptor
pointer	band			#I band descriptor
double	flux			#O flux
double	norm			#O normalization

int	i, i1, i2
double	a, b, w1, w2, x1, x2, wt
pointer	x, y
double	sb_filter(), shdr_wl()

begin
	# Return if no band is defined.
	flux = INDEFD
	norm = 1
	if (band == NULL)
	    return

	# Check end points.
	a = BAND_WC(band) - BAND_DW(band) / 2.
	b = BAND_WC(band) + BAND_DW(band) / 2.
	w1 = min (a, b)
	w2 = max (a, b)
	a = shdr_wl (sh, w1)
	b = shdr_wl (sh, w2)
	x1 = min (a, b)
	x2 = max (a, b)
	i1 = nint (x1)
	i2 = nint (x2)
	if (x1 == x2 || i1 < 1 || i2 > SN(sh))
	    return

	x = SX(sh) + i1 - 1
	y = SY(sh) + i1 - 1

	if (i1 == i2) {
	    wt = sb_filter (double(Memr[x]), band) * (x2 - x1)
	    flux = wt * Memr[y]
	    norm = wt
	} else {
	    wt = sb_filter (double(Memr[x]), band) * (i1 + 0.5 - x1)
	    flux = wt * Memr[y]
	    norm = wt
	    x = x + 1
	    y = y + 1
	    for (i = i1 + 1; i <= i2 - 1; i = i + 1) {
		wt = sb_filter (double(Memr[x]), band)
		flux = flux + wt * Memr[y]
		norm = norm + wt
		x = x + 1
		y = y + 1
	    }
	    wt = sb_filter (double(Memr[x]), band) * (x2 - i2 + 0.5)
	    flux = flux + wt * Memr[y]
	    norm = norm + wt
	}
end


# SB_FILTER -- Given a filter array interpolate to the specified wavelength.

double procedure sb_filter (w, band)

double	w		# Wavelength desired
pointer	band		# Band pointer

int	i, n
double	x1, x2
pointer	x, y

begin
	n = BAND_FN(band)
	if (n == 0)
	    return (1.)

	x = BAND_FW(band)
	y = BAND_FR(band)
	x1 = Memd[x]
	x2 = Memd[x+n-1]

	if (w <= x1)
	    return (Memd[y])
	else if (w >= x2)
	    return (Memd[y+n-1])
	
	if ((w - x1) < (x2 - w))
	    for (i = 1; w > Memd[x+i]; i=i+1)
		;
	else
	    for (i = n - 1; w < Memd[x+i-1]; i=i-1)
		;
		
	x1 = Memd[x+i-1]
	x2 = Memd[x+i]
	return ((w - x1) / (x2 - x1) * (Memd[y+i] - Memd[y+i-1]) + Memd[y+i-1])
end
