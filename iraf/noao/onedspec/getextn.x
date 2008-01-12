include	<error.h>
include	<syserr.h>

define	EXTN_LOOKUP	10	# Interp index for de-extinction
define	DEXTN_LOOKUP	11	# Interp index for differential extn table
define	TEMP_SPACE	100	# Amount of temporary space to allocate

# GET_EXTN -- Get extinction from calibration file and
#             any update as indicated from the SENSITIVITY
#             computation

procedure get_extn (wave_tbl, extn_tbl, nwaves)

pointer	wave_tbl, extn_tbl
int	nwaves

pointer	waves, extns

begin
	# Get standard extinction values
	call ext_load (waves, extns, nwaves)

	# Copy values to external pointer.
	# Use of salloc is incorrect but this is a hack on old code. FV
	call salloc (extn_tbl, nwaves, TY_REAL)
	call salloc (wave_tbl, nwaves, TY_REAL)
	call amovr (Memr[waves], Memr[wave_tbl], nwaves)
	call amovr (Memr[extns], Memr[extn_tbl], nwaves)
	call mfree (waves, TY_REAL)
	call mfree (extns, TY_REAL)
end


# DE_EXT_SPEC -- Apply extinction correction to a spectrum

procedure de_ext_spec (spec, airm, w0, wpc, wave_tbl, extn_tbl, nwaves, len)

real	spec[ARB], wave_tbl[ARB], extn_tbl[ARB]
real	airm, w0, wpc
int	nwaves, len

int	i, ierr
real	wave, ext
bool	lin_log

begin
	# Assume linear dispersion, but possibly in LOG10
	if (w0 < 5.0 && wpc < 0.05)
	    lin_log = true
	else
	    lin_log = false

	# Initialize interpolator
	call intrp0 (EXTN_LOOKUP)

	do i = 1, len {
	    wave = w0 + (i-1) * wpc
	    if (lin_log)
		wave = 10.0 ** wave

	    # Table must be in wavelength, not log[]
	    call intrp (EXTN_LOOKUP, wave_tbl, extn_tbl, nwaves, 
		wave, ext, ierr)

	    spec[i] = spec[i] * 10.0 ** (0.4 * airm * ext)
	}
end

# SUM_SPEC -- Add up counts within a specified region of a spectrum
#             denoted by a wavelength range.
#             The summation is active only over those pixels which
#             are completely within the range specification.
#             Data referenced outside the spectrum is ignored.

procedure sum_spec (spec, w1, w2, w0, wpc, counts, len)

real	spec[ARB], w1, w2, w0, wpc, counts
int	len

int	i, pix1, pix2

real	pix_index()

begin
	# Compute pixel numbers from w1 to w2
	pix1 = max (int (pix_index (w0, wpc, w1) + 1.0), 1)
	pix2 = max (int (pix_index (w0, wpc, w2)      ), pix1)
	pix2 = min (pix2, len)

	counts = 0.0

	do i = pix1, pix2
	    counts = counts + spec[i]

	# Guarantee that there are no negative counts
	if (counts < 0.0)
	    counts = 0.0
end

# PIX_INDEX -- Returns the pixel index at wavelength for linearly
#              dispersion corrected spectra
#
#              The "Guess" is made that if the start wavelength for the
#              spectrum is less than 5.0 and the dispersion is less than
#              0.05, the spectrum has been linearized in LOG10 space.
#
# Note that in IRAF, a pixel index effectively refers to the center of a pixel.
# So a spectrum must actually extend from w0-0.5*wpc to w0+(len+0.5)*wpc

real procedure pix_index (w0, wpc, w)

real	w0, wpc, w
real	xw

begin
	# Check for LOG10 dispersion

	if (w0 < 5.0 && wpc < 0.05)
	    xw = log10 (w)
	else
	    xw = w

	pix_index = (xw - w0) / wpc + 1.0
end


define	NALLOC	128	# Allocation block size

# EXT_LOAD -- Read extinction data from database directory.

procedure ext_load (waves, extns, nwaves)

pointer	waves, extns
int	nwaves

real	wave, extn
int	fd, nalloc
pointer	sp, file

int	open(), fscan(), nscan(), errcode()

begin
	call smark (sp)
	call salloc (file, SZ_FNAME, TY_CHAR)

	# Get the extinction file and open it.
	call clgstr ("extinction", Memc[file], SZ_FNAME)
	iferr (fd = open (Memc[file], READ_ONLY, TEXT_FILE)) {
	    switch (errcode()) {
	    case SYS_FNOFNAME:
		nwaves = 2
		call malloc (waves, nwaves, TY_REAL)
		call malloc (extns, nwaves, TY_REAL)
		Memr[waves] = 1000.
		Memr[extns] = 0.
		Memr[waves+1] = 10000.
		Memr[extns+1] = 0.
		call eprintf ("No extinction correction applied\n")
		return
	    default:
		call erract (EA_ERROR)
	    }
	}

	# Read the extinction data.
	nalloc = 0
	nwaves = 0
	while (fscan (fd) != EOF) {
	    call gargr (wave)
	    call gargr (extn)
	    if (nscan() != 2)
		next

	    if (nalloc == 0) {
		nalloc = nalloc + NALLOC
		call malloc (waves, nalloc, TY_REAL)
		call malloc (extns, nalloc, TY_REAL)
	    } else if (nwaves == nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (waves, nalloc, TY_REAL)
		call realloc (extns, nalloc, TY_REAL)
	    }

	    Memr[waves+nwaves] = wave
	    Memr[extns+nwaves] = extn
	    nwaves = nwaves + 1
	}
	call close (fd)

	if (nwaves == 0)
	    call error (1, "No extinction data found")

	call realloc (waves, nwaves, TY_REAL)
	call realloc (extns, nwaves, TY_REAL)

	call sfree (sp)
end


# EXT_FREE -- Free extinction data arrays.

procedure ext_free (waves, extns)

pointer	waves, extns

begin
	call mfree (waves, TY_REAL)
	call mfree (extns, TY_REAL)
end
