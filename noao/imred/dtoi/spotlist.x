include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	<fset.h>

# T_SPOTLIST -- calculates the densities and standard deviations of calibration
# spots read in as IRAF images.  Entries for density, sdev, spot number
# and number of pixels used in the average are made in the output database.
# These values are entered for all spots and the fog level.  The
# fog level is calculated but not subtracted in this procedure.

procedure t_spotlist ()

pointer	db, sp, den, ngpix, sdev, dloge, option, npix
int	spot_fd, fog_fd, fngpix, nspots, maxad, fnpix
real	scale, sigma, fsdev, fog
double	maxden

pointer	ddb_map()
int	imtopenp(), imtlen(), clgeti(), strncmp()
real	clgetr()

begin
	call smark (sp)
	call salloc (dloge, SZ_FNAME, TY_CHAR)
	call salloc (option, SZ_FNAME, TY_CHAR)

	# Get parameters and open file name templates
	spot_fd = imtopenp ("spots")
	fog_fd  = imtopenp ("fogs")
	call clgstr ("database", Memc[dloge], SZ_FNAME)
	scale = clgetr ("scale")
	maxad = clgeti ("maxad")
	sigma = clgetr ("sigma")
	call clgstr ("option", Memc[option], SZ_FNAME)

	maxden = maxad * double (scale)

	# Allocate space for density, standard deviation and ngpix arrays
	nspots = imtlen (spot_fd)
	call salloc ( den,  nspots, TY_REAL)
	call salloc (sdev,  nspots, TY_REAL)
	call salloc (ngpix, nspots, TY_INT)
	call salloc ( npix, nspots, TY_INT)

	# Calculate densities depending on algorithm option.  The 
	# number of saturated pixels per spot is also calculated now.

	if (strncmp (Memc[option], "median", 3) == 0)
	    call hd_median (spot_fd, Memr[den], Memr[sdev], Memi[ngpix],
		nspots, scale, Memi[npix])
	else
	    call hd_mean (spot_fd, Memr[den], Memr[sdev], Memi[ngpix], 
		nspots, scale, sigma, Memi[npix])

	# Calculate fog level and count saturated pixels
	call hd_fogcalc (fog_fd, fog, fsdev, fngpix, scale, sigma, 
	    Memc[option], fnpix)

	# Now print results to stdout
	call hd_printit (Memr[den], Memr[sdev], Memi[npix], Memi[ngpix], 
	    fog, fsdev, fnpix, fngpix, nspots)
	    
	# Open output database file and write spot information
	db = ddb_map (Memc[dloge], APPEND)
	call hd_wspotdb (db, Memr[den], Memr[sdev], Memi[ngpix], nspots)

	# Write fog information to database as single record
	call ddb_prec (db, "fog")
	call ddb_putr (db, "density", fog)
	call ddb_putr (db, "sdev", fsdev)
	call ddb_puti (db, "ngpix", fngpix)

	# Scale info gets written to database also (very precisely!)
	call ddb_prec (db, "common")
	call ddb_putr (db, "scale", scale)
	call ddb_putd (db, "maxden", maxden)
	call ddb_pstr (db, "option", Memc[option])

	call ddb_unmap (db)

	call imtclose (spot_fd)
	call imtclose (fog_fd)
	call sfree (sp)
end


# HD_MEAN -- Calculate mean density of calibration spots.

procedure hd_mean (spot_fd, den, sdev, ngpix, nspots, scale, sigma, npix)

int	spot_fd		# File descriptor for list of spots
real	den[ARB]	# Mean density values - filled on return
real	sdev[ARB]	# Standard deviation array - filled on return
int	ngpix[ARB]	# Number of unrejected pixels - filled on return
int	nspots		# Number of spots in list
real	scale		# Scale for voltage to density conversion
real	sigma		# Rejection criteria set by user
int	npix[ARB]	# Number of pixels per spot

pointer	im, spot, sp, pix
int	i, junk, ncols, nlines
pointer	immap(), imgs2r()
int	imtgetim(), hd_aravr()
errchk	imgs2r, amulkr, hd_aravr

begin
	call smark (sp)
	call salloc (spot, SZ_FNAME, TY_CHAR)

	# Loop over spot rasters.  Calculate density and standard deviation.
	for (i = 1; i <= nspots; i = i + 1) {
	    junk = imtgetim (spot_fd, Memc[spot], SZ_FNAME)
	    iferr (im = immap (Memc[spot], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    npix[i] = ncols * nlines

	    # For all pixels in the image, scale the a/d value to density and
	    # calculate the mean value, using a mean rejection algorithm.

	    pix = imgs2r (im, 1, ncols, 1, nlines)
	    call amulkr (Memr[pix], scale, Memr[pix], npix[i])
	    ngpix[i] = hd_aravr (Memr[pix], npix[i], den[i], sdev[i], sigma)
	    call imunmap (im)
	}

	call sfree (sp)
end


# HD_MEDIAN -- Calculate median density of calibration spots.

procedure hd_median (spot_fd, den, sdev, ngpix, nspots, scale, npix)

int	spot_fd		# File descriptor for list of spots
real	den[ARB]	# Mean density values - filled on return
real	sdev[ARB]	# Standard deviation of input spots
int	ngpix[ARB]	# Number of pixels not rejected
int	nspots		# Number of spots in list
real	scale		# Scale for voltage to density conversion
int	npix[ARB]	# Number of pixels per spot

pointer	im, spot, sp, pix
int	i, junk, ncols, nlines
real	junk_mean
pointer	immap(), imgs2r()
int	imtgetim()
real	amedr()
errchk	imgs2r, amulkr, amedr

begin
	call smark (sp)
	call salloc (spot, SZ_FNAME, TY_CHAR)

	# Loop over spot rasters.  Calculate density and standard deviation.
	for (i = 1; i <= nspots; i = i + 1) {
	    junk = imtgetim (spot_fd, Memc[spot], SZ_FNAME)
	    iferr (im = immap (Memc[spot], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    npix[i] = ncols * nlines

	    # For all pixels in the image, scale the a/d value to density and
	    # calculate the median value.  For the user's information, the
	    # sigma is also calculated in a call to aavgr.

	    pix = imgs2r (im, 1, ncols, 1, nlines)
	    call amulkr (Memr[pix], scale, Memr[pix], npix[i])
	    den[i] = amedr (Memr[pix], npix[i])
	    ngpix[i] = npix[i]
	    call aavgr (Memr[pix], npix[i], junk_mean, sdev[i])
	    call imunmap (im)
	}

	call sfree (sp)
end


# HD_FOGCALC -- Calculate fog density.

procedure hd_fogcalc (fog_fd, fog, fsdev, fngpix, scale, sigma, option, nfpix)

int	fog_fd		# File descriptor for list of fog spots
real	fog		# Mean fog value - returned
real	fsdev		# Standard deviation - returned
int	fngpix		# Number of pixels used - returned
real	scale		# Voltage to density scaling factor
real	sigma		# Rejection criteria - set by user
char	option[ARB]     # User's choice of mean/median algorithm
int	nfpix		# Total number of fog pixels

pointer	pix, im, sp, fogfile, ptr
int	nfog, maxfog, i, junk, ncols, nlines, npix, total_pix, op
real	junk_mean
pointer	immap(), imgs2r()
int	imtlen(), imtgetim(), hd_aravr(), strncmp()
real	amedr()
errchk	calloc, imgs2r, aaddr, amulkr, hd_aravr

begin
	call smark (sp)
	call salloc (fogfile, SZ_FNAME, TY_CHAR)

	pix = NULL
	total_pix = 0
	op = 0
	nfog = imtlen (fog_fd)
	maxfog = nfog
	do i = 1, maxfog {
	    junk = imtgetim (fog_fd, Memc[fogfile], SZ_FNAME)
	    iferr (im = immap (Memc[fogfile], READ_ONLY, 0)) {
		call erract (EA_WARN)
		nfog = nfog - 1
		next
	    }

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    npix = ncols * nlines
	    total_pix = total_pix + npix

	    if (pix == NULL)
		# Initialize space for accumulating pixels
		call calloc (pix, npix, TY_REAL)
	    else
	       # Increase space for accumulating pixels
	       call realloc (pix, total_pix, TY_REAL)

	    # Build up vector of fog pixels
	    ptr = imgs2r (im, 1, ncols, 1, nlines)
	    call amovr (Memr[ptr], Memr[pix+op], npix)
	    op = op + npix

	    call imunmap (im)
	}

	# Scale values to density and calculate fog and std deviation

	if (nfog > 0) {
# 7 Sept 1989, S. Rooke:  in Suzanne's absence, made following bugfix after
# bug reported by Steve Majewski that fog values are off by 1/n images where
# multiple fog images are used in a single run.  The total_pix already contains
# the sum of all pixel values, so the fog pixel values should not be divided
# by nfog.  This should be verified by Suzanne on her return, and these comments
# removed.
#	    call amulkr (Memr[pix], scale / real (nfog), Memr[pix], total_pix)
	    call amulkr (Memr[pix], scale, Memr[pix], total_pix)
	    if (strncmp (option, "median", 3) == 0) {
		fog = amedr (Memr[pix], total_pix)
		fngpix = total_pix
		call aavgr (Memr[pix], total_pix, junk_mean, fsdev)
	    } else
	        fngpix = hd_aravr (Memr[pix], total_pix, fog, fsdev, sigma)
	}  else {
	    fog = 0.0
	    fsdev = 0.0
	    fngpix = 0
	}
	nfpix = total_pix

	call mfree (pix, TY_REAL)
	call sfree (sp)
end


# HD_WSPOTDB -- Write spot information to database file.  Values are first
# sorted in order of increasing density.

procedure hd_wspotdb (db, density, sdev, ngpix, nspots)

pointer	db			# Pointer to database
real	density[ARB]		# Array of densities
real	sdev [ARB]		# Array of standard deviations
int	ngpix [ARB]		# Array of npix used in calculations
int	nspots			# Number of density spots

begin
	if (density[1] > density[nspots]) {
	    # Need to reorder arrays
	    call hd_reorderr (density, nspots)
	    call hd_reorderr (   sdev, nspots)
	    call hd_reorderi (  ngpix, nspots)
	}

	call ddb_ptime (db)

	# Density record
	call ddb_prec (db, "density")
	call ddb_par (db, "den_val", density, nspots)

	# Standard deviation of density is written to a record
	call ddb_prec (db, "standard deviation")
	call ddb_par (db, "sdev_val", sdev, nspots)

	# Record for npix_used
	call ddb_prec (db, "ngpix")
	call ddb_pai (db, "npix_val", ngpix, nspots)
end


# HD_REORDERR - Flip order of real array in place.

procedure hd_reorderr (array, nvals)

real	array[ARB]		# Real array to be reordered
int	nvals			# Number of elements in array

pointer	sp, tmp
int	i

begin
	call smark (sp)
	call salloc (tmp, nvals, TY_REAL)

	call amovr (array, Memr[tmp], nvals)
	do i = 1, nvals
	    array[i] = Memr[tmp+nvals-i]

	call sfree (sp)
end


# HD_REORDERI -- Flip order of integer array in place.

procedure hd_reorderi (array, nvals)

int	array[ARB]		# Integer array to be ordered
int	nvals			# Number of elements in array

pointer	sp, tmp
int	i

begin
	call smark (sp)
	call salloc (tmp, nvals, TY_INT)

	call amovi (array, Memi[tmp], nvals)
	do i = 1, nvals
	    array[i] = Memi[tmp+nvals-i]

	call sfree (sp)
end


# HD_PRINTIT -- Neatly printout all the accumulated information.

procedure hd_printit (den, sdev, npix, ngpix, fog, fsdev, fnpix, fngpix, nspots)

real	  den[ARB]	# density array 
real	 sdev[ARB]	# std deviation array 
int	 npix[ARB]      # npix array
int	ngpix[ARB]      # ngoodpix array
real	fog		# fog value
real	fsdev           # std deviation of fog
int     fnpix		# npix in fog
int	fngpix		# ngoodpix in fog
int	nspots		# number of spots
int	i

begin

	call fseti (STDOUT, F_FLUSHNL, YES)

	call printf ("\n                                         # Number of P")
	call printf ("ixels\n")
	call printf ("# Spot Number  Density   Std Deviation  Total  Used  Rej")
	call printf ("ected \n")

	do i = 1, nspots {
	    call printf ("    %d %17t%.4f %27t%.4f %43t%d %5d %6d \n")
	        call pargi (i)
	        call pargr (den[i])
	        call pargr (sdev[i])
	        call pargi (npix[i])
	        call pargi (ngpix[i])
	        call pargi (npix[i] - ngpix[i])
	}

	call printf ("   FOG %17t%.4f %27t%.4f %43t%d %5d %6d \n")
	    call pargr (fog)
	    call pargr (fsdev)
	    call pargi (fnpix)
	    call pargi (fngpix)
	    call pargi (fnpix - fngpix)

end
