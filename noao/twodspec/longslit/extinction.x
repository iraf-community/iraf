include	<imhdr.h>
include	<error.h>


# T_EXTINCTION -- CL task for applying extinction corrections to images.
#
# The image headers must contain the parameters DISPAXIS, CRVALn,
# CRPIXn, and CDELTn to define the wavelength coordinates and
# either AIRMASS, ZD, or information needed to compute the zenith
# distance (HA, LATITUDE, RA, DEC, ST).
#
# The extinction table contains wavelengths and extinctions in
# magnitudes such that the multiplicative extinction correction
# is given by:
#
#	correction = 10 ** (0.4 * airmass * extinction value)
#
# The extinction table need not be sorted.


procedure t_extinction()

int	list1			# List of images to be corrected
int	list2			# List of extinction corrected images
char	table[SZ_FNAME]		# Extinction table filename

bool	extcor
char	image1[SZ_FNAME], image2[SZ_FNAME]
int	fd, nalloc, len_table
real	wavelen, ext
pointer	im1, im2, w, e

int	clpopnu(), fscan(), nscan(), open(), clgfil()
bool	imgetb(), streq()
pointer	immap()

errchk	ext_cor()

begin
	# Get the list of images and the extinction table.

	list1 = clpopnu ("input")
	list2 = clpopnu ("output")
	call clgstr ("extinction", table, SZ_FNAME)

	# Read the extinction table.  Dynamically allocate memory for the
	# table.

	fd = open (table, READ_ONLY, TEXT_FILE)
	nalloc = 100
	call malloc (w, nalloc, TY_REAL)
	call malloc (e, nalloc, TY_REAL)

	len_table = 0
	while (fscan (fd) != EOF) {
	    call gargr (wavelen)
	    call gargr (ext)
	    if (nscan() < 2)
		next

	    if (len_table == nalloc) {
		nalloc = nalloc + 100
		call realloc (w, nalloc, TY_REAL)
		call realloc (e, nalloc, TY_REAL)
	     }

	     Memr[w + len_table] = wavelen
	     Memr[e + len_table] = ext
	     len_table = len_table + 1
	}
	call close (fd)

	# If there are no extinction values in the table then return an error.
	# Sort the extinction values by wavelength.

	if (len_table > 0) {
	    call realloc (w, len_table, TY_REAL)
	    call realloc (e, len_table, TY_REAL)
	    call xt_sort2 (Memr[w], Memr[e], len_table)
	} else {
	    call mfree (w, TY_REAL)
	    call mfree (e, TY_REAL)
	    call error (0, "No extinction values extinction table")
	}

	# Loop through each pair of input and output images.  Check if
	# the input image has been corrected previously.  If TRUE then
	# print message and go on to the next input image.  If FALSE
	# print message and apply extinction corrections.
	# Missing information in the image header will return an error
	# which will warn the user and go on to the next image.

	while (clgfil (list1, image1, SZ_FNAME) != EOF) {

	    if (clgfil (list2, image2, SZ_FNAME) == EOF) {
		call eprintf ("No output image for %s.\n")
		    call pargstr (image1)
		next
	    }

	    if (streq (image1, image2)) {
	        im1 = immap (image1, READ_WRITE, 0)
	        im2 = im1
	    } else {
	        im1 = immap (image1, READ_ONLY, 0)
	        im2 = immap (image2, NEW_COPY, im1)
	    }

	    iferr (extcor = imgetb (im1, "extcor"))
		extcor = false

	    if (extcor) {
		call printf ("Image %s is extinction corrected.\n")
		    call pargstr (image1)
	    } else {
	        call printf ("Extinction correction: %s -> %s.\n")
		    call pargstr (image1)
		    call pargstr (image2)
	        call flush (STDOUT)
	        iferr (call do_extinct(im1, im2, Memr[w], Memr[e], len_table)) {
		    call printf ("!!No extinction correction for %s!!\n")
		        call pargstr (image1)
		    call flush (STDOUT)
		    call erract (EA_WARN)
	        }
	    }

	    if (im2 != im1)
	        call imunmap (im2)
	    call imunmap (im1)
	}

	# Finish up.

	call mfree (w, TY_REAL)
	call mfree (e, TY_REAL)
	call clpcls (list1)
	call clpcls (list2)
end


# DO_EXTINCT -- Apply extinction correction.

define	SZ_FIELD	8	# Size of field string

procedure do_extinct (im1, im2, w, e, len_table)

pointer	im1			# Input IMIO pointer
pointer	im2			# Output IMIO pointer
real	w[len_table]		# Wavelengths
real	e[len_table]		# Extinction values
int	len_table		# Length of extinction table

char	field[SZ_FIELD]
int	laxis, paxis, npix, i, flag, dcflag
real	crval, cdelt, crpix, airmass, wavelen, extval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	sp, ext, pix1, pix2

int	imgeti(), imgnlr(), impnlr()
real	imgetr(), img_airmass()
errchk	get_daxis, imgeti, imgetr, img_airmass

begin
	# Determine the dispersion axis and linear coordinates.
	call get_daxis (im1, laxis, paxis)

	call sprintf (field, SZ_FIELD, "crval%d")
	    call pargi (laxis)
	crval = imgetr (im1, field)
	call sprintf (field, SZ_FIELD, "crpix%d")
	    call pargi (laxis)
	crpix = imgetr (im1, field)
	call sprintf (field, SZ_FIELD, "cdelt%d")
	    call pargi (laxis)
	iferr (cdelt = imgetr (im1, field)) {
	    call sprintf (field, SZ_FIELD, "cd%d_%d")
	        call pargi (laxis)
	        call pargi (laxis)
	    cdelt = imgetr (im1, field)
	}
	dcflag = imgeti (im1, "dc-flag")

	# Determine the airmass.

	airmass = img_airmass (im1)

	# Determine the extinction values at each pixel.

	npix = IM_LEN (im1, laxis)
	call smark (sp)
	call salloc (ext, npix, TY_REAL)

	do i = 1, npix {
	    wavelen = crval + (i - crpix) * cdelt
	    if (dcflag == 1)
		wavelen = 10. ** wavelen
	    call intrp (1, w, e, len_table, wavelen, extval, flag)
	    Memr[ext+i-1] = 10. ** (0.4 * airmass * extval)
	}

	# Loop through the image applying the extinction correction to each
	# pixel.

	call amovkl (long (1), v1, IM_MAXDIM)
	call amovkl (long (1), v2, IM_MAXDIM)
	while ((imgnlr(im1, pix1, v1) != EOF) &&
	    (impnlr(im2, pix2, v2) != EOF)) {
	    switch (laxis) {
	    case 1:
		call amulr (Memr[pix1], Memr[ext], Memr[pix2], IM_LEN (im1, 1))
	    default:
		extval = Memr[ext+v1[laxis]-2]
		call amulkr (Memr[pix1], extval, Memr[pix2], IM_LEN (im1, 1))
	    }
	}

	call sfree (sp)

	# Add the extinction correction flag, history, and return.
	# The parameter ex-flag is added for compatibility with onedspec.

	call imaddb (im2, "extcor", true)
	call imaddi (im2, "ex-flag", 0)
	call xt_phistory (im2, "Extinction correction applied.")
end
