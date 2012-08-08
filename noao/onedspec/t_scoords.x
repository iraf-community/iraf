include	<error.h>
include	<imhdr.h>

# T_SCOORDS -- Set sampled coordinates in spectra.
# This task is currently limited to 1D spectra.
# It reads a text file of spectral coordinates and sets the WCS.

procedure t_scoords ()

pointer	speclist	# List of spectrum image names
pointer	coordlist	# List of coordinate file names
pointer	label		# Coordinate axis label
pointer	units		# Coordinate axis units

int	n, fd, open(), fscan(), nscan()
int	imtopenp, imtlen(), imtgetim(), clpopnu(), fntlenb(), fntgfnb()
bool	verbose, clgetb()
pointer	sp, spec, coords, values, im, tmp, immap()

errchk	immap, open, scoords

begin
	call smark (sp)
	call salloc (spec, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	speclist = imtopenp ("images")
	coordlist = clpopnu ("coords")
	call clgstr ("label", Memc[label], SZ_FNAME)
	call clgstr ("units", Memc[units], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Check for match between image and coordinate lists.
	if (fntlenb (coordlist) > 1 && fntlenb (coordlist) != imtlen (speclist))
	    call error (1, "Image and coordinate lists do not match")

	# Loop through spectrum list.
	while (imtgetim (speclist, Memc[spec], SZ_FNAME) != EOF) {
	    if (fntgfnb (coordlist, Memc[coords], SZ_FNAME) == EOF)
		;

	    iferr {
		im = NULL
		fd = NULL

		# Open the image.
		tmp = immap (Memc[spec], READ_WRITE, 0); im = tmp

		# Get the coordinate values.
		tmp = open (Memc[coords], READ_ONLY, TEXT_FILE); fd = tmp
		call salloc (values, IM_LEN(im,1)+1, TY_DOUBLE)
		n = 0
		while (fscan(fd) != EOF) {
		    call gargd (Memd[values+n])
		    if (nscan() == 1)
			n = n + 1
		    if (n > IM_LEN(im,1))
			break
		}
		if (n != IM_LEN(im,1))
		    call error (1, "Wrong number of coordinate values in file")

		# Create the WCS
		if (verbose) {
		    call printf ("SCOORDS: ")
		    call printf (
			"Setting coordinates for %s from coordinate file %s.\n")
			call pargstr (Memc[spec])
			call pargstr (Memc[coords])
		}
		call scoords (im, Memc[label], Memc[units], Memd[values])
	    } then
		call erract (EA_WARN)

	    # Close files.
	    if (im != NULL)
		call imunmap (im)
	    if (fd != NULL)
		call close (fd)
	}

	call imtclose (speclist)
	call fntclsb (coordlist)
	call sfree (sp)
end



# SCOORDS -- Make a multispec pixel array coordinate system.
# This is currently limited to 1D spectra.

procedure scoords (im, label, units, waves)

pointer	im		#I Imageio I/O pointer (must be 1D image)
char	label[ARB]	#I Axis label (e.g. "Wavelength")
char	units[ARB]	#I Axis units (e.g. "Angstroms")
double	waves[ARB]	#I Array of dispersion coordinates

int	i, n, fd, stropen()
double	dw
pointer	sp, coeffs, mw, mw_open()

int	axes[6]
data	axes/1, 2, 1, 0, 0, 0/

errchk	mw_open, mw_saveim, stropen

begin
	call smark (sp)

	if (IM_NDIM(im) != 1)
	    call error (1, "scoords: image must be one dimensional")

	# Initialize the MWCS.
	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axes, 2, "multispec", "")
	if (label[1] != EOS)
	    call mw_swattrs (mw, 1, "label", label)
	if (units[1] != EOS)
	    call mw_swattrs (mw, 1, "units", label)
	call mw_saxmap (mw, axes[3], axes[4], 2)

	# Setup multispec coefficient string.
	n = IM_LEN(im,1)
	i = 20 * (n + 6)
	call salloc (coeffs, i, TY_CHAR)
	call aclrc (Memc[coeffs], i)
	fd = stropen (Memc[coeffs], i, NEW_FILE)

	# Set the common attribute parameters.
	dw = (waves[n] - waves[1]) / (n - 1)
	call fprintf (fd, "%d %d %d %g %g %d %g %g %g ")
	    call pargi (1)		# Aperture number
	    call pargi (1)		# Beam number
	    call pargi (2)		# Dispersion type (2=non-linear)
	    call pargd (waves[1])	# Starting coordinate
	    call pargd (dw)		# Average dispersion
	    call pargi (n)		# Number of pixels
	    call pargd (0D0)		# Redshift
	    call pargd (INDEFD)		# Aperture limit
	    call pargd (INDEFD)		# Aperture limit

	# Set the general non-linear function parameters.
	call fprintf (fd, "%g %g %d ")
	    call pargd (1D0)		# Function weight
	    call pargd (0D0)		# Zero point shift
	    call pargi (5)		# Function type (5=pixel array)

	# Set the pixel array function values.
	call fprintf (fd, "%d")
	    call pargi (n)		# Number of pixels
	do i = 1, n {
	    if (i > 2) {
		if ((waves[i]-waves[i-1]) * dw <= 0) {
		    call strclose (fd)
		    call mw_close (mw)
		    call sfree (sp)
		    call error (1, "Coordinates are not monotonic")
		}
	    }

	    call fprintf (fd, " %g")
		call pargd (waves[i])	# Coordinates
	}

	# Write the attribute.
	call strclose (fd)
	call mw_swattrs (mw, 2, "spec1", Memc[coeffs])

	# Store the WCS in the image header.
	call mw_saveim (mw, im)
	call mw_close (mw)

	call sfree (sp)
end
