# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pkg/gtools.h>

# LINECLEAN -- Fit a function to the image lines and output an image
# with rejected points replaced by the fit.  The fitting parameters may be
# set interactively using the icfit package.

procedure t_lineclean ()

int	listin				# Input image list
int	listout				# Output image list
char	sample[SZ_LINE]			# Sample ranges
int	naverage			# Sample averaging size
char	function[SZ_LINE]		# Curve fitting function
int	order				# Order of curve fitting function
real	low_reject, high_reject		# Rejection threshold
int	niterate			# Number of rejection iterations
real	grow				# Rejection growing radius
bool	interactive			# Interactive?

char	input[SZ_LINE]			# Input image
char	output[SZ_FNAME]		# Output image
pointer	in, out				# IMIO pointers
pointer	ic				# ICFIT pointer
pointer	gt				# GTOOLS pointer

int	imtopen(), imtgetim(), imtlen(), gt_init()
int	clgeti()
real	clgetr()
bool	clgetb()

begin
	# Get input and output lists and check that the number of images
	# are the same.

	call clgstr ("input", input, SZ_LINE)
	listin = imtopen (input)
	call clgstr ("output", input, SZ_LINE)
	listout = imtopen (input)
	if (imtlen (listin) != imtlen (listout)) {
	    call imtclose (listin)
	    call imtclose (listout)
	    call error (0, "Input and output image lists do not match")
	}

	# Get task parameters.

	call clgstr ("sample", sample, SZ_LINE)
	naverage = clgeti ("naverage")
	call clgstr ("function", function, SZ_LINE)
	order = clgeti ("order")
	low_reject = clgetr ("low_reject")
	high_reject = clgetr ("high_reject")
	niterate = clgeti ("niterate")
	grow = clgetr ("grow")
	interactive = clgetb ("interactive")


	# Set the ICFIT pointer structure.
	call ic_open (ic)
	call ic_pstr (ic, "sample", sample)
	call ic_puti (ic, "naverage", naverage)
	call ic_pstr (ic, "function", function)
	call ic_puti (ic, "order", order)
	call ic_putr (ic, "low", low_reject)
	call ic_putr (ic, "high", high_reject)
	call ic_puti (ic, "niterate", niterate)
	call ic_putr (ic, "grow", grow)
	call ic_pstr (ic, "ylabel", "")

	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")

	# Clean the lines in each input image.

	while ((imtgetim (listin, input, SZ_FNAME) != EOF) &&
	    (imtgetim (listout, output, SZ_FNAME) != EOF)) {

	    call lc_immap (input, output, in, out)
	    call lineclean (in, out, ic, gt, input, interactive)
	    call imunmap (in)
	    call imunmap (out)
	}

	call ic_closer (ic)
	call gt_free (gt)
	call imtclose (listin)
	call imtclose (listout)
end


# LINECLEAN -- Given the image descriptor determine the fitting function
# for each line and create an output image.  If the interactive flag
# is set then set the fitting parameters interactively.

procedure lineclean (in, out, ic, gt, title, interactive)

pointer	in				# IMIO pointer for input image
pointer	out				# IMIO pointer for output image
pointer	ic				# ICFIT pointer
pointer	gt				# GTIO pointer
char	title[ARB]			# Title
bool	interactive			# Interactive?

char	graphics[SZ_FNAME]
int	i, nx, new
long	inline[IM_MAXDIM], outline[IM_MAXDIM]
pointer	cv, gp, sp, x, wts, indata, outdata

int	lf_getline(), imgnlr(), impnlr(), strlen()
pointer	gopen()

begin
	# Allocate memory for curve fitting.

	nx = IM_LEN(in, 1)

	call smark (sp)
	call salloc (x, nx, TY_REAL)
	call salloc (wts, nx, TY_REAL)

	do i = 1, nx
	    Memr[x+i-1] = i
	call amovkr (1., Memr[wts], nx)

	call ic_putr (ic, "xmin", Memr[x])
	call ic_putr (ic, "xmax", Memr[x+nx-1])

	# If the interactive flag is set then use icg_fit to set the
	# fitting parameters.  Get_fitline returns EOF when the user
	# is done.  The weights are reset since the user may delete
	# points.

	if (interactive) {
	    call clgstr ("graphics", graphics, SZ_FNAME)
	    gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)

	    i = strlen (title)
	    while (lf_getline (ic, gt, in, indata, inline, title)!=EOF) {
		title[i + 1] = EOS
	        call icg_fit (ic, gp, "cursor", gt, cv, Memr[x], Memr[indata],
		    Memr[wts], nx)
		call amovkr (1., Memr[wts], nx)
	    }
	    call gclose (gp)
	}

	# Loop through each input image line and create an output image line.

	new = YES
	call amovkl (long(1), inline, IM_MAXDIM)
	call amovkl (long(1), outline, IM_MAXDIM)

	while (imgnlr (in, indata, inline) != EOF) {
	    if (impnlr (out, outdata, outline) == EOF)
		call error (0, "Error writing output image")

	    call ic_fit (ic, cv, Memr[x], Memr[indata], Memr[wts],
		nx, new, YES, new, new)
	    new = NO

	    call ic_clean (ic, cv, Memr[x], Memr[indata], Memr[wts], nx)

	    call amovr (Memr[indata], Memr[outdata], nx)
	}

	call cvfree (cv)
	call sfree (sp)
end


# LC_IMMAP -- Map images for lineclean.

procedure lc_immap (input, output, in, out)

char	input[ARB]		# Input image
char	output[ARB]		# Output image
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

pointer	sp, root, sect
int	imaccess()
pointer	immap()

begin
	# Get the root name and section of the input image.

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (sect, SZ_FNAME, TY_CHAR)

	call get_root (input, Memc[root], SZ_FNAME)
	call get_section (input, Memc[sect], SZ_FNAME)

	# If the output image is not accessible then create it as a new copy
	# of the full input image.

	if (imaccess (output, 0) == NO)
	    call img_imcopy (Memc[root], output, false)

	# Map the input and output images.

	in = immap (input, READ_ONLY, 0)

	call sprintf (Memc[root], SZ_FNAME, "%s%s")
	    call pargstr (output)
	    call pargstr (Memc[sect])
	out = immap (Memc[root], READ_WRITE, 0)

	call sfree (sp)
end


# LF_GETLINE -- Get an image line to be fit interactively.  Return EOF
# when the user enters EOF or CR.  Default line is the first line and
# the out of bounds lines are silently limited to the nearest in bounds line.

int procedure lf_getline (ic, gt, im, data, v, title)

pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
pointer	im			# IMIO pointer
pointer	data			# Image data
long	v[ARB]			# Image line vector
char	title[ARB]		# Title

int	i
char	line[SZ_LINE]
int	getline(), nscan(), imgnlr()

begin
	call sprintf (title, SZ_LINE, "%s: Fit line =")
	    call pargstr (title)

	call printf ("%s ")
	    call pargstr (title)
	call flush (STDOUT)

	if (getline(STDIN, line) == EOF)
	    return (EOF)
	call sscan (line)

	call amovkl (long (1), v, IM_MAXDIM)
	do i = 2, max (2, IM_NDIM(im)) {
	    call gargl (v[i])
	    if (nscan() == 0)
		return (EOF)
	    else if (nscan() != i - 1)
		break

	    if (IM_NDIM(im) == 1)
		v[i] = 1
	    else
	        v[i] = max (1, min (IM_LEN(im, i), v[i]))

	    call sprintf (title, SZ_LINE, "%s %d")
		call pargstr (title)
		call pargl (v[i])
	}

	call sprintf (title, SZ_LINE, "%s\n%s")
	    call pargstr (title)
	    call pargstr (IM_TITLE(im))
	call ic_pstr (ic, "xlabel", "Column")
	call gt_sets (gt, GTTITLE, title)

	return (imgnlr (im, data, v))
end
