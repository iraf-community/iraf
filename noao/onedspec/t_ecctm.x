include	<imhdr.h>
include <pkg/gtools.h>
include <pkg/rg.h>
include <math/curfit.h>
include	<error.h>
include "idsmtn.h"

# ECCONTINUUM -- Fit a function to echelle spectra orders and output an image
# consisting of the fit, difference or ratio; or print the power series
# coefficients of the fit.  The fitting parameters may be set interactively
# using the icfit package.

# Image header keyword for saving the orders previously normalized

define	ECC_KW		"ECORDERS"

# Choices for the type of output

define	OUT_TYPES	"|fit|difference|ratio|"

define	FIT		1
define	DIFFERENCE	2
define	RATIO		3

# Choices for the interactive prompts
# (the 1st define is for clgwrd (strdic), the 2nd for CL enumeration)
# Note that the CL assumes that the separator is `|'.

define	ECC_ANS1	"|yes|no|skip|YES|NO|SKIP|"
define	ECC_ANS1X	"yes|no|skip|YES|NO|SKIP"

define	ECC_ANS2	"|order|image|all|cancel|"
define	ECC_ANS2X	"order|image|all|cancel"

define	LEN_ANS		7

define	YES_ONCE	1
define	NO_ONCE		2
define	SKIP_ONCE	3
define	YES_ALWAYS	4
define	NO_ALWAYS	5
define	SKIP_ALWAYS	6

define	SKIP_ORDER	1
define	SKIP_IMAGE	2
define	SKIP_ALL	3
define	SKIP_CANCEL	4

# Switches and pointers

define	ECC_OFF		20			# with a little elbow room

define	INTERACTIVE	Memi[$1]		# all orders are non-interactive
define	WAVESCALE	Memi[$1+1]		# X is wavelength if possible
define	LOGSCALE	Memi[$1+2]		# axes are logarithmic
define	OVERRIDE	Memi[$1+3]		# allow lines to be redone
define	LISTONLY	Memi[$1+4]		# don't modify images
define	OUTTYPE		Memi[$1+5]		# output type code

define	GRAPH_OPEN	Memi[$1+6]		# keep track of gopen
define	LOG_TO_STDOUT	Memi[$1+7]		# STDOUT/ERR is used
define	PROMPT		Memi[$1+8]		# prompt flag
define	QUIET		Memi[$1+9]		# quiet flag

define	RGIN		Memi[$1+10]		# lines specified
define	RGFIT		Memi[$1+11]		# all lines to fit
define	RGREFIT		Memi[$1+12]		# those to fit again

define	NLOGFD		Memi[$1+13]		# number of logfiles
define	LOGFD		Memi[$1+14]		# array of logfiles

define	IC		Memi[$1+15]		# current ic descriptor
define	YMAX		Memi[$1+16]		# max number of lines
define	IC_DESC		Memi[$1+ECC_OFF+$2-1]	# ic descriptors


# T_ECCONTINUUM -- Entry point for the task.  Read parameters,
# initialize structures and loop over the image templates.

procedure t_eccontinuum ()

pointer	listin, listout, input, output, graphics
pointer	ec, gp, gt, in, out, sp
int	stat

int	ecc_icfit(), imtgetim(), gt_init(), imtlen()
bool	clgetb()
pointer	imtopenp()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)

	# Open the image templates
	listin = imtopenp ("input")

	if (clgetb ("listonly"))
	    listout = NULL
	else {
	    listout = imtopenp ("output")
	    if (imtlen (listin) != imtlen (listout)) {
		call imtclose (listin)
		call imtclose (listout)
		call sfree (sp)
		call error (1, "Input and output image lists do not match")
	    }
	}

	# Initialize the various descriptors
	iferr (call ecc_init (listin, listout, ec)) {
	    call imtclose (listin)
	    if (listout != NULL)
		call imtclose (listout)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	# The graphics pointers are passed explicitly
	if (INTERACTIVE(ec) == YES) {
	    call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	    gt = gt_init()
	    call gt_sets (gt, GTTYPE, "line")
	}

	# Fit the lines in each input image.

	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {

	    if (listout != NULL)
		stat = imtgetim (listout, Memc[output], SZ_FNAME)
	    else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    iferr {
		call ecc_immap (Memc[input], Memc[output], in, out, ec, gp)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    stat = ecc_icfit (in, out, ec, gp, gt, Memc[graphics])

	    call ecc_unmap (in, out, ec)

	    if (stat == EOF)
		break
	}

	if (INTERACTIVE(ec) == YES)
	    call gt_free (gt)
	if (GRAPH_OPEN(ec) == YES)
	    call gclose (gp)

	call ecc_close (ec)
	call imtclose (listin)
	if (listout != NULL)
	    call imtclose (listout)
	call sfree (sp)
end


# ECC_INIT -- Initialize templates, ranges, logfiles, type, icfit descriptors.

procedure ecc_init (listin, listout, ec)

pointer	listin, listout		#I Image template descriptors
pointer	ec			#I Pointer to task switches

pointer	input, output, im, sp
int	ymax, i

real	clgetr()
bool	clgetb()
int	clgwrd(), clgeti(), btoi(), strlen()
int	rg_next(), imtgetim(), imaccess(), xt_logopen()
pointer	rg_ranges(), immap()

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	ec = NULL
	iferr {

	    # find the maximum number of lines (orders)
	    ymax = 0

	    while (imtgetim (listin, Memc[input], SZ_LINE) != EOF)
		if (imaccess (Memc[input], READ_ONLY) == YES) {
		    im = immap (Memc[input], READ_ONLY, 0)
		    ymax = max (IM_LEN(im, 2), ymax)
		    call imunmap (im)
		}
	    call imtrew (listin)

	    if (listout != NULL) {
		while (imtgetim (listout, Memc[output], SZ_FNAME) != EOF)
		    if (imaccess (Memc[output], READ_ONLY) == YES) {
			im = immap (Memc[output], READ_ONLY, 0)
			ymax = max (IM_LEN(im, 2), ymax)
			call imunmap (im)
		    }
		call imtrew (listout)
	    }

	    # allocate space for the task switch structure
	    call malloc (ec, ECC_OFF + ymax, TY_STRUCT)

	    YMAX(ec) = ymax

	    # NULL the pointers for error handling
	    RGIN(ec)	= NULL
	    NLOGFD(ec)	= 0
	    do i = 1, YMAX(ec)
		IC_DESC(ec,i) = NULL

	    # Set the switches
	    INTERACTIVE(ec) = btoi (clgetb ("interactive"))
	    WAVESCALE(ec) = btoi (clgetb ("wavescale"))
	    LOGSCALE(ec) = btoi (clgetb ("logscale"))
	    OVERRIDE(ec) = btoi (clgetb ("override"))
	    LISTONLY(ec) = btoi (clgetb ("listonly"))
	    GRAPH_OPEN(ec) = NO
	    PROMPT(ec) = INTERACTIVE(ec)
	    QUIET(ec) = btoi (INTERACTIVE(ec) == NO)

	    # Expand the range specification, allow either hyphens or colons

	    call clgstr ("lines", Memc[input], SZ_LINE)

	    do i = 1, strlen (Memc[input])
		if (Memc[input+i-1] == '-')
		    Memc[input+i-1] =  ':'
		else if (Memc[input+i-1] == 'x' || Memc[input+i-1] == 'X')
		    call error (1, "Range step (`x' notation) not implemented")

	    RGIN(ec) = rg_ranges (Memc[input], 1, YMAX(ec))
	    call rg_order (RGIN(ec))
	    call rg_merge (RGIN(ec))

	    i = 0
	    if (rg_next (RGIN(ec), i) == EOF)
		call error (1, "With range specification for `lines'")
	    else {
		# Open the initial icfit descriptor
		call ic_open (IC(ec))

		call clgstr ("sample", Memc[input], SZ_LINE)
		call ic_pstr (IC(ec), "sample", Memc[input])
		call clgstr ("function", Memc[input], SZ_LINE)
		call ic_pstr (IC(ec), "function", Memc[input])

		call ic_puti (IC(ec), "naverage", clgeti ("naverage"))
		call ic_puti (IC(ec), "order", clgeti ("order"))
		call ic_putr (IC(ec), "low", clgetr ("low_reject"))
		call ic_putr (IC(ec), "high", clgetr ("high_reject"))
		call ic_puti (IC(ec), "niterate", clgeti ("niterate"))
		call ic_putr (IC(ec), "grow", clgetr ("grow"))

		IC_DESC(ec,i) = IC(ec)
	    }

	    # Get the desired output type
	    OUTTYPE(ec) = clgwrd ("type", Memc[input], SZ_LINE, OUT_TYPES)

	    # Open the logfiles
	    NLOGFD(ec) = xt_logopen ("logfiles", "ECCONTINUUM:", LOGFD(ec),
		LOG_TO_STDOUT(ec))

	} then {
	    call sfree (sp)
	    call ecc_close (ec)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
	return
end


# ECC_CLOSE -- Close the various descriptors.

procedure ecc_close (ec)

pointer	ec			#I Pointer to task switches

int	i

begin
	if (ec != NULL) {
	    if (RGIN(ec) != NULL)
		call rg_free (RGIN(ec))
	    if (NLOGFD(ec) != 0)
		call xt_logclose (LOGFD(ec), NLOGFD(ec), "END:")
	    do i = 1, YMAX(ec)
		if (IC_DESC(ec,i) != NULL)
		    call ic_closer (IC_DESC(ec,i))
	    call mfree (ec, TY_STRUCT)
	}
end


# ECC_IMMAP -- Map images for eccontinuum.

procedure ecc_immap (input, output, in, out, ec, gp)

char	input[ARB]		#I Input image name
char	output[ARB]		#I Output image name
pointer	in, out			#O IMIO pointers
pointer	ec			#I Pointer for task switches
pointer	gp			#I GIO pointer

int	i
pointer	inroot, insect, outroot, outsect, b1, b2
pointer	sp, inranges, outranges, rgin, rgout, rgtmp
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
char	emsg[SZ_LINE]

int	imaccess(), imaccf(), imgnlr(), impnlr(), strcmp()
pointer	immap(), rg_ranges(), rg_window(), rg_union(), rg_intersect()
errchk	immap

define	err_	13

begin
	call smark (sp)
	call salloc (inroot, SZ_FNAME, TY_CHAR)
	call salloc (insect, SZ_FNAME, TY_CHAR)
	call salloc (outroot, SZ_FNAME, TY_CHAR)
	call salloc (outsect, SZ_FNAME, TY_CHAR)
	call salloc (inranges, SZ_LINE, TY_CHAR)
	call salloc (outranges, SZ_LINE, TY_CHAR)

	in = NULL
	out = NULL
	RGFIT(ec) = NULL
	RGREFIT(ec) = NULL

	call imgimage (input, Memc[inroot], SZ_FNAME)
	call imgsection (input, Memc[insect], SZ_FNAME)

	call imgimage (output, Memc[outroot], SZ_FNAME)
	call imgsection (output, Memc[outsect], SZ_FNAME)

	if (Memc[insect] != EOS || Memc[outsect] != EOS) {

	    call sprintf (emsg, SZ_LINE, "Sections not allowed (%s --> %s)")
		call pargstr (input)
		call pargstr (output)
	    goto err_

	} else if (imaccess (Memc[inroot], READ_ONLY) == NO) {

	    call sprintf (emsg, SZ_LINE, "Cannot access %s")
		call pargstr (input)
	    goto err_

	} else if (LISTONLY(ec) == YES) {

	    # The `out = in' allows the ranges code at the end of this
	    # procedure to cover all cases (with a little inefficiency).
	    # No check on the sizes of the input and output images.

	    in = immap (Memc[inroot], READ_ONLY, 0)
	    out = in

	} else if (strcmp (Memc[inroot], Memc[outroot]) == 0) {

	    # Overwrite the input image.
	    in = immap (Memc[inroot], READ_WRITE, 0)
	    out = in

	} else if (imaccess (Memc[outroot], READ_WRITE) == NO) {

	    in = immap (Memc[inroot], READ_ONLY, 0)
	    out = immap (Memc[outroot], NEW_COPY, in)

	    # Do this since imcopy is unimplemented

	    call amovkl (long(1), v1, IM_MAXDIM)
	    call amovkl (long(1), v2, IM_MAXDIM)

	    while (imgnlr (in, b1, v1) != EOF && impnlr (out, b2, v2) != EOF)
		call amovr (Memr[b1], Memr[b2], IM_LEN(in, 1))

	} else {

	    in = immap (Memc[inroot], READ_ONLY, 0)
	    out = immap (Memc[outroot], READ_WRITE, 0)

	    # This relies on the axes beyond IM_NDIM(im) being unity

	    do i = 1, max (IM_NDIM(in), IM_NDIM(out))
		if (IM_LEN(in, i) != IM_LEN(out, i)) {
		    call sprintf (emsg, SZ_LINE, "%s & %s aren't the same size")
			call pargstr (Memc[inroot])
			call pargstr (Memc[outroot])
		    goto err_
		}

	}

	do i = 3, IM_NDIM(in)
	    if (IM_LEN(in, i) != 1) {
		call sprintf (emsg, SZ_LINE, "Too many dimensions for %s")
		    call pargstr (Memc[inroot])
		goto err_
	    }

	if (imaccf (in, ECC_KW) == YES)
	    call imgstr (in, ECC_KW, Memc[inranges], SZ_LINE)
	else
	    call strcpy ("", Memc[inranges], SZ_LINE)

	if (imaccf (out, ECC_KW) == YES)
	    call imgstr (out, ECC_KW, Memc[outranges], SZ_LINE)
	else {
	    call strcpy ("", Memc[outranges], SZ_LINE)
	    call imastr (out, ECC_KW, Memc[outranges])
	}

	rgin = rg_ranges (Memc[inranges], 1, IM_LEN(in, 2))
	rgout = rg_ranges (Memc[outranges], 1, IM_LEN(out, 2))
	rgtmp = rg_union (rgin, rgout)
	call rg_free (rgin)
	call rg_free (rgout)

	if (OVERRIDE(ec) == YES) {
	    RGFIT(ec) = rg_window (RGIN(ec), 1, IM_LEN(in, 2))
	    RGREFIT(ec) = rgtmp
	} else {
	    call rg_inverse (rgtmp, 1, IM_LEN(out, 2))
	    RGFIT(ec) = rg_intersect (RGIN(ec), rgtmp)
	    RGREFIT(ec) = rg_ranges ("0", 1, 2)
	    call rg_free (rgtmp)
	}

	if (RG_NPTS(RGFIT(ec)) <= 0) {
	    call sprintf (emsg, SZ_LINE, "No lines left to fit for %s")
		call pargstr (Memc[inroot])
	    goto err_
	}

	call sfree (sp)
	return

err_	call sfree (sp)
	call ecc_unmap (in, out, ec)
	if (GRAPH_OPEN(ec) == YES) {
	    call gclose (gp)
	    GRAPH_OPEN(ec) = NO
	}
	# STDERR should get flushed AFTER closing graphics
	call error (1, emsg)
end


# ECC_UNMAP -- Unmap images for eccontinuum.

procedure ecc_unmap (in, out, ec)

pointer	in, out			#I IMIO pointers
pointer	ec			#I Task structure pointer

begin
	if (out != NULL && out != in)
	    call imunmap (out)
	if (in != NULL)
	    call imunmap (in)
	if (RGFIT(ec) != NULL)
	    call rg_free (RGFIT(ec))
	if (RGREFIT(ec) != NULL)
	    call rg_free (RGREFIT(ec))
end


# ECC_ICFIT -- Given the image descriptors determine the fitting function
# for each line and output the fit, difference, ratio or coefficients.

int procedure ecc_icfit (in, out, ec, gp, gt, graphics)

pointer	in, out				#I IMIO pointers
pointer	ec				#I Pointer for task switches
pointer	gp				#I GIO pointer
pointer	gt				#I GTOOLS pointer
char	graphics[ARB]			#I Graphics device

pointer	cv, x, wts, raw, data, sp
int	line, nx

int	ecc_getline()
pointer	gopen()
real	ecc_efncr()
extern	ecc_efncr

begin
	nx = IM_LEN(in, 1)
	call smark (sp)
	call salloc (x, nx, TY_REAL)
	call salloc (wts, nx, TY_REAL)
	call salloc (raw, nx, TY_REAL)

	line = 0
	while (ecc_getline (in, out, ec, gt, x, raw, data, line) > 0) {

	    call amovkr (1., Memr[wts], nx)

	    if (QUIET(ec) == NO) {
		if (GRAPH_OPEN(ec) == NO) {
		    gp = gopen (graphics, NEW_FILE, STDGRAPH)
		    GRAPH_OPEN(ec) = YES
		}
		call icg_fit (IC(ec), gp, "cursor", gt, cv, Memr[x], Memr[raw],
		    Memr[wts], nx)
	    } else
		call ic_fit (IC(ec), cv, Memr[x], Memr[raw], Memr[wts], nx,
		    YES, YES, YES, YES)

	    if (LISTONLY(ec) == NO) {
		call cvvector (cv, Memr[x], Memr[data], nx)

		switch (OUTTYPE(ec)) {

		case FIT:
		    call ecc_update (out, line)

		case DIFFERENCE:
		    call asubr (Memr[raw], Memr[data], Memr[data], nx)
		    call ecc_update (out, line)

		case RATIO:
		    call advzr (Memr[raw], Memr[data], Memr[data],nx,ecc_efncr)
		    call ecc_update (out, line)

		default:
		    call error (1, "bad switch in ecc_icfit")

		}
	    }

	    call ecc_power (in, line, cv, gp, ec)

	    call cvfree (cv)

	}

	# This terminates the cursor (GIN) mode echoplex surpression in
	# case the next ecc_immap generates a password prompt from ZFIOKS.
	# Note that any such password prompt (from the kernel!) will
	# now show up on the status line, not the graphics plane.

	if (GRAPH_OPEN(ec) == YES) {
	    call printf ("\r")
	    call flush (STDOUT)
	}

	call sfree (sp)
	return (line)
end


# ECC_GETLINE -- Get image data to be fit.  Returns the line number
# or 0 when the ranges are exhausted.

int procedure ecc_getline (in, out, ec, gt, x, raw, data, line)

pointer	in, out			#I IMIO pointers
pointer	ec			#I Pointer for task switches
pointer	gt			#I GTOOLS pointer
pointer x			#O Image x coordinates
pointer	raw			#O Copy of image data (or log)
pointer	data			#O Image data (or log)
int	line			#U Line number

int	i, nx, ny
bool	waveok
char	ask[LEN_ANS]
pointer	ids, linebuf, rg1, rg2, sp

int	clgwrd(), rg_next(), rg_inrange()
pointer	imgl3r(), impl3r(), rg_ranges(), rg_intersect()
real	ecc_efncr()
extern	ecc_efncr

define	again_		99

begin
	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	if (line == 0) {
	    nx = IM_LEN(in, 1)
	    ny = IM_LEN(in, 2)
	}

again_	if (rg_next (RGFIT(ec), line) == EOF) {
	    line = 0
	    return (line)
	}

	if (PROMPT(ec) == YES) {
	    call clprintf ("ask.p_min", "%s")
		call pargstr (ECC_ANS1X)

	    if (ny == 1 && rg_inrange (RGREFIT(ec), line) == YES) {
		call clprintf ("ask.p_prompt",
		    "Refit %s w/ graph?                   ")

	    } else if (ny == 1) {
		call clprintf ("ask.p_prompt",
		    "Fit %s w/ graph?                     ")

	    } else if (rg_inrange (RGREFIT(ec), line) == YES) {
		call clprintf ("ask.p_prompt",
		    "Refit line %d of %s w/ graph?        ")

	    } else {
		call clprintf ("ask.p_prompt",
		    "Fit line %d of %s w/ graph?          ")
	    }

		if (ny != 1)
		    call pargi (line)
		call pargstr (IM_HDRFILE(in))

	    switch (clgwrd ("ask", ask, LEN_ANS, ECC_ANS1)) {

	    case YES_ONCE:
		QUIET(ec) = NO

	    case NO_ONCE:
		QUIET(ec) = YES

	    case SKIP_ONCE:
		goto again_

	    case YES_ALWAYS:
		QUIET(ec) = NO
		PROMPT(ec) = NO

	    case NO_ALWAYS:
		QUIET(ec) = YES
		PROMPT(ec) = NO

	    case SKIP_ALWAYS:
		if (ny == 1) {
		    line = EOF
		    return (line)
		}

		call clprintf ("ask", "cancel")
		call clprintf ("ask.p_min", "%s")
		    call pargstr (ECC_ANS2X)
		call clprintf ("ask.p_prompt",
		    "Skip what?    (`all' to exit task)   ")

		switch (clgwrd ("ask", ask, LEN_ANS, ECC_ANS2)) {

		case SKIP_ORDER:
		    call clprintf ("ask", "yes")
		    # delete the order from the list
		    call sprintf (Memc[linebuf], SZ_LINE, "%d")
			call pargi (line)

		    rg1 = rg_ranges (Memc[linebuf], 1, ny)
		    call rg_inverse (rg1, 1, ny)
		    rg2 = rg_intersect (RGIN(ec), rg1)
		    call rg_free (rg1)
		    call rg_free (RGIN(ec))

		    RGIN(ec) = rg2
		    goto again_

		case SKIP_IMAGE:
		    call clprintf ("ask", "yes")
		    line = 0
		    return (line)

		case SKIP_ALL:
		    call clprintf ("ask", "yes")
		    line = EOF
		    return (line)

		case SKIP_CANCEL:
		    call clprintf ("ask", "yes")
		    line = line - 1
		    goto again_

		default:
		    call error (1, "bad switch (2) in ecc_getline")

		}

	    default:
		call error (1, "bad switch (1) in ecc_getline")

	    }

	}

	if (LISTONLY(ec) == YES) {
	    data = imgl3r (in, line, 1)
	    if (LOGSCALE(ec) == YES)
		call alogr (Memr[data], Memr[data], nx, ecc_efncr)
	    call amovr (Memr[data], Memr[raw], nx)
	} else {
	    call amovr (Memr[imgl3r (in, line, 1)], Memr[raw], nx)
	    if (LOGSCALE(ec) == YES)
		call alogr (Memr[raw], Memr[raw], nx, ecc_efncr)
	    data = impl3r (out, line, 1)
	    call amovr (Memr[raw], Memr[data], nx)
	}

	if (WAVESCALE(ec) == YES) {
	    call load_ids_hdr (ids, in, line)
	    waveok = (! IS_INDEF(W0(ids)) && ! IS_INDEF(WPC(ids)))
	} else
	    waveok = false

	if (waveok)
	    do i = 1, nx
		Memr[x+i-1] = W0(ids) + WPC(ids) * i
	else
	    do i = 1, nx
		Memr[x+i-1] = i

	if (LOGSCALE(ec) == YES)
	    call alogr (Memr[x], Memr[x], nx, ecc_efncr)

	# Initialize and/or update the icfit descriptor

	if (IC_DESC(ec,line) == NULL) {
	    call ic_open (IC_DESC(ec,line))
	    call ic_copy (IC(ec), IC_DESC(ec,line))
	    call ic_pstr (IC_DESC(ec,line), "sample", "*")
	}

	IC(ec) = IC_DESC(ec,line)

	call ic_putr (IC(ec), "xmin", Memr[x])
	call ic_putr (IC(ec), "xmax", Memr[x+nx-1])

	if (QUIET(ec) == NO) {
	    if (waveok && LOGSCALE(ec) == YES) {
		call ic_pstr (IC(ec), "xlabel", "log wavelength")
		call ic_pstr (IC(ec), "ylabel", "log data")
	    } else if (LOGSCALE(ec) == YES) {
		call ic_pstr (IC(ec), "xlabel", "log column")
		call ic_pstr (IC(ec), "ylabel", "log data")
	    } else if (waveok) {
		call ic_pstr (IC(ec), "xlabel", "wavelength")
		call ic_pstr (IC(ec), "ylabel", "")
	    } else {
		call ic_pstr (IC(ec), "xlabel", "column")
		call ic_pstr (IC(ec), "ylabel", "")
	    }

	    call sprintf (Memc[linebuf], SZ_LINE, "%s, line = %d\n%s")
		call pargstr (IM_HDRFILE(in))
		call pargi (line)
		call pargstr (IM_TITLE(in))

	    call gt_sets (gt, GTTITLE, Memc[linebuf])
	}

	call sfree (sp)
	return (line)
end


# ECC_EFNCR -- Called by advzr on division by zero or by alogr for a
# zero or negative argument.

real procedure ecc_efncr (x)

real	x

begin
	return (0.)
end


# ECC_POWER -- Transform the curfit output into a power series and
# print the coefficients to the logfiles.  This should be modified to
# print the errors as well.  That requires modifying the curfit routine
# cvpower to deal with errors; and adding an icfit routine (or include
# file define) that allows access to the dynamic arrays of sample points
# that are initialized if the sample is less than the whole set of points.

procedure ecc_power (im, line, cv, gp, ec)

pointer	im			#I IMIO descriptor for labeling
int	line			#I Image line number for labeling
pointer	cv			#I CURFIT pointer
pointer	gp			#I GIO pointer for tidy output
pointer ec			#I Pointer for task switches

pointer	ps_coeff, linebuf, sp
int	ncoeffs, i, j, fd

int	cvstati(), strcmp()

begin
	if (NLOGFD(ec) <= 0)
	    return

	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	# cvpower only works with legendre or chebyshev functions

	call ic_gstr (IC(ec), "function", Memc[linebuf], SZ_LINE)
	if (strcmp (Memc[linebuf], "legendre") != 0 &&
	    strcmp (Memc[linebuf], "chebyshev") != 0) {
	    call sfree (sp)
	    return
	}

	if (GRAPH_OPEN(ec) == YES && LOG_TO_STDOUT(ec) == YES) {
	    call gclose (gp)
	    GRAPH_OPEN(ec) = NO
	}

	ncoeffs = cvstati (cv, CVNCOEFF)
	call salloc (ps_coeff, ncoeffs, TY_REAL)
	call cvpower (cv, Memr[ps_coeff], ncoeffs)

	do i = 1, NLOGFD(ec) {
	    fd = Memi[LOGFD(ec)+i-1]

	    call fprintf (fd, "Line %d of %s:\n\n")
		call pargi (line)
		call pargstr (IM_HDRFILE(im))

	    call fprintf (fd, "      coeff           value\n")

	    do j = 1, ncoeffs {
		call fprintf (fd, "\t%d\t%12.5e\n")
		    call pargi (j)
		    call pargr (Memr[ps_coeff+j-1])
	    }

	    call fprintf (fd, "\n")
	    call flush (fd)
	}

	call sfree (sp)
end


# ECC_UPDATE -- Update the keyword with completed orders.  Flush the pixels.

procedure ecc_update (im, line)

pointer	im			#I IMIO pointer
int	line			#I Line just completed

pointer linebuf, rg1, rg2, rgold, sp

pointer	rg_ranges(), rg_union()

begin
	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	# this could be recoded to use "rg_add"

	call sprintf (Memc[linebuf], SZ_LINE, "%d")
	    call pargi (line)
	rg1 = rg_ranges (Memc[linebuf], 1, IM_LEN(im, 2))

	call imgstr (im, ECC_KW, Memc[linebuf], SZ_LINE)
	rg2 = rg_ranges (Memc[linebuf], 1, IM_LEN(im, 2))

	rgold = rg_union (rg1, rg2)
	call rg_encode (rgold, Memc[linebuf], SZ_LINE)
	call impstr (im, ECC_KW, Memc[linebuf])

	call imflush (im)

	call rg_free (rg1)
	call rg_free (rg2)
	call rg_free (rgold)
	call sfree (sp)
end
