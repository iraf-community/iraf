include	<imhdr.h>
include <pkg/gtools.h>
include <pkg/rg.h>
include <math/curfit.h>
include	<error.h>
include	<smw.h>

# SFIT -- Fit a function to spectra and output the fit, difference or
# ratio; or print the power series coefficients of the fit.  The fitting
# parameters may be set interactively using the icfit package.

# Image header keywords for saving the previous fit

define	SFT_KW		"SFIT"
define	SFT_KWB		"SFITB"

# Choices for the type of output

define	OUT_TYPES	"|data|fit|difference|ratio|"

define	DATA		1
define	FIT		2
define	DIFFERENCE	3
define	RATIO		4

# Choices for the interactive prompts
# (the 1st define is for clgwrd (strdic), the 2nd for CL enumeration)
# Note that the CL assumes that the separator is `|'.

define	SFT_ANS1	"|yes|no|skip|YES|NO|SKIP|"
define	SFT_ANS1X	"yes|no|skip|YES|NO|SKIP"

define	SFT_ANS2	"|spectrum|image|all|cancel|"
define	SFT_ANS2X	"spectrum|image|all|cancel"

define	LEN_ANS		7

define	YES_ONCE	1
define	NO_ONCE		2
define	SKIP_ONCE	3
define	YES_ALWAYS	4
define	NO_ALWAYS	5
define	SKIP_ALWAYS	6

define	SKIP_SPEC	1
define	SKIP_IMAGE	2
define	SKIP_ALL	3
define	SKIP_CANCEL	4

# Switches and pointers

define	SFT_OFF		22

define	INTERACTIVE	Memi[$1]		# all spectra are noninteractive
define	REPLACE		Memi[$1+1]		# replace rejected points?
define	WAVESCALE	Memi[$1+2]		# X is wavelength if possible
define	LOGSCALE	Memi[$1+3]		# axes are logarithmic
define	OVERRIDE	Memi[$1+4]		# allow lines to be redone
define	LISTONLY	Memi[$1+5]		# don't modify images
define	OUTTYPE		Memi[$1+6]		# output type code

define	GRAPH_OPEN	Memi[$1+7]		# keep track of gopen
define	LOG_TO_STDOUT	Memi[$1+8]		# STDOUT/ERR is used
define	PROMPT		Memi[$1+9]		# prompt flag
define	QUIET		Memi[$1+10]		# quiet flag

define	RGIN		Memi[$1+11]		# lines specified
define	RGFIT		Memi[$1+12]		# all lines to fit
define	RGREFIT		Memi[$1+13]		# those to fit again
define	RGINB		Memi[$1+14]		# bands specified
define	RGFITB		Memi[$1+15]		# all bands to fit
define	RGREFITB	Memi[$1+16]		# those to fit again

define	NLOGFD		Memi[$1+17]		# number of logfiles
define	LOGFD		Memi[$1+18]		# array of logfiles

define	IC		Memi[$1+19]		# current ic descriptor
define	YMAX		Memi[$1+20]		# max number of lines
define	BMAX		Memi[$1+21]		# max number of lines
define	IC_DESC		Memi[$1+SFT_OFF+($3-1)*YMAX($1)+$2-1]	# ic descriptors


# T_SFIT -- Entry point for the task.  Read parameters,
# initialize structures and loop over the image templates.

procedure t_sfit ()

pointer	listin, listout, input, output, graphics
pointer	sf, gp, gt, in, out, mw, sh, sp
int	stat

int	sft_icfit(), imtgetim(), gt_init(), imtlen()
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
	iferr (call sft_init (listin, listout, sf)) {
	    call imtclose (listin)
	    if (listout != NULL)
		call imtclose (listout)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	# The graphics pointers are passed explicitly
	if (INTERACTIVE(sf) == YES) {
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
		call sft_immap (Memc[input], Memc[output],
		    in, out, mw, sh, sf, gp)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    stat = sft_icfit (in, out, mw, sh, sf, gp, gt, Memc[graphics])

	    call sft_unmap (in, out, mw, sh, sf)

	    if (stat == EOF)
		break
	}

	if (INTERACTIVE(sf) == YES)
	    call gt_free (gt)
	if (GRAPH_OPEN(sf) == YES)
	    call gclose (gp)

	call sft_close (sf)
	call imtclose (listin)
	if (listout != NULL)
	    call imtclose (listout)
	call sfree (sp)
end


# SFT_INIT -- Initialize templates, ranges, logfiles, type, icfit descriptors.

procedure sft_init (listin, listout, sf)

pointer	listin, listout		#I Image template descriptors
pointer	sf			#I Pointer to task switches

pointer	input, output, im, sp, mw
int	ymax, bmax, i, j

real	clgetr()
bool	clgetb()
int	clgwrd(), clgeti(), btoi(), strlen()
int	rg_next(), imtgetim(), imaccess(), xt_logopen()
pointer	rg_ranges(), immap(), smw_openim()
errchk	immap, smw_openim

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	sf = NULL
	iferr {

	    # find the maximum number of lines and bands (spectra)
	    ymax = 0
	    bmax = 0

	    while (imtgetim (listin, Memc[input], SZ_LINE) != EOF)
		if (imaccess (Memc[input], READ_ONLY) == YES) {
		    im = immap (Memc[input], READ_ONLY, 0)
		    mw = smw_openim (im)
		    ymax = max (SMW_LLEN(mw,2), ymax)
		    bmax = max (SMW_LLEN(mw,3), bmax)
		    call smw_close (mw)
		    call imunmap (im)
		}
	    call imtrew (listin)

	    if (listout != NULL) {
		while (imtgetim (listout, Memc[output], SZ_FNAME) != EOF)
		    if (imaccess (Memc[output], READ_ONLY) == YES) {
			im = immap (Memc[output], READ_ONLY, 0)
			mw = smw_openim (im)
			ymax = max (SMW_LLEN(mw,2), ymax)
			bmax = max (SMW_LLEN(mw,3), bmax)
			call smw_close (mw)
			call imunmap (im)
		    }
		call imtrew (listout)
	    }

	    # allocate space for the task switch structure
	    call malloc (sf, SFT_OFF + ymax * bmax, TY_STRUCT)

	    YMAX(sf) = ymax
	    BMAX(sf) = bmax

	    # NULL the pointers for error handling
	    RGIN(sf)	= NULL
	    RGINB(sf)	= NULL
	    NLOGFD(sf)	= 0
	    do j = 1, BMAX(sf)
		do i = 1, YMAX(sf)
		    IC_DESC(sf,i,j) = NULL

	    # Set the switches
	    INTERACTIVE(sf) = btoi (clgetb ("interactive"))
	    REPLACE(sf) = btoi (clgetb ("replace"))
	    WAVESCALE(sf) = btoi (clgetb ("wavescale"))
	    LOGSCALE(sf) = btoi (clgetb ("logscale"))
	    OVERRIDE(sf) = btoi (clgetb ("override"))
	    LISTONLY(sf) = btoi (clgetb ("listonly"))
	    GRAPH_OPEN(sf) = NO
	    PROMPT(sf) = INTERACTIVE(sf)
	    QUIET(sf) = btoi (INTERACTIVE(sf) == NO)

	    # Expand the range specification, allow either hyphens or colons

	    call clgstr ("lines", Memc[input], SZ_LINE)
	    do i = 1, strlen (Memc[input])
		if (Memc[input+i-1] == '-')
		    Memc[input+i-1] =  ':'
		else if (Memc[input+i-1] == 'x' || Memc[input+i-1] == 'X')
		    call error (1, "Range step (`x' notation) not implemented")

	    RGIN(sf) = rg_ranges (Memc[input], 1, YMAX(sf))
	    call rg_order (RGIN(sf))
	    call rg_merge (RGIN(sf))

	    call clgstr ("bands", Memc[input], SZ_LINE)
	    do i = 1, strlen (Memc[input])
		if (Memc[input+i-1] == '-')
		    Memc[input+i-1] =  ':'
		else if (Memc[input+i-1] == 'x' || Memc[input+i-1] == 'X')
		    call error (1, "Range step (`x' notation) not implemented")

	    RGINB(sf) = rg_ranges (Memc[input], 1, BMAX(sf))
	    call rg_order (RGINB(sf))
	    call rg_merge (RGINB(sf))

	    i = 0
	    j = 0
	    if (rg_next (RGIN(sf), i) == EOF || rg_next (RGINB(sf), j) == EOF)
		call error (1, "With range specification for `lines or bands'")
	    else {
		# Open the initial icfit descriptor
		call ic_open (IC(sf))

		call clgstr ("sample", Memc[input], SZ_LINE)
		call ic_pstr (IC(sf), "sample", Memc[input])
		call clgstr ("function", Memc[input], SZ_LINE)
		call ic_pstr (IC(sf), "function", Memc[input])

		call ic_puti (IC(sf), "naverage", clgeti ("naverage"))
		call ic_puti (IC(sf), "order", clgeti ("order"))
		call ic_putr (IC(sf), "low", clgetr ("low_reject"))
		call ic_putr (IC(sf), "high", clgetr ("high_reject"))
		call ic_puti (IC(sf), "niterate", clgeti ("niterate"))
		call ic_putr (IC(sf), "grow", clgetr ("grow"))
		call ic_puti (IC(sf), "markrej", btoi (clgetb ("markrej")))

		IC_DESC(sf,i,j) = IC(sf)
	    }

	    # Get the desired output type
	    OUTTYPE(sf) = clgwrd ("type", Memc[input], SZ_LINE, OUT_TYPES)

	    # Open the logfiles
	    NLOGFD(sf) = xt_logopen ("logfiles", "SFIT:", LOGFD(sf),
		LOG_TO_STDOUT(sf))

	} then {
	    call sfree (sp)
	    call sft_close (sf)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
	return
end


# SFT_CLOSE -- Close the various descriptors.

procedure sft_close (sf)

pointer	sf			#I Pointer to task switches

int	i, j

begin
	if (sf != NULL) {
	    if (RGIN(sf) != NULL)
		call rg_free (RGIN(sf))
	    if (RGINB(sf) != NULL)
		call rg_free (RGINB(sf))
	    if (NLOGFD(sf) != 0)
		call xt_logclose (LOGFD(sf), NLOGFD(sf), "END:")
	    do j = 1, BMAX(sf)
		do i = 1, YMAX(sf)
		    if (IC_DESC(sf,i,j) != NULL)
			call ic_closer (IC_DESC(sf,i,j))
	    call mfree (sf, TY_STRUCT)
	}
end


# SFT_IMMAP -- Map images for sfit.

procedure sft_immap (input, output, in, out, mw, sh, sf, gp)

char	input[ARB]		#I Input image name
char	output[ARB]		#I Output image name
pointer	in, out			#O IMIO pointers
pointer	mw			#O MWCS pointer
pointer	sh			#O SHDR pointer
pointer	sf			#I Pointer for task switches
pointer	gp			#I GIO pointer

int	i, ax1, ax2, ax3
pointer	inroot, insect, outroot, outsect, b1, b2
pointer	sp, inranges, outranges
pointer	rgin, rgout, rgtmp, rgtmpb
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
char	emsg[SZ_LINE]

int	imaccess(), imaccf(), imgnlr(), impnlr(), strcmp()
pointer	immap(), smw_openim()
pointer	rg_ranges(), rg_window(), rg_union(), rg_intersect()
errchk	immap, smw_openim

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
	mw = NULL
	sh = NULL
	RGFIT(sf) = NULL
	RGREFIT(sf) = NULL
	RGFITB(sf) = NULL
	RGREFITB(sf) = NULL

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

	} else if (LISTONLY(sf) == YES) {

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
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL

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

	do i = 4, IM_NDIM(in)
	    if (IM_LEN(in, i) != 1) {
		call sprintf (emsg, SZ_LINE, "Too many dimensions for %s")
		    call pargstr (Memc[inroot])
		goto err_
	    }

	if (imaccf (in, SFT_KW) == YES)
	    call imgstr (in, SFT_KW, Memc[inranges], SZ_LINE)
	else
	    call strcpy ("", Memc[inranges], SZ_LINE)

	if (imaccf (out, SFT_KW) == YES)
	    call imgstr (out, SFT_KW, Memc[outranges], SZ_LINE)
	else {
	    call strcpy ("", Memc[outranges], SZ_LINE)
	    call imastr (out, SFT_KW, Memc[outranges])
	}

	mw = smw_openim (in)
	ax1 = SMW_LLEN(mw,1)
	ax2 = SMW_LLEN(mw,2)
	ax3 = SMW_LLEN(mw,3)

	rgin = rg_ranges (Memc[inranges], 1, ax2)
	rgout = rg_ranges (Memc[outranges], 1, ax2)
	rgtmp = rg_union (rgin, rgout)
	call rg_free (rgin)
	call rg_free (rgout)

	if (imaccf (in, SFT_KWB) == YES)
	    call imgstr (in, SFT_KWB, Memc[inranges], SZ_LINE)
	else
	    call strcpy ("", Memc[inranges], SZ_LINE)

	if (imaccf (out, SFT_KWB) == YES)
	    call imgstr (out, SFT_KWB, Memc[outranges], SZ_LINE)
	else {
	    call strcpy ("", Memc[outranges], SZ_LINE)
	    call imastr (out, SFT_KWB, Memc[outranges])
	}

	rgin = rg_ranges (Memc[inranges], 1, ax3)
	rgout = rg_ranges (Memc[outranges], 1, ax3)
	rgtmpb = rg_union (rgin, rgout)
	call rg_free (rgin)
	call rg_free (rgout)

	if (OVERRIDE(sf) == YES) {
	    RGFIT(sf) = rg_window (RGIN(sf), 1, ax2)
	    RGREFIT(sf) = rgtmp
	    RGFITB(sf) = rg_window (RGINB(sf), 1, ax3)
	    RGREFITB(sf) = rgtmpb
	} else {
	    call rg_inverse (rgtmp, 1, ax2)
	    RGFIT(sf) = rg_intersect (RGIN(sf), rgtmp)
	    RGREFIT(sf) = rg_ranges ("0", 1, 2)
	    call rg_free (rgtmp)
	    #call rg_inverse (rgtmpb, 1, ax3)
	    #RGFITB(sf) = rg_intersect (RGINB(sf), rgtmpb)
	    #RGREFITB(sf) = rg_ranges ("0", 1, 2)
	    #call rg_free (rgtmpb)
	    RGFITB(sf) = rg_window (RGINB(sf), 1, ax3)
	    RGREFITB(sf) = rgtmpb
	}

	if (RG_NPTS(RGFIT(sf)) <= 0) {
	    call sprintf (emsg, SZ_LINE, "No lines left to fit for %s")
		call pargstr (Memc[inroot])
	    goto err_
	}

	call sfree (sp)
	return

err_	call sfree (sp)
	call sft_unmap (in, out, mw, sh, sf)
	if (GRAPH_OPEN(sf) == YES) {
	    call gclose (gp)
	    GRAPH_OPEN(sf) = NO
	}
	# STDERR should get flushed AFTER closing graphics
	call error (1, emsg)
end


# SFT_UNMAP -- Unmap images for sfit.

procedure sft_unmap (in, out, mw, sh, sf)

pointer	in, out			#I IMIO pointers
pointer	mw			#I MWCS pointer
pointer	sh			#I SHDR pointer
pointer	sf			#I Task structure pointer

begin
	call shdr_close (sh)
	if (mw != NULL)
	    call smw_close (mw)
	if (out != NULL && out != in)
	    call imunmap (out)
	if (in != NULL)
	    call imunmap (in)
	if (RGFIT(sf) != NULL)
	    call rg_free (RGFIT(sf))
	if (RGREFIT(sf) != NULL)
	    call rg_free (RGREFIT(sf))
	if (RGFITB(sf) != NULL)
	    call rg_free (RGFITB(sf))
	if (RGREFITB(sf) != NULL)
	    call rg_free (RGREFITB(sf))
end


# SFT_ICFIT -- Given the image descriptors determine the fitting function
# for each line and output the fit, difference, ratio or coefficients.

int procedure sft_icfit (in, out, mw, sh, sf, gp, gt, graphics)

pointer	in, out				#I IMIO pointers
pointer	mw				#I MWCS pointer
pointer	sh				#I SHDR pointer
pointer	sf				#I Pointer for task switches
pointer	gp				#I GIO pointer
pointer	gt				#I GTOOLS pointer
char	graphics[ARB]			#I Graphics device

pointer	sp, wts, cv, data
int	line, band, i, j, n

int	sft_getline()
pointer	gopen(), imps3r()
real	sft_efncr()
extern	sft_efncr

begin
	call smark (sp)
	call salloc (wts, SMW_LLEN(mw,1), TY_REAL)

	line = 0
	band = 0
	while (sft_getline (in, mw, sh, sf, gt, line, band) != EOF) {

	    call amovkr (1., Memr[wts], SN(sh))

	    if (QUIET(sf) == NO) {
		if (GRAPH_OPEN(sf) == NO) {
		    gp = gopen (graphics, NEW_FILE, STDGRAPH)
		    GRAPH_OPEN(sf) = YES
		}
		call icg_fit (IC(sf), gp, "cursor", gt, cv, Memr[SX(sh)],
		    Memr[SY(sh)], Memr[wts], SN(sh))
	    } else
		call ic_fit (IC(sf), cv, Memr[SX(sh)], Memr[SY(sh)],
		    Memr[wts], SN(sh), YES, YES, YES, YES)

	    if (LISTONLY(sf) == NO) {
		i = LINDEX(sh,1)
		j = LINDEX(sh,2)
		n = SMW_LLEN(mw,1)
		switch (SMW_LAXIS(mw,1)) {
		case 1:
		    data = imps3r (out, 1, n, i, i, j, j)
		case 2:
		    data = imps3r (out, i, i, 1, n, j, j)
		case 3:
		    data = imps3r (out, i, i, j, j,  1, n)
		}
		if (SN(sh) < n)
		    call aclrr (Memr[data], n)

		switch (OUTTYPE(sf)) {
		case DATA:
		    if (REPLACE(sf) == YES)
			call ic_clean (IC(sf), cv, Memr[SX(sh)], Memr[SY(sh)],
			    Memr[wts], SN(sh))
		    call amovr (Memr[SY(sh)], Memr[data], SN(sh))
		    call sft_update (out, mw, line, band)
		case FIT:
		    call cvvector (cv, Memr[SX(sh)], Memr[data], SN(sh))
		    call sft_update (out, mw, line, band)
		case DIFFERENCE:
		    call cvvector (cv, Memr[SX(sh)], Memr[data], SN(sh))
		    if (REPLACE(sf) == YES)
			call ic_clean (IC(sf), cv, Memr[SX(sh)], Memr[SY(sh)],
			    Memr[wts], SN(sh))
		    call asubr (Memr[SY(sh)], Memr[data], Memr[data], SN(sh))
		    call sft_update (out, mw, line, band)
		case RATIO:
		    call cvvector (cv, Memr[SX(sh)], Memr[data], SN(sh))
		    if (REPLACE(sf) == YES)
			call ic_clean (IC(sf), cv, Memr[SX(sh)], Memr[SY(sh)],
			    Memr[wts], SN(sh))
		    call advzr (Memr[SY(sh)], Memr[data], Memr[data], SN(sh),
			sft_efncr)
		    call sft_update (out, mw, line, band)
		default:
		    call error (1, "bad switch in sft_icfit")
		}
	    }

	    call sft_power (in, line, cv, gp, sf)
	    call cvfree (cv)

	}

	# This terminates the cursor (GIN) mode echoplex suppression in
	# case the next sft_immap generates a password prompt from ZFIOKS.
	# Note that any such password prompt (from the kernel!) will
	# now show up on the status line, not the graphics plane.

	if (GRAPH_OPEN(sf) == YES) {
	    call printf ("\r")
	    call flush (STDOUT)
	}

	call sfree (sp)
	return (line)
end


# SFT_GETLINE -- Get image data to be fit.  Returns the line and band numbers.
# Returns EOF when done.

int procedure sft_getline (in, mw, sh, sf, gt, line, band)

pointer	in			#I IMIO pointer
pointer	mw			#I MWCS pointer
pointer	sh			#I SHDR pointer
pointer	sf			#I Pointer for task switches
pointer	gt			#I GTOOLS pointer
int	line			#U Line number
int	band			#U Band number

int	i
bool	waveok
char	ask[LEN_ANS]
pointer	linebuf, rg1, rg2, sp

int	clgwrd(), rg_next(), rg_inrange()
pointer	rg_ranges(), rg_intersect()
real	sft_efncr()
extern	sft_efncr
errchk	shdr_open

define	again_		99

begin
	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	if (band == 0)
	    if (rg_next (RGFITB(sf), band) == EOF)
		return (EOF)

again_	if (rg_next (RGFIT(sf), line) == EOF) {
	    line = 0
	    if (rg_next (RGFITB(sf), band) == EOF)
		return (EOF)
	    goto again_
	}

	if (PROMPT(sf) == YES) {
	    call clprintf ("ask.p_min", "%s")
		call pargstr (SFT_ANS1X)

	    if (rg_inrange (RGREFIT(sf), line) == YES &&
		rg_inrange (RGREFITB(sf), band) == YES) {
		call clprintf ("ask.p_prompt",
		    "Refit [%d,%d] of %s w/ graph?        ")
	    } else {
		call clprintf ("ask.p_prompt",
		    "Fit [%d,%d] of %s w/ graph?          ")
	    }
	    call pargi (line)
	    call pargi (band)
	    call pargstr (IM_HDRFILE(in))

	    switch (clgwrd ("ask", ask, LEN_ANS, SFT_ANS1)) {

	    case YES_ONCE:
		QUIET(sf) = NO

	    case NO_ONCE:
		QUIET(sf) = YES

	    case SKIP_ONCE:
		goto again_

	    case YES_ALWAYS:
		QUIET(sf) = NO
		PROMPT(sf) = NO

	    case NO_ALWAYS:
		QUIET(sf) = YES
		PROMPT(sf) = NO

	    case SKIP_ALWAYS:
		call clprintf ("ask", "cancel")
		call clprintf ("ask.p_min", "%s")
		    call pargstr (SFT_ANS2X)
		call clprintf ("ask.p_prompt",
		    "Skip what?    (`all' to exit task)   ")

		switch (clgwrd ("ask", ask, LEN_ANS, SFT_ANS2)) {

		case SKIP_SPEC:
		    call clprintf ("ask", "yes")
		    # delete the spectrum from the list
		    call sprintf (Memc[linebuf], SZ_LINE, "%d")
			call pargi (line)

		    rg1 = rg_ranges (Memc[linebuf], 1, SMW_LLEN(mw,2))
		    call rg_inverse (rg1, 1, SMW_LLEN(mw,2))
		    rg2 = rg_intersect (RGIN(sf), rg1)
		    call rg_free (rg1)
		    call rg_free (RGIN(sf))

		    RGIN(sf) = rg2
		    goto again_

		case SKIP_IMAGE:
		    call clprintf ("ask", "yes")
		    return (EOF)

		case SKIP_ALL:
		    call clprintf ("ask", "yes")
		    return (EOF)

		case SKIP_CANCEL:
		    call clprintf ("ask", "yes")
		    line = line - 1
		    goto again_

		default:
		    call error (1, "bad switch (2) in sft_getline")

		}

	    default:
		call error (1, "bad switch (1) in sft_getline")

	    }

	}

	call shdr_open (in, mw, line, band, INDEFI, SHDATA, sh)

	if (LOGSCALE(sf) == YES)
	    call alogr (Memr[SY(sh)], Memr[SY(sh)], SN(sh), sft_efncr)

	if (WAVESCALE(sf) == YES) {
	    waveok = true
	} else
	    waveok = false

	if (!waveok)
	    do i = 1, SN(sh)
		Memr[SX(sh)+i-1] = i

	if (LOGSCALE(sf) == YES)
	    call alogr (Memr[SX(sh)], Memr[SX(sh)], SN(sh), sft_efncr)

	# Initialize and/or update the icfit descriptor

	if (IC_DESC(sf,line,band) == NULL) {
	    call ic_open (IC_DESC(sf,line,band))
	    call ic_copy (IC(sf), IC_DESC(sf,line,band))
	    #call ic_pstr (IC_DESC(sf,line,band), "sample", "*")
	}

	IC(sf) = IC_DESC(sf,line,band)

	call ic_putr (IC(sf), "xmin", min (Memr[SX(sh)], Memr[SX(sh)+SN(sh)-1]))
	call ic_putr (IC(sf), "xmax", max (Memr[SX(sh)], Memr[SX(sh)+SN(sh)-1]))

	if (QUIET(sf) == NO) {
	    if (waveok && LOGSCALE(sf) == YES) {
		call ic_pstr (IC(sf), "xlabel", "log wavelength")
		call ic_pstr (IC(sf), "ylabel", "log data")
	    } else if (LOGSCALE(sf) == YES) {
		call ic_pstr (IC(sf), "xlabel", "log column")
		call ic_pstr (IC(sf), "ylabel", "log data")
	    } else if (waveok) {
		call ic_pstr (IC(sf), "xlabel", "wavelength")
		call ic_pstr (IC(sf), "ylabel", "")
	    } else {
		call ic_pstr (IC(sf), "xlabel", "column")
		call ic_pstr (IC(sf), "ylabel", "")
	    }

	    call sprintf (Memc[linebuf], SZ_LINE, "%s, [%d,%d]\n%s")
		call pargstr (IM_HDRFILE(in))
		call pargi (line)
		call pargi (band)
		call pargstr (TITLE(sh))

	    call gt_sets (gt, GTTITLE, Memc[linebuf])
	}

	call sfree (sp)
	return (OK)
end


# SFT_EFNCR -- Called by advzr on division by zero or by alogr for a
# zero or negative argument.

real procedure sft_efncr (x)

real	x

begin
	return (0.)
end


# SFT_POWER -- Transform the curfit output into a power series and
# print the coefficients to the logfiles.  This should be modified to
# print the errors as well.  That requires modifying the curfit routine
# cvpower to deal with errors; and adding an icfit routine (or include
# file define) that allows access to the dynamic arrays of sample points
# that are initialized if the sample is less than the whole set of points.

procedure sft_power (im, line, cv, gp, sf)

pointer	im			#I IMIO descriptor for labeling
int	line			#I Image line number for labeling
pointer	cv			#I CURFIT pointer
pointer	gp			#I GIO pointer for tidy output
pointer sf			#I Pointer for task switches

pointer	ps_coeff, linebuf, sp
int	ncoeffs, i, j, fd

int	cvstati(), strcmp()

begin
	if (NLOGFD(sf) <= 0)
	    return

	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	# cvpower only works with legendre or chebyshev functions

	call ic_gstr (IC(sf), "function", Memc[linebuf], SZ_LINE)
	if (strcmp (Memc[linebuf], "legendre") != 0 &&
	    strcmp (Memc[linebuf], "chebyshev") != 0) {
	    call sfree (sp)
	    return
	}

	if (GRAPH_OPEN(sf) == YES && LOG_TO_STDOUT(sf) == YES) {
	    call gclose (gp)
	    GRAPH_OPEN(sf) = NO
	}

	ncoeffs = cvstati (cv, CVNCOEFF)
	call salloc (ps_coeff, ncoeffs, TY_REAL)
	call cvpower (cv, Memr[ps_coeff], ncoeffs)

	do i = 1, NLOGFD(sf) {
	    fd = Memi[LOGFD(sf)+i-1]

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


# SFT_UPDATE -- Update the keyword with completed spectrum.  Flush the pixels.

procedure sft_update (im, mw, line, band)

pointer	im			#I IMIO pointer
pointer	mw			#I MWCS pointer
int	line			#I Line just completed
int	band			#I Band just completed

pointer linebuf, rg1, rg2, rgold, sp

pointer	rg_ranges(), rg_union()

begin
	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	# this could be recoded to use "rg_add"

	call sprintf (Memc[linebuf], SZ_LINE, "%d")
	    call pargi (line)
	rg1 = rg_ranges (Memc[linebuf], 1, SMW_LLEN(mw,2))

	call imgstr (im, SFT_KW, Memc[linebuf], SZ_LINE)
	rg2 = rg_ranges (Memc[linebuf], 1, SMW_LLEN(mw,2))

	rgold = rg_union (rg1, rg2)
	call rg_encode (rgold, Memc[linebuf], SZ_LINE)
	call impstr (im, SFT_KW, Memc[linebuf])

	call rg_free (rg1)
	call rg_free (rg2)
	call rg_free (rgold)

	call sprintf (Memc[linebuf], SZ_LINE, "%d")
	    call pargi (band)
	rg1 = rg_ranges (Memc[linebuf], 1, SMW_LLEN(mw,3))

	call imgstr (im, SFT_KWB, Memc[linebuf], SZ_LINE)
	rg2 = rg_ranges (Memc[linebuf], 1, SMW_LLEN(mw,3))

	rgold = rg_union (rg1, rg2)
	call rg_encode (rgold, Memc[linebuf], SZ_LINE)
	call impstr (im, SFT_KWB, Memc[linebuf])

	call rg_free (rg1)
	call rg_free (rg2)
	call rg_free (rgold)

	call imflush (im)
	call sfree (sp)
end
