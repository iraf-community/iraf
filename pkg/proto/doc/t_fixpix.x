include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>


# T_FIXPIX -- Interpolate over bad columns and lines.

procedure t_fixpix ()

int	ilist			# List of images
int	mlist			# List of masks
int	linterp			# Mask code for line interpolation
int	cinterp			# Mask code for column interpolation
bool	verbose			# Verbose output?
int	fd			# List pixels?

int	i, nc, nl
long	v[IM_MAXDIM]
pointer	sp, imname, pmname, str1, str2, im, pmim, pm, fp, buf, tmp

bool	clgetb(), pm_linenotempty()
int	imtopenp(), imtgetim(), imtlen(), clgeti(), imaccf(), imstati()
long	clktime()
pointer	immap(), yt_pmmap(), xt_fpinit()
pointer	xt_fps(), xt_fpi(), xt_fpl(), xt_fpr(), xt_fpd()
pointer	impl2s(), impl2i(), impl2l(), impl2r(), impl2d()
errchk	immap, yt_pmmap, xt_fpinit

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (pmname, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Get task parameters
	ilist = imtopenp ("images")
	mlist = imtopenp ("masks")
	linterp = clgeti ("linterp")
	cinterp = clgeti ("cinterp")
	verbose = clgetb ("verbose")
	if (verbose && clgetb ("pixels"))
	    fd = STDOUT
	else
	    fd = NULL

	i = imtlen (mlist)
	if (i == 0 || (i > 1 && i != imtlen (ilist))) {
	    call imtclose (ilist)
	    call imtclose (mlist)
	    call sfree (sp)
	    call error (1, "Image and mask lists are incompatible")
	}
	if (!IS_INDEFI(linterp) && !IS_INDEFI(cinterp) &&
	    linterp>0 && linterp==cinterp) {
	    call imtclose (ilist)
	    call imtclose (mlist)
	    call sfree (sp)
	    call error (1, "Interpolation codes are the same")
	}

	# Fix the pixels.
	while (imtgetim (ilist, Memc[imname], SZ_FNAME) != EOF) {
	    if (imtgetim (mlist, Memc[pmname], SZ_FNAME) == EOF) {
		call imtrew (mlist)
		if (imtgetim (mlist, Memc[pmname], SZ_FNAME) == EOF)
		    call error (1, "Error in mask list")
	    }
	    iferr {
		im = NULL
		pmim = NULL
		fp = NULL
	        tmp = immap (Memc[imname], READ_WRITE, 0)
		im = tmp
		tmp = yt_pmmap (Memc[pmname], im, Memc[pmname], SZ_FNAME);
		pmim = tmp
		pm = imstati (pmim, IM_PMDES)

		nc = IM_LEN(im,1)
		nl = IM_LEN(im,2)
		tmp= xt_fpinit (pm, linterp, cinterp)
		fp = tmp

		if (verbose || fd != NULL) {
		    call printf ("FIXPIX: image %s with mask %s\n")
			call pargstr (Memc[imname])
			call pargstr (Memc[pmname])
		    call flush (STDOUT)
		}

		call amovkl (long(1), v, IM_MAXDIM)
		if (fp != NULL) {
		    do i = 1, nl {
			v[2] = i
			if (!pm_linenotempty (pm, v))
			    next
			switch (IM_PIXTYPE(im)) {
			case TY_SHORT:
			    tmp = xt_fps (fp, im, i, fd)
			    buf = impl2s (im, i)
			    call amovs (Mems[tmp], Mems[buf], nc)
			case TY_INT:
			    tmp = xt_fpi (fp, im, i, fd)
			    buf = impl2i (im, i)
			    call amovi (Memi[tmp], Memi[buf], nc)
			case TY_USHORT, TY_LONG:
			    tmp = xt_fpl (fp, im, i, fd)
			    buf = impl2l (im, i)
			    call amovl (Meml[tmp], Meml[buf], nc)
			case TY_REAL, TY_COMPLEX:
			    tmp = xt_fpr (fp, im, i, fd)
			    buf = impl2r (im, i)
			    call amovr (Memr[tmp], Memr[buf], nc)
			case TY_DOUBLE:
			    tmp = xt_fpd (fp, im, i, fd)
			    buf = impl2d (im, i)
			    call amovd (Memd[tmp], Memd[buf], nc)
			}
		    }
		}

		# Add log to header.
		call cnvdate (clktime(0), Memc[str2], SZ_LINE)
		call sprintf (Memc[str1], SZ_LINE, "%s Bad pixel file is %s")
			call pargstr (Memc[str2])
			call pargstr (Memc[pmname])
		if (imaccf (im, "FIXPIX") == NO)
		    call imastr (im, "FIXPIX", Memc[str1])
		else {
		    do i = 2, 99 {
			call sprintf (Memc[str2], SZ_LINE, "FIXPIX%02d")
			    call pargi (i)
			if (imaccf (im, Memc[str2]) == NO) {
			    call imastr (im, Memc[str2], Memc[str1])
			    break
			}
		    }
		}
	    } then
		call erract (EA_WARN)

	    if (fp != NULL)
		call xt_fpfree (fp)
	    if (pmim != NULL)
		call xt_pmunmap (pmim)
	    if (im != NULL)
		call imunmap (im)
	}

	call imtclose (ilist)
	call imtclose (mlist)
	call sfree (sp)
end
