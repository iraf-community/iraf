include	<fset.h>
include	<imhdr.h>
include	<imio.h>

define	OUTTYPES "|filter|difference|ratio|"
define	OT_NTYPES	3	# Number of output types
define	OT_FILTER	1	# Output filter values
define	OT_DIFF		2	# Output difference
define	OT_RATIO	3	# Output ratio

define	STORETYPES	"|real|short|"

define	NSAMPLE		100000	# Number of pixels to sample for mode
define	NLINES		10	# Minimum number of lines to sample
define	FRAC		0.9	# Fraction of sorted sample for mean


# RUNMED -- Apply running median to a list of images.

procedure runmed (input, output, window, masks, inmaskkey, outmaskkey,
	outtype, exclude, nclip, navg, scale, normscale, outscale, blank,
	storetype, verbose)

pointer	input			#I List of input images
pointer	output			#I List of output images
int	window			#I Filter window
pointer	masks			#I List of output masks
char	inmaskkey[ARB]		#I Input mask keyword
char	outmaskkey[ARB]		#I Output mask keyword
char	outtype[ARB]		#I Output type
bool	exclude			#I Exclude input image?
real	nclip			#I Clipping factor
int	navg			#I Number of values to average
char	scale[ARB]		#I Scale specification
bool	normscale		#I Normalize scales to first scale?
bool	outscale		#I Scale output?
real	blank			#I Blank values
char	storetype[ARB]		#I Storage type
bool	verbose			#I Verbose?

int	i, j, nims, nc, nl, iindex, oindex, eindex, stat, halfwin, ot, stype
int	fd, len[IM_MAXDIM]
short	nused
real	iscl, iscl1, oscl, val, mean, sigma, median, mode
pointer	in, im, out, om, idata, imdata, imdata1, odata, omdata, omdata1, hdr, rm
pointer	sp, inname, outname, imtemp, imname, omname
pointer	iline, imline, oline, omline, hdrs, scales, sample, str, rms

bool	streq(), strne(), aveqi()
int	open(), fscan(), nscan(), nowhite(), strdic()
int	imtlen(), imtrgetim()
int	xt_sampler(), xt_samples(), imgnlr(), impnlr(), imgnls(), impnls()
real	imgetr(), rm_med(), rm_gmed(), rm_gdata()
pointer	immap(), yt_mappm(), rm_open()
errchk	immap, yt_mappm

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Check input data for errors.
	nims = imtlen (input)
	if (nims < 0)
	    call error (1, "No input images specified")
	if (imtlen (output) != nims)
	    call error (2, "Number of input and output images don't agree")
	if (window < 0)
	    call error (3, "Window size error")
	if (window > nims)
	    call error (4, "Window size exceeds number of images")
	if (imtlen (masks) > 0 && imtlen (masks) != nims)
	    call error (5, "Number of output masks and images don't agree")
	ot = strdic (outtype, Memc[str], SZ_LINE, OUTTYPES)
	if (ot < 1 || ot > OT_NTYPES)
	    call error (7, "Unknown output type")
	if (navg < 0)
	    call error(8,
		"Number of central pixels to average must be positive")
	if (strne (scale, "none") && strne (scale, "mode") &&
	    scale[1] != '!' && scale[1] != '@')
	    call error (11, "Bad scale specification")
	if (IS_INDEFR(blank))
	    call error (12, "Blank value may not be INDEF")
	switch (strdic (storetype, Memc[str], SZ_LINE, STORETYPES)) {
	case 1:
	    stype = TY_REAL
	case 2:
	    stype = TY_SHORT
	default:
	    call error (14, "Unsupported storage type")
	}

	# Open and check scale file if one is specified.
	if (scale[1] == '@') {
	    fd = open (scale[2], READ_ONLY, TEXT_FILE)
	    i = 0
	    while (fscan (fd) != EOF) {
		i = i + 1
	        call gargr (val)
		if (nscan() != 1 || i > nims) {
		    call close (fd)
		    call error (13, "Scale file error")
		}
	    }
	    call seek (fd, BOF)
	} else
	    fd = NULL

	# Allocate memory.
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (omname, SZ_FNAME, TY_CHAR)
	call salloc (iline, IM_MAXDIM, TY_LONG)
	call salloc (imline, IM_MAXDIM, TY_LONG)
	call salloc (oline, IM_MAXDIM, TY_LONG)
	call salloc (omline, IM_MAXDIM, TY_LONG)
	call salloc (hdrs, window, TY_POINTER)
	call salloc (scales, window, TY_REAL)
	if (streq (scale, "mode"))
	    call salloc (sample, NSAMPLE, TY_STRUCT)

	# Initialize
	halfwin = window / 2
	call aclri (Memi[hdrs], window)
	call amovkr (1., Memr[scales], window)
	imdata1 = NULL
	omdata1 = NULL
	oindex = 0
	eindex = 0
	if (verbose)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Loop through data.
	do iindex = 1, nims {
	    # Setup input image and save copy of header.
	    stat = imtrgetim (input, iindex, Memc[inname], SZ_FNAME)
	    if (verbose) {
		call printf ("  Reading %s ...\n")
		    call pargstr (Memc[inname])
	    }
	    in = immap (Memc[inname], READ_ONLY, 0)
	    im = NULL
	    if (nowhite (inmaskkey, Memc[str], SZ_LINE) > 0) {
	        ifnoerr (call imgstr (in, Memc[str], Memc[imname], SZ_FNAME)) {
		    call printf ("  Reading mask %s ...\n")
			call pargstr (Memc[imname])
		    #im = immap (Memc[imname], READ_ONLY, 0)
		    im = yt_mappm (Memc[imname], in, "logical",
			Memc[imname], SZ_FNAME)
		}
	    }
	    
	    j = mod (iindex, window)
	    hdr = Memi[hdrs+j]
	    call mfree (hdr, TY_STRUCT)
	    call malloc (hdr, LEN_IMDES+IM_HDRLEN(in)+1, TY_STRUCT)
	    call amovi (Memi[in], Memi[hdr], LEN_IMDES)
	    call amovi (IM_MAGIC(in), IM_MAGIC(hdr), IM_HDRLEN(in)+1)
	    call strcpy (Memc[inname], IM_NAME(hdr), SZ_IMNAME)
	    Memi[hdrs+j] = hdr

	    # Check image size.
	    if (iindex == 1)
		call amovi (IM_LEN(in,1), len, IM_MAXDIM)
	    else if (!aveqi (IM_LEN(in,1), len, IM_MAXDIM))
	        call error (21, "Image sizes are not the same")
	    if (im != NULL) {
	        if (!aveqi (IM_LEN(im,1), len, IM_MAXDIM))
		    call error (21, "Mask size not the same")
	    } else if (imdata1 == NULL) {
	        call salloc (imdata1, IM_LEN(in,1), TY_SHORT)
		call aclrs (Mems[imdata1], IM_LEN(in,1))
	    }

	    # Initialize.
	    if (iindex == 1) {
		nc = IM_LEN(in,1)
	        nl = 1
		do i = 2, IM_NDIM(in)
		    nl = nl * len[i]

		# Memory is allocated in blocks of number of columns.
		call salloc (rms, nl, TY_POINTER)
		do j = 1, nl {
		    rm = rm_open (window, "median", nc, stype)
		    Memi[rms+j-1] = rm
		}
	    }

	    # Go through input image and create output image.

	    # Set scale factor.
	    if (fd != NULL) {
	        stat = fscan (fd)
		call gargr (iscl)
		if (iscl == 0.)
		    iscl = 1.
	    } else if (scale[1] == '!') {
	        iscl = imgetr (in, scale[2])
		if (iscl == 0.)
		    iscl = 1.
	    } else if (streq (scale, "mode")) {
		if (IM_PIXTYPE(in) == TY_SHORT) {
		    i = xt_samples (in, im, Mems[P2S(sample)], NSAMPLE, NLINES) 
		    call xt_stats (Mems[P2S(sample)], i, FRAC,
			mean, sigma, median, mode)
		} else {
		    i = xt_sampler (in, im, Memr[sample], NSAMPLE, NLINES) 
		    call xt_statr (Memr[sample], i, FRAC,
			mean, sigma, median, mode)
		}
		if (verbose) {
		    call printf("    nsample=%d, mean=%g, median=%g, mode=%g\n")
			call pargi (i)
			call pargr (mean)
			call pargr (median)
			call pargr (mode)
		}
		if (mode != 0.) 
		    iscl = 1. / mode
		else
		    iscl = 1.
	    } else
	        iscl = 1.

	    if (iindex == 1)
		iscl1 = iscl
	    if (normscale)
		iscl = iscl / iscl1
	    if (verbose && strne (scale, "none")) {
		call printf ("    scale = %g\n")
		    call pargr (iscl)
	    }
	    Memr[scales+mod(iindex,window)] = iscl

	    # Do initial accumulation.
	    if (iindex < window) {
		call amovkl (long(1), Meml[iline], IM_MAXDIM)
		call amovkl (long(1), Meml[imline], IM_MAXDIM)
		do j = 1, nl {
		    rm = Memi[rms+j-1]
		    if (im != NULL)
			stat = imgnls (im, imdata, Meml[imline])  
		    else
		        imdata = imdata1
		    if (IM_PIXTYPE(in) == TY_SHORT) {
			stat = imgnls (in, idata, Meml[iline])  
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, 0, iindex,
				iscl*Mems[idata+i-1], Mems[imdata+i-1],
				nused)
			    call rm_pack (rm, i)
			}
		    } else {
			stat = imgnlr (in, idata, Meml[iline])  
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, 0, iindex,
				iscl*Memr[idata+i-1], Mems[imdata+i-1],
				nused)
			    call rm_pack (rm, i)
			}
		    }
		}
		if (im != NULL)
		    call imunmap (im)
	        call imunmap (in)
		next
	    }

	    # Setup output image.
	    oindex = oindex + 1
	    if (exclude)
	        eindex = oindex
	    stat = imtrgetim (output, oindex, Memc[outname], SZ_FNAME)
	    if (verbose) {
		call printf ("  Writing %s ...\n")
		    call pargstr (Memc[outname])
	    }
	    hdr = Memi[hdrs+mod(oindex,window)]
	    call xt_mkimtemp (IM_NAME(hdr), Memc[outname], Memc[imtemp],
	        SZ_FNAME)
	    out = immap (Memc[outname], NEW_COPY, hdr)
	    IM_PIXTYPE(out) = TY_REAL

	    # Setup output mask.
	    stat = imtrgetim (masks, oindex, Memc[str], SZ_LINE)
	    if (stat != EOF) {
	        call xt_maskname (Memc[str], "pl", NEW_IMAGE, Memc[omname],
		    SZ_FNAME)
	        if (verbose) {
		    call printf ("  Writing mask %s ...\n")
		        call pargstr (Memc[omname])
		}
		om = immap (Memc[omname], NEW_COPY, hdr)
		if (nowhite (outmaskkey, Memc[str], SZ_LINE) > 0)
		    call imastr (out, Memc[str], Memc[omname])
	    } else
	        om = NULL
	    if (omdata1 == NULL)
	        call salloc (omdata1, IM_LEN(in,1), TY_SHORT)

	    if (outscale)
	        oscl = 1
	    else
		oscl = 1 / Memr[scales+mod(oindex,window)]

	    # Add input data and create output data.
	    call amovkl (long(1), Meml[iline], IM_MAXDIM)
	    call amovkl (long(1), Meml[imline], IM_MAXDIM)
	    call amovkl (long(1), Meml[oline], IM_MAXDIM)
	    call amovkl (long(1), Meml[omline], IM_MAXDIM)
	    do j = 1, nl {
		stat = impnlr (out, odata, Meml[oline])
		if (im != NULL)
		    stat = imgnls (im, imdata, Meml[imline])  
		else
		    imdata = imdata1
		if (om != NULL)
		    stat = impnls (om, omdata, Meml[omline])  
		else
		    omdata = omdata1

		rm = Memi[rms+j-1]
		if (IM_PIXTYPE(in) == TY_SHORT) {
		    stat = imgnls (in, idata, Meml[iline])  
		    switch (ot) {
		    case OT_FILTER:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    Memr[odata+i-1] = oscl * rm_med (rm, nclip, navg,
			    	blank, eindex, iindex, iscl*Mems[idata+i-1],
				Mems[imdata+i-1], Mems[omdata+i-1])
			    call rm_pack (rm, i)
			}
		    case OT_DIFF:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, eindex,
			        iindex, iscl*Mems[idata+i-1], Mems[imdata+i-1],
				Mems[omdata+i-1])
			    Memr[odata+i-1] =
			        oscl * (rm_gdata (rm, oindex) - val)
			    call rm_pack (rm, i)
			}
		    case OT_RATIO:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, eindex,
			        iindex, iscl*Mems[idata+i-1], Mems[imdata+i-1],
				Mems[omdata+i-1])
			    if (val != 0.)
				Memr[odata+i-1] = rm_gdata (rm, oindex) / val
			    else
				Memr[odata+i-1] = blank
			    call rm_pack (rm, i)
			}
		    }
		} else {
		    stat = imgnlr (in, idata, Meml[iline])  
		    switch (ot) {
		    case OT_FILTER:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    Memr[odata+i-1] = oscl * rm_med (rm, nclip, navg,
			        blank, eindex, iindex, iscl*Memr[idata+i-1],
				Mems[imdata+i-1], Mems[omdata+i-1])
			    call rm_pack (rm, i)
			}
		    case OT_DIFF:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, eindex,
			        iindex, iscl*Memr[idata+i-1], Mems[imdata+i-1],
				Mems[omdata+i-1])
			    Memr[odata+i-1] =
			        oscl * (rm_gdata (rm, oindex) - val)
			    call rm_pack (rm, i)
			}
		    case OT_RATIO:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_med (rm, nclip, navg, blank, eindex,
			        iindex, iscl*Memr[idata+i-1], Mems[imdata+i-1],
				Mems[omdata+i-1])
			    if (val != 0.)
				Memr[odata+i-1] = rm_gdata (rm, oindex) / val
			    else
				Memr[odata+i-1] = blank
			    call rm_pack (rm, i)
			}
		    }
		}
	    }

	    if (om != NULL)
		call imunmap (om)
	    call imunmap (out)
	    call xt_delimtemp (Memc[outname], Memc[imtemp])
	    if (im != NULL)
		call imunmap (im)
	    call imunmap (in)

	    # Do endpoints.
	    while (oindex <= halfwin ||
		(oindex >= nims - (window-1)/2 && oindex < nims)) {

		oindex = oindex + 1
		if (exclude)
		    eindex = oindex
		stat = imtrgetim (output, oindex, Memc[outname], SZ_FNAME)
		if (verbose) {
		    call printf ("  Writing %s ...\n")
			call pargstr (Memc[outname])
		}
		hdr = Memi[hdrs+mod(oindex,window)]
		call xt_mkimtemp (IM_NAME(hdr), Memc[outname], Memc[imtemp],
		    SZ_FNAME)
		out = immap (Memc[outname], NEW_COPY, hdr)
		IM_PIXTYPE(out) = TY_REAL

		stat = imtrgetim (masks, oindex, Memc[str], SZ_LINE)
		if (stat != EOF) {
		    call xt_maskname (Memc[str], "pl", NEW_IMAGE, Memc[omname],
			SZ_FNAME)
		    if (verbose) {
			call printf ("  Writing mask %s ...\n")
			    call pargstr (Memc[omname])
		    }
		    om = immap (Memc[omname], NEW_COPY, hdr)
		    if (nowhite (outmaskkey, Memc[str], SZ_LINE) > 0)
			call imastr (out, Memc[str], Memc[omname])
		} else
		    om = NULL
		if (omdata1 == NULL)
		    call salloc (omdata1, IM_LEN(in,1), TY_SHORT)

		if (outscale)
		    oscl = 1
		else
		    oscl = 1 / Memr[scales+mod(oindex,window)]

		call amovkl (long(1), Meml[oline], IM_MAXDIM)
		call amovkl (long(1), Meml[omline], IM_MAXDIM)
		do j = 1, nl {
		    stat = impnlr (out, odata, Meml[oline])
		    if (om != NULL)
			stat = impnls (om, omdata, Meml[omline])  
		    else
			omdata = omdata1

		    rm = Memi[rms+j-1]
		    switch (ot) {
		    case OT_FILTER:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    Memr[odata+i-1] = oscl * rm_gmed (rm, nclip, navg,
			        blank, eindex, Mems[omdata+i-1])
			    call rm_pack (rm, i)
			}
		    case OT_DIFF:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_gmed (rm, nclip, navg, blank, eindex,
			        Mems[omdata+i-1])
			    Memr[odata+i-1] = oscl *
			        (rm_gdata (rm, oindex) - val)
			    call rm_pack (rm, i)
			}
		    case OT_RATIO:
			do i = 1, nc {
			    call rm_unpack (rm, i)
			    val = rm_gmed (rm, nclip, navg, blank, eindex,
			        Mems[omdata+i-1])
			    if (val != 0.)
				Memr[odata+i-1] = rm_gdata (rm, oindex) / val
			    else
				Memr[odata+i-1] = blank
			    call rm_pack (rm, i)
			}
		    }
		}

		if (om != NULL)
		    call imunmap (om)
		call imunmap (out)
		call xt_delimtemp (Memc[outname], Memc[imtemp])
	    }
	}

	# Finish up.
	if (fd != NULL)
	    call close (fd)
	do j = 1, nl {
	    rm = Memi[rms+j-1]
	    call rm_close (rm)
	}
	do i = 1, window {
	    hdr = Memi[hdrs+mod(i,window)]
	    call mfree (hdr, TY_STRUCT)
	}
	call sfree (sp)
end
