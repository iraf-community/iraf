# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	"icombine.h"


# T_IMCOMBINE - This task combines a list of images into an output image
# and an optional sigma image.  There are many combining options from
# which to choose.

procedure t_imcombine ()

pointer	sp, input, output, sigma, plfile, logfile
int	ilist, olist, slist, plist

bool	clgetb()
real	clgetr()
int	clgwrd(), clgeti(), imtopenp(), imtgetim()

include	"icombine.com"

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (plfile, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Get task parameters.  Some additional parameters are obtained later.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	plist = imtopenp ("plfile")
	slist = imtopenp ("sigma")
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)

	project = clgetb ("project")
	combine = clgwrd ("combine", Memc[input], SZ_FNAME, COMBINE)
	reject = clgwrd ("reject", Memc[input], SZ_FNAME, REJECT)
	blank = clgetr ("blank")
	call clgstr ("gain", Memc[gain], SZ_FNAME)
	call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
	call clgstr ("snoise", Memc[snoise], SZ_FNAME)
	lthresh = clgetr ("lthreshold")
	hthresh = clgetr ("hthreshold")
	lsigma = clgetr ("lsigma")
	hsigma = clgetr ("hsigma")
	pclip = clgetr ("pclip")
	flow = clgetr ("nlow")
	fhigh = clgetr ("nhigh")
	nkeep = clgeti ("nkeep")
	grow = clgeti ("grow")
	mclip = clgetb ("mclip")
	sigscale = clgetr ("sigscale")

	# Check parameters, map INDEFs, and set threshold flag
	if (pclip == 0. && reject == PCLIP)
	    call error (1, "Pclip parameter may not be zero")
	if (IS_INDEFR (blank))
	    blank = 0.
	if (IS_INDEFR (lsigma))
	    lsigma = MAX_REAL
	if (IS_INDEFR (hsigma))
	    hsigma = MAX_REAL
	if (IS_INDEFR (pclip))
	    pclip = -0.5
	if (IS_INDEFR (flow))
	    flow = 0
	if (IS_INDEFR (fhigh))
	    fhigh = 0
	if (IS_INDEFI (grow))
	    grow = 0
	if (IS_INDEF (sigscale))
	    sigscale = 0.

	if (IS_INDEF(lthresh) && IS_INDEF(hthresh))
	    dothresh = false
	else {
	    dothresh = true
	    if (IS_INDEF(lthresh))
		lthresh = -MAX_REAL
	    if (IS_INDEF(hthresh))
		hthresh = MAX_REAL
	}

	# Loop through image lists.  Note that if not projecting then
	# the input list will be exhausted by IMCOMBINE.

	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF) {
		if (project) {
		    call eprintf ("IMCOMBINE: No output image for %s\n")
			call pargstr (Memc[input])
		    next
		} else {
		    call eprintf ("IMCOMBINE: No output image\n")
			call pargstr (Memc[input])
		    break
		}
	    }
	    if (imtgetim (plist, Memc[plfile], SZ_FNAME) == EOF)
		Memc[plfile] = EOS
	    if (imtgetim (slist, Memc[sigma], SZ_FNAME) == EOF)
		Memc[sigma] = EOS

	    iferr (call icombine (ilist, Memc[input], Memc[output],
		Memc[plfile], Memc[sigma], Memc[logfile], NO))
		call erract (EA_WARN)
	}

	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (plist)
	call imtclose (slist)
	call sfree (sp)
end


# IMCOMBINE -- Combine input list or image.
# This procedure maps the images, sets the output dimensions and datatype,
# opens the logfile, and sets IMIO parameters.  It attempts to adjust
# buffer sizes and memory requirements for maximum efficiency.

procedure icombine (list, input, output, plfile, sigma, logfile, stack)

int	list			# List of input images
char	input[ARB]		# Input image
char	output[ARB]		# Output image
char	plfile[ARB]		# Output pixel list file
char	sigma[ARB]		# Sigma image (optional)
char	logfile[ARB]		# Logfile (optional)
int	stack			# Stack input images?

int	i, j, nimages, intype, bufsize, maxsize, memory, oldsize, stack1, err
pointer	sp, in, out[3], icm, offsets, key, tmp

char	clgetc()
int	imtlen(), imtgetim(), getdatatype()
int	begmem(), errcode(), open(), ty_max(), sizeof()
pointer	immap(), ic_plfile()
errchk	ic_imstack, immap, ic_plfile, ic_setout

include	"icombine.com"

define	retry_	98
define	done_	99

begin
	# Map the input images.
	bufsize = 0
	stack1 = stack

retry_
	iferr {
	    call smark (sp)

	    out[1] = NULL
	    out[2] = NULL
	    out[3] = NULL
	    icm = NULL
	    logfd = NULL

	    # Stack the input images.
	    if (stack1 == YES) {
		call mktemp ("tmp", input, SZ_FNAME)
		call imtrew (list)
		call ic_imstack (list, input)
		project = true
	    }

	    # Open the input image(s).
	    nimages = 0
	    if (project) {
		tmp = immap (input, READ_ONLY, 0); out[1] = tmp
		if (IM_NDIM(out[1]) == 1)
		    call error (1, "Can't project one dimensional images")
		nimages = IM_LEN(out[1],IM_NDIM(out[1]))
		call calloc (in, nimages, TY_POINTER)
		call amovki (out[1], Memi[in], nimages)
	    } else {
		call calloc (in, imtlen(list), TY_POINTER)
		call imtrew (list)
		while (imtgetim (list, input, SZ_FNAME)!=EOF) {
		    tmp = immap (input, READ_ONLY, 0)
		    Memi[in+nimages] = tmp
		    nimages = nimages + 1
		}
	    }

	    # Check if there are no images.
	    if (nimages == 0) {
		call eprintf ("No input images to combine\n")
		goto done_
	    }

	    # Convert the pclip parameter to a number of pixels rather than
	    # a fraction.  This number stays constant even if pixels are
	    # rejected.  The number of low and high pixel rejected, however,
	    # are converted to a fraction of the valid pixels.

	    if (reject == PCLIP) {
		i = (nimages - 1) / 2.
		if (abs (pclip) < 1.)
		    pclip = pclip * i
		if (pclip < 0.)
		    pclip = min (-1, max (-i, int (pclip)))
		else
		    pclip = max (1, min (i, int (pclip)))
	    }

	    if (reject == MINMAX) {
		if (flow >= 1)
		    flow = flow / nimages
		if (fhigh >= 1)
		    fhigh = fhigh / nimages
		i = flow * nimages
		j = fhigh * nimages
		if (i + j == 0)
		    reject = NONE
		else if (i + j >= nimages) {
		    call eprintf ("Bad minmax rejection parameters\n")
		    goto done_
		}
	    }

	    # Map the output image and set dimensions and offsets.
	    tmp = immap (output, NEW_COPY, Memi[in]); out[1] = tmp
	    if (stack1 == YES) {
		call salloc (key, SZ_FNAME, TY_CHAR)
		do i = 1, nimages {
		    call sprintf (Memc[key], SZ_FNAME, "stck%04d")
			call pargi (i)
		    call imdelf (out[1], Memc[key])
		}
	    }
	    call salloc (offsets, nimages*IM_NDIM(out[1]), TY_INT)
	    call ic_setout (Memi[in], out, Memi[offsets], nimages)

	    # Determine the highest precedence datatype and set output datatype.
	    intype = IM_PIXTYPE(Memi[in])
	    do i = 2, nimages
		intype = ty_max (intype, IM_PIXTYPE(Memi[in+i-1]))
	    IM_PIXTYPE(out[1]) = getdatatype (clgetc ("outtype"))
	    if (IM_PIXTYPE(out[1]) == ERR)
		IM_PIXTYPE(out[1]) = intype

	    # Open pixel list file if given.
	    if (plfile[1] != EOS) {
		tmp = ic_plfile (plfile, NEW_COPY, out[1]); out[2] = tmp
	    } else
		out[2] = NULL

	    # Open the sigma image if given.
	    if (sigma[1] != EOS) {
		tmp = immap (sigma, NEW_COPY, out[1]); out[3] = tmp
		IM_PIXTYPE(out[3]) = ty_max (TY_REAL, IM_PIXTYPE(out[1]))
		call sprintf (IM_TITLE(out[3]), SZ_IMTITLE,
		    "Combine sigma images for %s")
		    call pargstr (output)
	    } else
		out[3] = NULL

	    # Open masks.
	    call ic_mopen (Memi[in], out, nimages)
	    icm = nimages

	    # Open the log file.
	    logfd = NULL
	    if (logfile[1] != EOS) {
		iferr (logfd = open (logfile, APPEND, TEXT_FILE)) {
		    logfd = NULL
		    call erract (EA_WARN)
		}
	    }

	    if (bufsize == 0) {
		# Set initial IMIO buffer size based on the number of images
		# and maximum amount of working memory available.  The buffer
		# size may be adjusted later if the task runs out of memory.
		# The FUDGE factor is used to allow for the size of the
		# program, memory allocator inefficiencies, and any other
		# memory requirements besides IMIO.

		bufsize = 1
		do i = 1, IM_NDIM(out[1])
		    bufsize = bufsize * IM_LEN(out[1],i)
		bufsize = bufsize * sizeof (intype)
		bufsize = min (bufsize, DEFBUFSIZE)
		memory = begmem ((nimages + 1) * bufsize, oldsize, maxsize)
		memory = min (memory, int (FUDGE * maxsize))
		bufsize = memory / (nimages + 1)
	    }

	    # Combine the images.  If an out of memory error occurs close all
	    # images and files, divide the IMIO buffer size in half and try
	    # again.  The integer types are not support because scaling is
	    # done on  the input data vectors.

	    switch (intype) {
	    case TY_SHORT:
		call icombines (Memi[in], out, Memi[offsets], nimages, bufsize)
	    case TY_USHORT, TY_INT, TY_LONG:
		call icombinei (Memi[in], out, Memi[offsets], nimages, bufsize)
	    case TY_DOUBLE:
		call icombined (Memi[in], out, Memi[offsets], nimages, bufsize)
	    case TY_COMPLEX:
		call error (1, "Complex images not allowed")
	    default:
		call icombiner (Memi[in], out, Memi[offsets], nimages, bufsize)
	    }
	} then {
	    err = errcode ()
	    if (icm != NULL)
		call ic_mclose (nimages)
	    if (!project) {
		do j = 2, nimages
		    if (Memi[in+j-1] != NULL)
			call imunmap (Memi[in+j-1])
	    }
	   if (out[2] != NULL) {
		call imunmap (out[2])
		call imdelete (plfile)
	    }
	    if (out[3] != NULL) {
		call imunmap (out[3])
		call imdelete (sigma)
	    }
	   if (out[1] != NULL) {
	       call imunmap (out[1])
	       call imdelete (output)
	    }
	    if (Memi[in] != NULL)
		call imunmap (Memi[in])
	    if (logfd != NULL)
		call close (logfd)

	    switch (err) {
	    case SYS_MFULL:
		bufsize = bufsize / 2
		call sfree (sp)
		goto retry_
	    case SYS_FTOOMANYFILES, SYS_IKIOPEN, SYS_IKIOPIX:
		if (!project) {
		    stack1 = YES
		    goto retry_
		}
		if (stack1 == YES)
		    call imdelete (input)
		call fixmem (oldsize)
		call sfree (sp)
		call erract (EA_ERROR)
	    default:
		if (stack1 == YES)
		    call imdelete (input)
		call fixmem (oldsize)
		call sfree (sp)
		call erract (EA_ERROR)
	    }
	}

	# Unmap all the images, close the log file, and restore memory.
	# The input images must be unmapped first to insure that there
	# is a FD for the output images since the headers are opened to
	# update them.  However, the order of the NEW_COPY pointers must
	# be preserved.

	if (!project) {
	    do i = 2, nimages
		if (Memi[in+i-1] != NULL)
		    call imunmap (Memi[in+i-1])
	}
	if (out[2] != NULL)
	    call imunmap (out[2])
	if (out[3] != NULL)
	    call imunmap (out[3])
	if (out[1] != NULL)
	    call imunmap (out[1])
	if (Memi[in] != NULL)
	    call imunmap (Memi[in])
	if (stack1 == YES)
	    call imdelete (input)
	if (logfd != NULL)
	    call close (logfd)
	if (icm != NULL)
	    call ic_mclose (nimages)
	call fixmem (oldsize)
	call sfree (sp)
	return

done_
	do i = 1, nimages {
	    call imunmap (Memi[in+i-1])
	    if (project)
		break
	}
	if (stack1 == YES)
	    call imdelete (input)

	call sfree (sp)
end


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, type, order[8]
data	order/TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=7) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=7) && (type2!=order[j]); j=j+1)
	    ;
	type = order[max(i,j)]

	# Special case of mixing short and unsigned short.
	if (type == TY_USHORT && type1 != type2)
	    type = TY_INT

	return (type)
end


# IC_PLFILE -- Map pixel list file
# This routine strips any image extensions and then adds .pl.

pointer procedure ic_plfile (plfile, mode, refim)

char	plfile[ARB]		# Pixel list file name
int	mode			# Image mode
pointer	refim			# Reference image
pointer	pl			# IMIO pointer (returned)

int	i, strlen()
bool	streq
pointer	sp, str, immap()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call imgimage (plfile, Memc[str], SZ_FNAME)

	# Strip any existing extensions
	i = strlen(Memc[str])
	switch (Memc[str+i-1]) {
	case 'h':
	    if (i > 3 && Memc[str+i-4] == '.')
		Memc[str+i-4] = EOS
	case 'l':
	    if (i > 2 && streq (Memc[str+i-3], ".pl"))
		Memc[str+i-3] = EOS
	}

	call strcat (".pl", Memc[str], SZ_FNAME)
	pl = immap (Memc[str], NEW_COPY, refim)
	call sfree (sp)
	return (pl)
end
