# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	<syserr.h>

define	DEFBUFSIZE		65536		# default IMIO buffer size
define	FUDGE			0.8		# fudge factor

# COMBINE OPTIONS:
define	COMBINE	"|sum|average|median|minreject|maxreject|minmaxreject|\
	|threshold|sigclip|avsigclip|"
define	SUM		1	# Sum of the images
define	AVERAGE		2	# Average of the images
define	MEDIAN		3	# Median of the images
define	MINREJECT	4	# Reject minimum
define	MAXREJECT	5	# Reject maximin
define	MINMAXREJECT	6	# Reject minimum and maximum
# 	newline		7
define	THRESHOLD	8	# Absolute threshold clip
define	SIGCLIP		9	# Sigma clip using sigma at each point
define	AVSIGCLIP	10	# Sigma clip using average sigma

# T_IMCOMBINE - This task combines a list of images into an output image
# and an optional sigma image.  There are many combining algorithms from
# which to choose.  These are described elsewhere.

procedure t_imcombine ()

int	list			# List of input images
pointer	output			# Output image
pointer	sigma			# Optional sigma image
pointer	logfile			# Log file
int	option			# Combine option
int	outtype			# Output datatype

int	i, j, nimages, intype, bufsize, maxsize, memory, oldsize, log
pointer	sp, in, out, sig, im, im1, input

char	clgetc()
int	imtopenp(), imtlen(), imtgetim()
int	clgwrd(), begmem(), errcode(), open(), ty_max(), sizeof()
pointer	immap()
errchk	immap
define	retry_	99

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (option, SZ_LINE, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Get the parameters.  Some parameters are obtained later.
	list = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("sigma", Memc[sigma], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	option = clgwrd ("option", Memc[input], SZ_FNAME, COMBINE)
	outtype = clgetc ("outtype")

	# Check if there are no images.
	nimages = imtlen (list)
	if (nimages == 0) {
	    call imtclose (list)
	    call sfree (sp)
	    call error (0, "No input images to combine")
	}
	call salloc (in, nimages, TY_POINTER)

	# Map the input images.
	bufsize = 0
retry_
	nimages = 0
	while (imtgetim (list, Memc[input], SZ_FNAME)!=EOF) {
	    Memi[in+nimages] = immap (Memc[input], READ_ONLY, 0)
	    nimages = nimages + 1
	}

	# Check all images are of the same dimension and size and
	# determine the highest precedence datatype.

	im = Memi[in]
	intype = IM_PIXTYPE(im)
	do i = 2, nimages {
	    im1 = Memi[in+i-1]
	    if (IM_NDIM(im1) != IM_NDIM(im))
		call error (0, "Image dimensions are not the same")
	    do j = 1, IM_NDIM(im)
		if (IM_LEN(im1,j) != IM_LEN(im,j))
		    call error (0, "Image sizes are not the same")
	    intype = ty_max (intype, IM_PIXTYPE(im1))
	}

	# Open the output image and set its pixel datatype.
	out = immap (Memc[output], NEW_COPY, Memi[in])
	switch (outtype) {
	case 's':
	    IM_PIXTYPE(out) = TY_SHORT
	case 'i':
	    IM_PIXTYPE(out) = TY_INT
	case 'l':
	    IM_PIXTYPE(out) = TY_LONG
	case 'r':
	    IM_PIXTYPE(out) = TY_REAL
	case 'd':
	    IM_PIXTYPE(out) = TY_DOUBLE
	case 'c':
	    IM_PIXTYPE(out) = TY_COMPLEX
	default:
	    IM_PIXTYPE(out) = intype
	}

	# Open the sigma image if given.
	switch (option) {
	case SUM:
	    Memc[sigma] = EOS
	case AVERAGE, MEDIAN, SIGCLIP, AVSIGCLIP, THRESHOLD:
	    if (nimages < 2)
		Memc[sigma] = EOS
	case MINREJECT, MAXREJECT:
	    if (nimages < 3)
		Memc[sigma] = EOS
	case MINMAXREJECT:
	    if (nimages < 4)
		Memc[sigma] = EOS
	}
	if (Memc[sigma] != EOS) {
	    sig = immap (Memc[sigma], NEW_COPY, out)
	    IM_PIXTYPE(sig) = ty_max (TY_REAL, IM_PIXTYPE(out))
	    call sprintf (IM_TITLE(sig), SZ_IMTITLE,
		"Combine sigma images for %s")
		call pargstr (output)
	} else
	    sig = NULL

	# Open the log file.
	log = NULL
	if (Memc[logfile] != EOS) {
	    iferr (log = open (Memc[logfile], APPEND, TEXT_FILE)) {
	        log = NULL
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
	    do i = 1, IM_NDIM(out)
		bufsize = bufsize * IM_LEN(out,i)
	    bufsize = bufsize * sizeof (intype)
	    bufsize = min (bufsize, DEFBUFSIZE)
	    memory = begmem ((nimages + 1) * bufsize, oldsize, maxsize)
	    memory = min (memory, int (FUDGE * maxsize))
	    bufsize = memory / (nimages + 1)
	}

	# Combine the images.  If an out of memory error occurs close all
	# images and files, divide the IMIO buffer size in half and try again.

	iferr {
	    switch (intype) {
	    case TY_SHORT:
	        call imcombines (log, Memi[in], nimages, out, sig, bufsize,
		    option)
	    case TY_INT:
	        call imcombinei (log, Memi[in], nimages, out, sig, bufsize,
		    option)
	    case TY_LONG:
	        call imcombinel (log, Memi[in], nimages, out, sig, bufsize,
		    option)
	    case TY_REAL:
	        call imcombiner (log, Memi[in], nimages, out, sig, bufsize,
		    option)
	    case TY_DOUBLE:
	        call imcombined (log, Memi[in], nimages, out, sig, bufsize,
		    option)
	    case TY_COMPLEX:
		if ((option == SIGCLIP) || (option == AVSIGCLIP))
		    call error (0, "Option not supported for complex images")
		else
	            call imcombinex (log, Memi[in], nimages, out, sig, bufsize,
		        option)
	    }
	} then {
	    switch (errcode()) {
	    case SYS_MFULL:
		do j = 1, nimages
		    call imunmap (Memi[in+j-1])
		call imunmap (out)
		call imdelete (Memc[output])
		if (sig != NULL) {
		    call imunmap (sig)
		    call imdelete (Memc[sigma])
		}
	     	if (log != NULL)
		    call close (log)
		call imtrew (list)
		bufsize = bufsize / 2
		goto retry_
	    default:
		call erract (EA_ERROR)
	    }
	}

	# Unmap all the images, close the log file, and restore memory.
	call imunmap (out)
	if (sig != NULL)
	    call imunmap (sig)
	if (log != NULL)
	    call close (log)
	do i = 1, nimages
	    call imunmap (Memi[in+i-1])
	call sfree (sp)

	call fixmem (oldsize)
end


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, order[7]
data	order/TY_SHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=6) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=6) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end
