include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include	"ccdred.h"

.help combine
.nf ----------------------------------------------------------------------------
This task combines a list of images into one or more output images.
The images may be filtered by their image types and subset strings.
There are many combining algorithms from which to choose.  These are
described elsewhere.  The input images may be either short or real
datatype.  The output image datatype may be specified or defaulted to
the datatype of the input images.  If the input images have mixed
datatype the default datatype is real.  The input images may be deleted
after they are combined and the output image(s) may be overwritten if desired.

This task is essentially the same as the IMAGES.IMCOMBINE task even to
the names of the procedures (IMC prefix).  The differences are as follows.

1. The main procedures (this file) are specialized for the CCDRED
package.  It selects the input images by CCD type and filter and using the
package image open and close routines.

2. IMCOMBINE.GX closes the log file.  The log file is opened by the
appropriate logging procedures IMCSUMLOG.X and IMCSCALES.X.

3. IMCSUMLOG.X and IMCSCALES.X are modified for logging based on the
package log parameters and also have additional knowledge of the
image header parameters.  They use the image header translation
mechanism.
.endhelp -----------------------------------------------------------------------

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


# T_COMBINE -- Combine CCD images.

procedure t_combine ()

pointer	images			# Images filter by ccdtype and sorted by subset
pointer	subsets			# Subsets
pointer	nimages			# Number of images in each subset
int	nsubsets		# Number of subsets
pointer	outroot			# Output root image name
pointer	sigroot			# Output root sigma image name
pointer	logfile			# Log filename
int	combine			# Combine option
bool	delete			# Delete input images?
bool	clobber			# Clobber output images?

int	i
pointer	sp, output, sigma, cmbstr

bool	clgetb()
int	clgwrd()

begin
	call smark (sp)
	call salloc (outroot, SZ_FNAME, TY_CHAR)
	call salloc (sigroot, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (cmbstr, SZ_LINE, TY_CHAR)

	# Open the header translation which is needed to determine the
	# subsets and ccdtypes.  Get the input images.
	# There must be a least one image in order to continue.

	call clgstr ("instrument", Memc[output], SZ_FNAME)
	call hdmopen (Memc[output])
	call cmb_images (images, subsets, nimages, nsubsets)
	if (nsubsets == 0)
	    call error (0, "No images to combine")

	# Get the output root and sigma root image names, and the combine,
	# delete, and clobber options.

	call clgstr ("output", Memc[outroot], SZ_FNAME)
	call clgstr ("sigma", Memc[sigroot], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	call xt_stripwhite (Memc[outroot])
	call xt_stripwhite (Memc[sigroot])
	call xt_stripwhite (Memc[logfile])
	combine = clgwrd ("combine", Memc[cmbstr], SZ_FNAME, COMBINE)
	delete = clgetb ("delete")
	clobber = clgetb ("clobber")

	# Combine each input subset.
	do i = 1, nsubsets {
	    # Set the output image name and sigma image name with
	    # subset extension.

	    call strcpy (Memc[outroot], Memc[output], SZ_FNAME)
	    call sprintf (Memc[output], SZ_FNAME, "%s%s")
		call pargstr (Memc[outroot])
		call pargstr (Memc[Memi[subsets+i-1]])

	    call strcpy (Memc[sigroot], Memc[sigma], SZ_FNAME)
	    if (Memc[sigma] != EOS) {
		call sprintf (Memc[sigma], SZ_FNAME, "%s%s")
		    call pargstr (Memc[sigroot])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Combine all images from the (subset) list.
	    iferr (call cmb_combine (Memc[Memi[images+i-1]], Memi[nimages+i-1],
		combine, Memc[output], Memc[sigma], Memc[logfile], delete,
		clobber)) {
		call erract (EA_WARN)
	    }
	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}

	# Finish up.
	call mfree (images, TY_POINTER)
	call mfree (subsets, TY_POINTER)
	call mfree (nimages, TY_INT)
	call hdmclose ()
	call sfree (sp)
end


# CMB_IMAGES -- Get images from a list of images.
# The images are filtered by ccdtype and sorted by subset.  The allocated lists
# must be freed by the caller.

procedure cmb_images (images, subsets, nimages, nsubsets)

pointer	images		# Pointer to lists of subsets (allocated)
pointer	subsets		# Subset numbers (allocated)
pointer	nimages		# Number of images in subset (allocated)
int	nsubsets	# Number of subsets

int	list		# List of input images
bool	dosubsets	# Divide input into subsets?

int	i, nimage, ccdtype
pointer	sp, image, subset, ptr, str, im
int	imtopenp(), imtlen(), imtgetim()
bool	clgetb(), streq()

begin
	# Get the input image list and check that there is at least one image.
	nsubsets = 0
	list = imtopenp ("images")
	nimage = imtlen (list)
	if (nimage == 0) {
	    call imtclose (list)
	    return
	}

	# Determine whether to divide images into subsets.
	dosubsets = clgetb ("subsets")

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (subset, SZ_FNAME, TY_CHAR)
	call calloc (images, nimage, TY_POINTER)
	call calloc (subsets, nimage, TY_POINTER)
	call calloc (nimages, nimage, TY_INT)

	# Go through the input list and eliminate images not satisfying the
	# CCD image type.  Separate into subsets if desired.  Create image
	# and subset lists.

	while (imtgetim (list, Memc[image], SZ_FNAME)!=EOF) {
	    call set_input (Memc[image], im, ccdtype)
	    if (im == NULL)
		next

	    if (dosubsets) {
	        call ccdsubset (im, Memc[subset], SZ_FNAME)
	        for (i=1; i <= nsubsets; i=i+1)
		    if (streq (Memc[subset], Memc[Memi[subsets+i-1]]))
		        break
	    } else {
		Memc[subset] = EOS
		i = 1
	    }
	    
	    if (i > nsubsets) {
	        nsubsets = i
		call malloc (ptr, SZ_FNAME, TY_CHAR)
		call strcpy (Memc[subset], Memc[ptr], SZ_FNAME)
		Memi[subsets+i-1] = ptr
		call malloc (ptr, SZ_FNAME, TY_CHAR)
	        call strcpy (Memc[image], Memc[ptr], SZ_FNAME-1)
	        Memi[images+i-1] = ptr
	        Memi[nimages+i-1] = 1
	    } else {
		ptr = Memi[images+i-1]
		nimage = Memi[nimages+i-1] + 1
		call realloc (ptr, nimage * SZ_FNAME, TY_CHAR)
	        str = ptr + (nimage - 1) * SZ_FNAME
	        call strcpy (Memc[image], Memc[str], SZ_FNAME-1)
	        Memi[images+i-1] = ptr
	        Memi[nimages+i-1] = nimage
	    }
		    
	    call imunmap (im)
	}
	call realloc (images, nsubsets, TY_POINTER)
	call realloc (subsets, nsubsets, TY_POINTER)
	call realloc (nimages, nsubsets, TY_INT)
	call imtclose (list)
	call sfree (sp)
end


# CMB_COMBINE -- Combine the CCD images in a list.
# The type of combining is specified.  The images combined may be optionally
# deleted.  The output image pixel type is set when it is mapped.  An
# optional sigma output image may be created.

procedure cmb_combine (images, nimages, combine, output, sigma, logfile,
	delete, clobber)

char	images[SZ_FNAME-1, nimages]	# Input images
int	nimages				# Number of images in list
int	combine				# Combine option
char	output[ARB]			# Output image
char	sigma[ARB]			# Output sigma image
char	logfile[ARB]			# Log filename
bool	delete				# Delete input images?
bool	clobber				# Clobber output images?

int	i, j, bufsize, maxsize, memory, oldsize, log, intype
pointer	sp, in, temp1, temp2, im, im1, out, sig

bool	strne()
int	begmem(), errcode(), imaccess(), open(), ty_max(), sizeof()
pointer	immap()
errchk	immap, ccddelete
define	retry_	10

begin
	call smark (sp)
	call salloc (in, nimages, TY_POINTER)
	call salloc (temp1, SZ_FNAME, TY_CHAR)
	call salloc (temp2, SZ_FNAME, TY_CHAR)

	# Check output images for existence.  If clobbering the output images
	# use temporary images for output and only replace the existing
	# image when the operation is successfully completed.

	call strcpy (output, Memc[temp1], SZ_FNAME)
	if (imaccess (Memc[temp1], READ_ONLY, 0) == YES) {
	    if (clobber)
		call mktemp ("temp", Memc[temp1], SZ_FNAME)
	    else
		call error (0, "Output image exists")
	}

	call strcpy (sigma, Memc[temp2], SZ_FNAME)
	if (Memc[temp2] != EOS) {
	    if (imaccess (Memc[temp2], READ_ONLY, 0) == YES) {
	        if (clobber)
		    call mktemp ("temp", Memc[temp2], SZ_FNAME)
	        else
		    call error (0, "Output sigma image exists")
	    }
	}

	# Map the input images.
	bufsize = 0
retry_
	do i = 1, nimages
	    Memi[in+i-1] = immap (images[1,i], READ_ONLY, 0)

	# Check all images are of the same dimension and size.
	# If one of the images is of type real then select it as the
	# default for the output image type.

	im = Memi[in]
	intype = IM_PIXTYPE(im)
	do i = 1, nimages {
	    im1 = Memi[in+i-1]
	    if (IM_NDIM(im1) != IM_NDIM(im))
		call error (0, "Image dimensions are not the same")
	    do j = 1, IM_NDIM(im)
		if (IM_LEN(im1,j) != IM_LEN(im,j))
		    call error (0, "Image sizes are not the same")
	    intype = ty_max (intype, IM_PIXTYPE(im1))
	    if (IM_PIXTYPE(im) != intype)
		im = im1
	}

	# Open the output image and set its pixel datatype.
	call set_output (im, out, Memc[temp1])

	# Open the sigma image if given.
	switch (combine) {
	case SUM:
	    Memc[temp2] = EOS
	case AVERAGE, MEDIAN, SIGCLIP, AVSIGCLIP, THRESHOLD:
	    if (nimages < 2)
		Memc[temp2] = EOS
	case MINREJECT, MAXREJECT:
	    if (nimages < 3)
		Memc[temp2] = EOS
	case MINMAXREJECT:
	    if (nimages < 4)
		Memc[temp2] = EOS
	}
	if (Memc[temp2] != EOS) {
	    sig = immap (Memc[temp2], NEW_COPY, out)
	    IM_PIXTYPE(sig) = TY_REAL
	    call sprintf (IM_TITLE(sig), SZ_IMTITLE,
		"Combine sigma images for %s")
		call pargstr (output)
	} else
	    sig = NULL

	# Check log file and reserve a file descriptor.
	log = NULL
	if (logfile[1] != EOS) {
	    iferr (log = open (logfile, APPEND, TEXT_FILE)) {
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
		call im_combines (log, Memi[in], nimages, out, sig, bufsize,
		    combine)
	    default:
		call im_combiner (log, Memi[in], nimages, out, sig, bufsize,
		    combine)
	    }
	} then {
	    switch (errcode()) {
	    case SYS_MFULL:
	        do j = 1, nimages
		    call imunmap (Memi[in+j-1])
		call imunmap (out)
		call imdelete (Memc[temp1])
		if (sig != NULL) {
		    call imunmap (sig)
		    call imdelete (Memc[temp2])
		}
	    	if (log != NULL)
		    call close (log)
	        bufsize = bufsize / 2
	        goto retry_
	    default:
	        call erract (EA_ERROR)
	    }
	}

	# Unmap all the images and delete them if desired.
	do i = 1, nimages {
	    call imunmap (Memi[in+i-1])
	    if (delete)
		call ccddelete (images[1,i])
	}
	iferr (call imdelf (out, "ccdmean"))
	    ;
	call imunmap (out)
	if (strne (Memc[temp1], output)) {
	    call ccddelete (output)
	    call imrename (Memc[temp1], output)
	}
	if (sig != NULL) {
	    call imunmap (sig)
	    if (strne (Memc[temp2], sigma)) {
	        call ccddelete (sigma)
	        call imrename (Memc[temp2], sigma)
	    }
	}

	call fixmem (oldsize)
	call sfree (sp)
end


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, order[3]
data	order/TY_SHORT,TY_REAL,TY_REAL/

begin
	for (i=1; (i<=2) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=2) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end
