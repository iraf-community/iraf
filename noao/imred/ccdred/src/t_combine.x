include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	"ccdred.h"
include	"icombine.h"


# T_COMBINE -- Combine CCD images.
# This task is a copy of IMAGES.IMCOMBINE except that it recognizes the
# CCD types and can group images by SUBSET.  It also uses header keyword
# translation for the exposure times.

procedure t_combine ()

pointer	images			# Images filter by ccdtype and sorted by subset
pointer	subsets			# Subsets
pointer	nimages			# Number of images in each subset
int	nsubsets		# Number of subsets
pointer	outroot			# Output root image name
pointer	plroot			# Output pixel list root name
pointer	sigroot			# Output root sigma image name
pointer	logfile			# Log filename
bool	delete			# Delete input images?
bool	clobber			# Clobber output images?

int	i
pointer	sp, output, plfile, sigma

bool	clgetb()
int	clgeti(), clgwrd()
real	clgetr()

include	"icombine.com"

begin
	call smark (sp)
	call salloc (outroot, SZ_FNAME, TY_CHAR)
	call salloc (plroot, SZ_FNAME, TY_CHAR)
	call salloc (sigroot, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (plfile, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Open the header translation which is needed to determine the
	# subsets and ccdtypes.  Get the input images.
	# There must be a least one image in order to continue.

	call clgstr ("instrument", Memc[output], SZ_FNAME)
	call hdmopen (Memc[output])
	call cmb_images (images, subsets, nimages, nsubsets)
	if (nsubsets == 0)
	    call error (0, "No images to combine")

	# Get task parameters.  Some additional parameters are obtained later.
	call clgstr ("output", Memc[outroot], SZ_FNAME)
	call clgstr ("plfile", Memc[plroot], SZ_FNAME)
	call clgstr ("sigma", Memc[sigroot], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	call xt_stripwhite (Memc[outroot])
	call xt_stripwhite (Memc[sigroot])
	call xt_stripwhite (Memc[logfile])

	project = clgetb ("project")
	combine = clgwrd ("combine", Memc[output], SZ_FNAME, COMBINE)
        reject = clgwrd ("reject", Memc[output], SZ_FNAME, REJECT)
        blank = clgetr ("blank")
        call clgstr ("gain", Memc[gain], SZ_FNAME)
        call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
        call clgstr ("snoise", Memc[snoise], SZ_FNAME)
        lthresh = clgetr ("lthreshold")
        hthresh = clgetr ("hthreshold")
        lsigma = clgetr ("lsigma")
        hsigma = clgetr ("hsigma")
        grow = clgeti ("grow")
        mclip = clgetb ("mclip")
        sigscale = clgetr ("sigscale")
	delete = clgetb ("delete")
	clobber = clgetb ("clobber")

        # Check parameters, map INDEFs, and set threshold flag
        if (IS_INDEFR (blank))
            blank = 0.
        if (IS_INDEFR (lsigma))
            lsigma = MAX_REAL
        if (IS_INDEFR (hsigma))
            hsigma = MAX_REAL
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

	# Combine each input subset.
	do i = 1, nsubsets {
	    # Set the output, pl, and sigma image names with subset extension.

	    call strcpy (Memc[outroot], Memc[output], SZ_FNAME)
	    call sprintf (Memc[output], SZ_FNAME, "%s%s")
		call pargstr (Memc[outroot])
		call pargstr (Memc[Memi[subsets+i-1]])

	    call strcpy (Memc[plroot], Memc[plfile], SZ_FNAME)
	    if (Memc[plfile] != EOS) {
		call sprintf (Memc[plfile], SZ_FNAME, "%s%s")
		    call pargstr (Memc[plroot])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[sigroot], Memc[sigma], SZ_FNAME)
	    if (Memc[sigma] != EOS) {
		call sprintf (Memc[sigma], SZ_FNAME, "%s%s")
		    call pargstr (Memc[sigroot])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Combine all images from the (subset) list.
	    iferr (call icombine (Memc[Memi[images+i-1]], Memi[nimages+i-1],
		Memc[output], Memc[plfile], Memc[sigma],
		Memc[logfile], delete, clobber)) {
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
	list = imtopenp ("input")
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


# ICOMBINE -- Combine the CCD images in a list.
# This procedure maps the images, sets the output dimensions and datatype,
# opens the logfile, and sets IMIO parameters.  It attempts to adjust
# buffer sizes and memory requirements for maximum efficiency.

procedure icombine (images, nimages, output, plfile, sigma, logfile,
	delete, clobber)

char	images[SZ_FNAME-1, nimages]	# Input images
int	nimages				# Number of images in list
char	output[ARB]			# Output image
char	plfile[ARB]			# Pixel list file
char	sigma[ARB]			# Output sigma image
char	logfile[ARB]			# Log filename
bool	delete				# Delete input images?
bool	clobber				# Clobber output images?

int	i, j, intype, bufsize, maxsize, memory, oldsize
pointer	sp, in, out[3], offsets, temp1, temp2, temp3

bool	strne()
int	getdatatype(), imaccess()
real	clgetr()
char	clgetc()
int	clgeti(), begmem(), errcode(), open(), ty_max(), sizeof()
pointer	immap(), ic_plfile()
errchk	immap, ic_plfile, ic_setout, ccddelete

include	"icombine.com"

define	retry_	98

begin
	call smark (sp)
	call salloc (in, nimages, TY_POINTER)
	call salloc (temp1, SZ_FNAME, TY_CHAR)
	call salloc (temp2, SZ_FNAME, TY_CHAR)
	call salloc (temp3, SZ_FNAME, TY_CHAR)

	# Check output images for existence.  If clobbering the output images
	# use temporary images for output and only replace the existing
	# image when the operation is successfully completed.

	call strcpy (output, Memc[temp1], SZ_FNAME)
	if (imaccess (Memc[temp1], READ_ONLY) == YES) {
	    if (clobber)
		call mktemp ("temp", Memc[temp1], SZ_FNAME)
	    else
		call error (0, "Output image exists")
	}

	call strcpy (plfile, Memc[temp2], SZ_FNAME)
	if (Memc[temp2] != EOS) {
	    if (imaccess (Memc[temp2], READ_ONLY) == YES) {
	        if (clobber)
		    call mktemp ("temp", Memc[temp2], SZ_FNAME)
	        else
		    call error (0, "Output pixel list image exists")
	    }
	}

	call strcpy (sigma, Memc[temp3], SZ_FNAME)
	if (Memc[temp3] != EOS) {
	    if (imaccess (Memc[temp3], READ_ONLY) == YES) {
	        if (clobber)
		    call mktemp ("temp", Memc[temp3], SZ_FNAME)
	        else
		    call error (0, "Output sigma image exists")
	    }
	}

	# Convert the nkeep parameter if needed.
        # Convert the pclip parameter to a number of pixels rather than
        # a fraction.  This number stays constant even if pixels are
        # rejected.  The number of low and high pixel rejected, however,
        # are converted to a fraction of the valid pixels.

	nkeep = clgeti ("nkeep")
	if (nkeep < 0)
	    nkeep = max (0, nimages + nkeep)

        if (reject == PCLIP) {
	    pclip = clgetr ("pclip")
	    if (pclip == 0.)
		call error (1, "Pclip parameter may not be zero")
	    if (IS_INDEFR (pclip))
		pclip = -0.5

            i = (nimages - 1) / 2.
            if (abs (pclip) < 1.)
                pclip = pclip * i
            if (pclip < 0.)
                pclip = min (-1, max (-i, int (pclip)))
            else
                pclip = max (1, min (i, int (pclip)))
        }

        if (reject == MINMAX) {
	    flow = clgetr ("nlow")
	    fhigh = clgetr ("nhigh")
	    if (IS_INDEFR (flow))
		flow = 0
	    if (IS_INDEFR (fhigh))
		fhigh = 0

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
		call sfree (sp)
		return
            }
        }

	# Map the input images.
	bufsize = 0
retry_
	do i = 1, nimages
	    iferr (Memi[in+i-1] = immap (images[1,i], READ_ONLY, 0)) {
		do j = 1, i-1
		    call imunmap (Memi[in+i-1])
		call sfree (sp)
		call erract (EA_ERROR)
	    }

        # Map the output image and set dimensions and offsets.
        out[1] = immap (Memc[temp1], NEW_COPY, Memi[in])
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
        if (Memc[temp2] != EOS) {
            out[2] = ic_plfile (Memc[temp2], NEW_COPY, out[1])
	    call imastr (out[2], "TEMPNAME", plfile)
        } else
            out[2] = NULL

        # Open the sigma image if given.
        if (Memc[temp3] != EOS) {
            out[3] = immap (Memc[temp3], NEW_COPY, out[1])
	    call imastr (out[3], "TEMPNAME", sigma)
            IM_PIXTYPE(out[3]) = ty_max (TY_REAL, IM_PIXTYPE(out[1]))
            call sprintf (IM_TITLE(out[3]), SZ_IMTITLE,
                "Combine sigma images for %s")
                call pargstr (output)
        } else
            out[3] = NULL

        # This is done hear to work around problem adding a keyword to
        # an NEW_COPY header and then using that header in a NEW_COPY.
        call imastr (out[1], "TEMPNAME", output)

        # Open masks.
        call ic_mopen (Memi[in], out, nimages)

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
	# images and files, divide the IMIO buffer size in half and try again.

        iferr {
            switch (intype) {
            case TY_SHORT:
                call icombines (Memi[in], out, Memi[offsets], nimages, bufsize)
            default:
                call icombiner (Memi[in], out, Memi[offsets], nimages, bufsize)
            }
        } then {
	    call ic_mclose (nimages)
	    if (!project) {
		do j = 2, nimages
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
	    call imunmap (out[1])
	    call imdelete (output)
	    call imunmap (Memi[in])
	    if (logfd != NULL)
		call close (logfd)
            switch (errcode()) {
            case SYS_MFULL:
                bufsize = bufsize / 2
		call sfree (sp)
                goto retry_
            default:
		call fixmem (oldsize)
		call sfree (sp)
                call erract (EA_ERROR)
            }
        }

        # Unmap all the images, close the log file, and restore memory.
	# The input images must be unmapped first to insure that there
	# is a FD for the output images since the headers are opened to
	# update them.  However, the order of the NEW_COPY pointers must
	# be preserved; i.e. the output depends on the first input image,
	# and the extra output images depend on the output image.

	if (!project) {
	    do i = 2, nimages {
		call imunmap (Memi[in+i-1])
		if (delete)
		    call ccddelete (images[1,i])
	    }
	}
	if (out[2] != NULL) {
	    call imdelf (out[2], "TEMPNAME")
	    call imunmap (out[2])
	    if (strne (Memc[temp2], plfile)) {
	        call ccddelete (plfile)
	        call imrename (Memc[temp3], plfile)
	    }
	}
	if (out[3] != NULL) {
	    call imdelf (out[3], "TEMPNAME")
	    call imunmap (out[3])
	    if (strne (Memc[temp3], sigma)) {
	        call ccddelete (sigma)
	        call imrename (Memc[temp3], sigma)
	    }
	}
	call imdelf (out[1], "TEMPNAME")
	call imunmap (out[1])
	if (strne (Memc[temp1], output)) {
	    call ccddelete (output)
	    call imrename (Memc[temp1], output)
	}
	call imunmap (Memi[in])
	if (delete)
	    call ccddelete (images[1,1])
	if (logfd != NULL)
	    call close (logfd)
	call ic_mclose (nimages)

	call fixmem (oldsize)
	call sfree (sp)
end


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, order[8]
data	order/TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=7) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=7) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end


# IC_PLFILE -- Map pixel list file
# This routine strips any image extensions and then adds .pl.

pointer procedure ic_plfile (plfile, mode, refim)

char    plfile[ARB]             # Pixel list file name
int     mode                    # Image mode
pointer refim                   # Reference image
pointer pl                      # IMIO pointer (returned)

int     i, strlen()
bool    streq
pointer sp, str, immap()

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
