include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	"ccdred.h"
include	"icombine.h"


# T_COMBINE -- Combine CCD images.
# This task is a copy of IMAGES.IMCOMBINE except that it recognizes the
# CCD types and can group images by AMP and SUBSET.  It also uses header
# keyword translation for the exposure times.

procedure t_combine ()

pointer	images			# Images
pointer	extns			# Image extensions for each subset
pointer	subsets			# Subsets
pointer	nimages			# Number of images in each subset
int	nsubsets		# Number of subsets
pointer	outroot			# Output root image name
pointer	plroot			# Output pixel list root name
pointer	sigroot			# Output root sigma image name
pointer	logfile			# Log filename
bool	delete			# Delete input images?

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
	# amps, subsets and ccdtypes.  Get the input images.
	# There must be a least one image in order to continue.

	call clgstr ("instrument", Memc[output], SZ_FNAME)
	call hdmopen (Memc[output])
	call cmb_images (images, extns, subsets, nimages, nsubsets)
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

	# This is here for backward compatibility.
	if (clgetb ("clobber"))
	    call error (1, "Clobber option is no longer supported")

	# Combine each input subset.
	do i = 1, nsubsets {
	    # Set the output, pl, and sigma image names with subset extension.

	    call strcpy (Memc[outroot], Memc[output], SZ_FNAME)
	    call sprintf (Memc[output], SZ_FNAME, "%s%s")
		call pargstr (Memc[outroot])
		call pargstr (Memc[Memi[extns+i-1]])

	    call strcpy (Memc[plroot], Memc[plfile], SZ_FNAME)
	    if (Memc[plfile] != EOS) {
		call sprintf (Memc[plfile], SZ_FNAME, "%s%s")
		    call pargstr (Memc[plroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[sigroot], Memc[sigma], SZ_FNAME)
	    if (Memc[sigma] != EOS) {
		call sprintf (Memc[sigma], SZ_FNAME, "%s%s")
		    call pargstr (Memc[sigroot])
		    call pargstr (Memc[Memi[extns+i-1]])
	    }

	    # Combine all images from the (subset) list.
	    iferr (call icombine (Memc[Memi[images+i-1]], Memi[nimages+i-1],
		Memc[output], Memc[plfile], Memc[sigma],
		Memc[logfile], NO, delete)) {
		call erract (EA_WARN)
	    }
	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[extns+i-1], TY_CHAR)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}

	# Finish up.
	call mfree (images, TY_POINTER)
	call mfree (extns, TY_POINTER)
	call mfree (subsets, TY_POINTER)
	call mfree (nimages, TY_INT)
	call hdmclose ()
	call sfree (sp)
end


# CMB_IMAGES -- Get images from a list of images.
# The images are filtered by ccdtype and sorted by amplifier and subset.
# The allocated lists must be freed by the caller.

procedure cmb_images (images, extns, subsets, nimages, nsubsets)

pointer	images		# Pointer to lists of subsets (allocated)
pointer	extns		# Image extensions for each subset (allocated)
pointer	subsets		# Subset names (allocated)
pointer	nimages		# Number of images in subset (allocated)
int	nsubsets	# Number of subsets

int	list		# List of input images
bool	doamps		# Divide input into subsets by amplifier?
bool	dosubsets	# Divide input into subsets by subset parameter?
bool	extend		# Add extensions to output image names?

int	i, nimage, ccdtype
pointer	sp, type, image, extn, subset, str, ptr, im
#int	imtopenp(), imtlen(), imtgetim(), ccdtypecl(), ccdtypes()
int	imtopenp(), imtlen(), imtgetim()
pointer	immap()
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

	# Determine whether to divide images into subsets and append extensions.
	#doamps = clgetb ("amps")
	doamps = false
	dosubsets = clgetb ("subsets")
	#extend = clgetb ("extensions")
	extend = true

	call smark (sp)
	call salloc (type, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (subset, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Go through the input list and eliminate images not satisfying the
	# CCD image type.  Separate into subsets if desired.  Create image
	# and subset lists.

	#ccdtype = ccdtypecl ("ccdtype", Memc[type], SZ_FNAME)
	ccdtype = 0
	call clgstr ("ccdtype", Memc[type], SZ_FNAME)
	call xt_stripwhite (Memc[type])

	while (imtgetim (list, Memc[image], SZ_FNAME)!=EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    #ccdtype = ccdtypes (im, Memc[str], SZ_FNAME)
	    call ccdtypes (im, Memc[str], SZ_FNAME)
	    if (Memc[type] != EOS && !streq (Memc[str], Memc[type]))
		next
	    
	    Memc[extn] = EOS
	    Memc[subset] = EOS
	    if (doamps) {
		#call ccdamp (im, Memc[str], SZ_FNAME)
		Memc[str] = EOS
		if (extend)
		    call strcat (Memc[str], Memc[extn], SZ_FNAME)
		call strcat (Memc[str], Memc[subset], SZ_FNAME)
	    }
	    if (dosubsets) {
		call ccdsubset (im, Memc[str], SZ_FNAME)
		call strcat (Memc[str], Memc[extn], SZ_FNAME)
		call strcat (Memc[str], Memc[subset], SZ_FNAME)
	    }
	    for (i=1; i <= nsubsets; i=i+1)
		if (streq (Memc[subset], Memc[Memi[subsets+i-1]]))
		    break

	    if (i > nsubsets) {
		if (nsubsets == 0) {
		    call malloc (images, nimage, TY_POINTER)
		    call malloc (extns, nimage, TY_POINTER)
		    call malloc (subsets, nimage, TY_POINTER)
		    call malloc (nimages, nimage, TY_INT)
		} else if (mod (nsubsets, nimage) == 0) {
		    call realloc (images, nsubsets+nimage, TY_POINTER)
		    call realloc (extns, nsubsets+nimage, TY_POINTER)
		    call realloc (subsets, nsubsets+nimage, TY_POINTER)
		    call realloc (nimages, nsubsets+nimage, TY_INT)
		}
		nsubsets = i
		call malloc (ptr, SZ_FNAME, TY_CHAR)
		call strcpy (Memc[image], Memc[ptr], SZ_FNAME-1)
		Memi[images+i-1] = ptr
		call malloc (ptr, SZ_FNAME, TY_CHAR)
		call strcpy (Memc[extn], Memc[ptr], SZ_FNAME)
		Memi[extns+i-1] = ptr
		call malloc (ptr, SZ_FNAME, TY_CHAR)
		call strcpy (Memc[subset], Memc[ptr], SZ_FNAME)
		Memi[subsets+i-1] = ptr
		Memi[nimages+i-1] = 1
	    } else {
		ptr = Memi[images+i-1]
		nimage = Memi[nimages+i-1] + 1
		call realloc (ptr, nimage * SZ_FNAME, TY_CHAR)
		Memi[images+i-1] = ptr
		Memi[nimages+i-1] = nimage
		ptr = ptr + (nimage - 1) * SZ_FNAME
		call strcpy (Memc[image], Memc[ptr], SZ_FNAME-1)
	    }
		    
	    call imunmap (im)
	}
	call realloc (images, nsubsets, TY_POINTER)
	call realloc (extns, nsubsets, TY_POINTER)
	call realloc (subsets, nsubsets, TY_POINTER)
	call realloc (nimages, nsubsets, TY_INT)
	call imtclose (list)
	call sfree (sp)
end


# ICOMBINE -- Combine the CCD images in a list.
# This procedure maps the images, sets the output dimensions and datatype,
# opens the logfile, and sets IMIO parameters.  It attempts to adjust
# buffer sizes and memory requirements for maximum efficiency.

procedure icombine (images, nims, output, plfile, sigma, logfile, stack,
	delete)

char	images[SZ_FNAME-1, nims]	# Input images
int	nims				# Number of images in list
char	output[ARB]			# Output image
char	plfile[ARB]			# Pixel list file
char	sigma[ARB]			# Output sigma image
char	logfile[ARB]			# Log filename
int	stack				# Stack input images?
bool	delete				# Delete input images?

char	errstr[SZ_LINE]
int	i, j, nimages, intype, bufsize, maxsize, memory, oldsize, stack1, err
pointer	sp, sp1, in, out[3], offsets, temp, key, tmp

int	getdatatype()
real	clgetr()
char	clgetc()
int	clgeti(), begmem(), errget(), open(), ty_max(), sizeof()
pointer	immap(), ic_plfile()
errchk	ic_imstack, immap, ic_plfile, ic_setout, ccddelete

include	"icombine.com"

define	retry_	98
define	done_	99

begin
	call smark (sp)

	# Set number of images to combine.
	if (project) {
	    if (nims > 1) {
		call sfree (sp)
		call error (1, "Cannot project combine a list of images")
	    }
	    tmp = immap (images[1,1], READ_ONLY, 0); out[1] = tmp
	    if (IM_NDIM(out[1]) == 1)
		call error (1, "Can't project one dimensional images")
	    nimages = IM_LEN(out[1],IM_NDIM(out[1]))
	    call imunmap (out[1])
	} else
	    nimages = nims

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

            i = nimages / 2.
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
	stack1 = stack
retry_
	iferr {
	    out[1] = NULL
	    out[2] = NULL
	    out[3] = NULL
	    icm = NULL
	    logfd = NULL

	    call smark (sp1)
	    if (stack1 == YES) {
		call salloc (temp, SZ_FNAME, TY_CHAR)
		call mktemp ("tmp", Memc[temp], SZ_FNAME)
		call ic_imstack (images, nimages, Memc[temp])
		project = true
	    }

	    # Map the input image(s).
	    if (project) {
		if (stack1 == YES) {
		    tmp = immap (Memc[temp], READ_ONLY, 0); out[1] = tmp
		} else {
		    tmp = immap (images[1,1], READ_ONLY, 0); out[1] = tmp
		}
		nimages = IM_LEN(out[1],IM_NDIM(out[1]))
		call calloc (in, nimages, TY_POINTER)
		call amovki (out[1], Memi[in], nimages)
	    } else {
		call calloc (in, nimages, TY_POINTER)
		do i = 1, nimages {
		    tmp = immap (images[1,i], READ_ONLY, 0); Memi[in+i-1] = tmp
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

	    # This is done here to work around problem adding a keyword to
	    # an NEW_COPY header and then using that header in a NEW_COPY.

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
	    # images and files, divide the IMIO buffer size in half and try
	    # again.

	    switch (intype) {
	    case TY_SHORT:
		call icombines (Memi[in], out, Memi[offsets], nimages,
		    bufsize)
	    default:
		call icombiner (Memi[in], out, Memi[offsets], nimages,
		    bufsize)
	    }
	} then {
	    err = errget (errstr, SZ_LINE)
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
		call sfree (sp1)
		goto retry_
	    case SYS_FTOOMANYFILES, SYS_IKIOPIX:
		if (!project) {
		    stack1 = YES
		    call sfree (sp1)
		    goto retry_
		}
		if (stack1 == YES)
		    call imdelete (Memc[temp])
		call fixmem (oldsize)
		call sfree (sp1)
		call error (err, errstr)
	    default:
		if (stack1 == YES)
		    call imdelete (Memc[temp])
		call fixmem (oldsize)
		call sfree (sp1)
		call error (err, errstr)
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
		if (Memi[in+i-1] != NULL) {
		    call imunmap (Memi[in+i-1])
		    if (delete)
			call ccddelete (images[1,i])
		}
	    }
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
	    call imdelete (Memc[temp])
	if (delete)
	    call ccddelete (images[1,1])
	if (logfd != NULL)
	    call close (logfd)
	if (icm != NULL)
	    call ic_mclose (nimages)

	call fixmem (oldsize)
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
