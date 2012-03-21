# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	<syserr.h>
include	"icombine.h"


# ICOMBINE -- Combine input list or image.
# This procedure maps the images, sets the output dimensions and datatype,
# opens the logfile, and sets IMIO parameters.  It attempts to adjust
# buffer sizes and memory requirements for maximum efficiency.

procedure icombine (list, output, headers, bmask, rmask, nrmask, emask,
	sigma, logfile, scales, zeros, wts, stack, delete, listonly)

int	list				#I List of input images
char	output[ARB]			#I Output image
char	headers[ARB]			#I Output header rootname
char	bmask[ARB]			#I Bad pixel mask
char	rmask[ARB]			#I Rejection mask
char	nrmask[ARB]			#I Nreject mask
char	emask[ARB]			#I Exposure mask
char	sigma[ARB]			#I Sigma image (optional)
char	logfile[ARB]			#I Logfile (optional)
real	scales[ARB]			#I Scale factors
real	zeros[ARB]			#I Offset factors
real	wts[ARB]			#I Weights
int	stack				#I Stack input images?
int	delete				#I Delete input images?
int	listonly			#I List images to combine?

bool	proj
char	input[SZ_FNAME], errstr[SZ_LINE]
int	i, j, nimages, intype, bufsize, oldsize, stack1, err, retry
int	maxsize, maxmemory, memory
pointer	sp, im, in1, in, out[6], offsets, key, tmp, bpmstack 

char	clgetc()
int	clgwrd(), imtlen(), imtgetim(), imtrgetim(), getdatatype(), envgeti()
int	begmem(), errget(), open(), ty_max(), sizeof(), strmatch()
pointer	immap(), xt_immap(), ic_pmmap()
errchk	ic_imstack, immap, imunmap, xt_immap, ic_pmmap, ic_setout

include	"icombine.com"

define	retry_	98
define	err_	99

begin
	if (listonly == YES) {
	    # Write the output list.
	    if (output[1] == EOS) {
		call imtrew (list)
		while (imtgetim (list, input, SZ_FNAME)!=EOF) {
		    i = strmatch (input, "[0]") - 3
		    if (i > 0)
		        call strcpy (input[i+3], input[i], SZ_FNAME)
		    call printf ("%s\n")
			call pargstr (input)
		}
	    } else {
		call sprintf (errstr, SZ_LINE, "%s.list")
		    call pargstr (output)
		iferr (logfd = open (errstr, APPEND, TEXT_FILE))
		    call erract (EA_WARN)
		call imtrew (list)
		while (imtgetim (list, input, SZ_FNAME)!=EOF) {
		    i = strmatch (input, "[0]") - 3
		    if (i > 0)
		        call strcpy (input[i+3], input[i], SZ_FNAME)
		    call printf ("%s -> %s\n")
			call pargstr (input)
			call pargstr (errstr)
		    call fprintf (logfd, "%s\n")
			call pargstr (input)
		}
		call close (logfd)
	    }
	    return
	}

	nimages = imtlen (list)
	if (nimages == 0)
	    call error (1, "No images to combine")

	if (project) {
	    if (imtgetim (list, input, SZ_FNAME) == EOF)
		call error (1, "No image to project")
	}

	bufsize = 0
#	if (nimages > LAST_FD - 15)
#	    stack1 = YES
#	else
	    stack1 = stack

	retry = 0

retry_
	iferr {
	    call smark (sp)
	    call salloc (in, 1, TY_POINTER)

	    nimages = 0
	    in1 = NULL; Memi[in] = NULL; logfd = NULL
	    out[1] = NULL; out[2] = NULL; out[3] = NULL
	    out[4] = NULL; out[5] = NULL; out[6] = NULL

	    # Stack the input images.
	    if (stack1 == YES) {
		proj = project
		project = true
		call salloc (bpmstack, SZ_FNAME, TY_CHAR)
		i = clgwrd ("masktype", Memc[bpmstack], SZ_FNAME, MASKTYPES)
		if (i == M_NONE)
		    Memc[bpmstack] = EOS
		else {
		    call mktemp ("tmp", Memc[bpmstack], SZ_FNAME)
		    call strcat (".pl", Memc[bpmstack], SZ_FNAME)
		}
		call mktemp ("tmp", input, SZ_FNAME)
		call imtrew (list)
		call ic_imstack (list, input, Memc[bpmstack])
	    }

	    # Open the input image(s).
	    if (project) {
		tmp = immap (input, READ_ONLY, 0); out[1] = tmp
		if (IM_NDIM(out[1]) == 1)
		    call error (1, "Can't project one dimensional images")
		nimages = IM_LEN(out[1],IM_NDIM(out[1]))
		call salloc (in, nimages, TY_POINTER)
		call amovki (out[1], Memi[in], nimages)
	    } else {
		call salloc (in, imtlen(list), TY_POINTER)
		call amovki (NULL, Memi[in], imtlen(list))
		call imtrew (list)
		while (imtgetim (list, input, SZ_FNAME)!=EOF) {
		    nimages = nimages + 1
		    tmp = xt_immap (input, READ_ONLY, 0, nimages, retry)
		    Memi[in+nimages-1] = tmp
		}

		# Check sizes and set I/O option.
		intype = 0
		tmp = Memi[in]
		do i = 2, nimages {
		    do j = 1, IM_NDIM(tmp) {
			if (IM_LEN(tmp,j) != IM_LEN(Memi[in+i-1],j))
			    intype = 1
		    }
		    if (intype == 1)
			break
		}
		if (intype == 1)
		    call xt_imseti (0, "option", intype)
	    }

	    # Check if there are no images.
	    if (nimages == 0)
		call error (1, "No images to combine")

	    # Convert the pclip parameter to a number of pixels rather than
	    # a fraction.  This number stays constant even if pixels are
	    # rejected.  The number of low and high pixel rejected, however,
	    # are converted to a fraction of the valid pixels.

	    if (reject == PCLIP) {
		i = nimages / 2.
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
		else if (i + j >= nimages)
		    call error (1, "Bad minmax rejection parameters")
	    }

	    # Map the output image and set dimensions and offsets.
	    if (stack1 == YES) {
		call imtrew (list)
		i = imtgetim (list, errstr, SZ_LINE)
		in1 = immap (errstr, READ_ONLY, 0)
		tmp = immap (output, NEW_COPY, in1); out[1] = tmp
		call salloc (key, SZ_FNAME, TY_CHAR)
		do i = 1, nimages {
		    call sprintf (Memc[key], SZ_FNAME, "stck%04d")
			call pargi (i)
		    iferr (call imdelf (out[1], Memc[key]))
			;
		    if (Memc[bpmstack] != EOS) {
			call sprintf (Memc[key], SZ_FNAME, "bpm%04d")
			    call pargi (i)
			iferr (call imdelf (out[1], Memc[key]))
			    ;
		    }
		}
	    } else {
		tmp = immap (output, NEW_COPY, Memi[in]); out[1] = tmp
		if (project) {
		    IM_LEN(out[1],IM_NDIM(out[1])) = 1
		    IM_NDIM(out[1]) = IM_NDIM(out[1]) - 1
		}
	    }
	    call salloc (offsets, nimages*IM_NDIM(out[1]), TY_INT)
	    iferr (call ic_setout (Memi[in], out, Memi[offsets], nimages)) {
		call erract (EA_WARN)
		call error (1, "Can't set output geometry")
	    }
	    call ic_hdr (Memi[in], out, nimages)
	    iferr (call imdelf (out, "BPM"))
		;

	    # Determine the highest precedence datatype and set output datatype.
	    intype = IM_PIXTYPE(Memi[in])
	    do i = 2, nimages
		intype = ty_max (intype, IM_PIXTYPE(Memi[in+i-1]))
	    IM_PIXTYPE(out[1]) = getdatatype (clgetc ("outtype"))
	    if (IM_PIXTYPE(out[1]) == ERR)
		IM_PIXTYPE(out[1]) = intype

	    # Open rejection masks
	    if (rmask[1] != EOS) {
		tmp = ic_pmmap (rmask, NEW_COPY, out[1]); out[4] = tmp
		IM_NDIM(out[4]) = IM_NDIM(out[4]) + 1
		IM_LEN(out[4],IM_NDIM(out[4])) = nimages
		if (!project) {
		    if (key == NULL)
			call salloc (key, SZ_FNAME, TY_CHAR)
		    do i = 100, nimages {
			j = imtrgetim (list, i, input, SZ_FNAME)
			if (i < 999)
			    call sprintf (Memc[key], SZ_FNAME, "imcmb%d")
			else if (i < 9999)
			    call sprintf (Memc[key], SZ_FNAME, "imcm%d")
			else
			    call sprintf (Memc[key], SZ_FNAME, "imc%d")
			    call pargi (i)
			call imastr (out[4], Memc[key], input)
		    }
		}
	    } else
		out[4] = NULL

	    # Open bad pixel pixel list file if given.
	    if (bmask[1] != EOS) {
		tmp = ic_pmmap (bmask, NEW_COPY, out[1]); out[2] = tmp
	    } else
		out[2] = NULL

	    # Open nreject pixel list file if given.
	    if (nrmask[1] != EOS) {
		tmp = ic_pmmap (nrmask, NEW_COPY, out[1]); out[5] = tmp
	    } else
		out[5] = NULL

	    # Open exposure mask if given.
	    if (emask[1] != EOS) {
		tmp = ic_pmmap (emask, NEW_COPY, out[1]); out[6] = tmp
	    } else
		out[6] = NULL

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
	    call ic_mopen (Memi[in], out, nimages, Memi[offsets],
	        min(retry,1))

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

		iferr (maxmemory = envgeti ("imcombine_maxmemory"))
		    maxmemory = MAXMEMORY
		memory = begmem (0, oldsize, maxsize)
		memory = min (memory, maxsize, maxmemory)
		bufsize = FUDGE * memory / (nimages + 1) / sizeof (intype)
	    }

	    # Combine the images.  If an out of memory error occurs close all
	    # images and files, divide the IMIO buffer size in half and try
	    # again. 

	    switch (ty_max (intype, IM_PIXTYPE(out[1]))) {
	    case TY_SHORT:
		call icombines (Memi[in], out, scales, zeros,
		    wts, Memi[offsets], nimages, bufsize)
	    case TY_USHORT, TY_INT, TY_LONG:
		call icombinei (Memi[in], out, scales, zeros,
		    wts, Memi[offsets], nimages, bufsize)
	    case TY_DOUBLE:
		call icombined (Memi[in], out, scales, zeros,
		    wts, Memi[offsets], nimages, bufsize)
	    case TY_COMPLEX:
		call error (1, "Complex images not allowed")
	    default:
		call icombiner (Memi[in], out, scales, zeros,
		    wts, Memi[offsets], nimages, bufsize)
	    }
	} then {
	    err = errget (errstr, SZ_LINE)
	    if (err == SYS_IKIOPIX && nimages < 250)
		err = SYS_MFULL
	    call ic_mclose (nimages)
	    if (!project) {
		do j = 2, nimages {
		    if (Memi[in+j-1] != NULL)
			call xt_imunmap (Memi[in+j-1], j)
		}
	    }
	    if (out[2] != NULL) {
		iferr (call imunmap (out[2]))
		    ;
		iferr (call imdelete (bmask))
		    ;
	    }
	    if (out[3] != NULL) {
		iferr (call imunmap (out[3]))
		    ;
		iferr (call imdelete (sigma))
		    ;
	    }
	    if (out[4] != NULL) {
		iferr (call imunmap (out[4]))
		    ;
		iferr (call imdelete (rmask))
		    ;
	    }
	    if (out[5] != NULL) {
		iferr (call imunmap (out[5]))
		    ;
		iferr (call imdelete (nrmask))
		    ;
	    }
	    if (out[6] != NULL) {
		iferr (call imunmap (out[6]))
		    ;
		iferr (call imdelete (emask))
		    ;
	    }
	    if (out[1] != NULL) {
		iferr (call imunmap (out[1]))
		    ;
		iferr (call imdelete (output))
		    ;
	    }
	    if (Memi[in] != NULL)
		call xt_imunmap (Memi[in], 1)
	    if (in1 != NULL)
		call imunmap (in1)
	    if (logfd != NULL)
		call close (logfd)

	    switch (err) {
	    case SYS_MFULL:
		if (project)
		    goto err_

		if (bufsize < 10000 && retry > 2) {
		    call strcat ("- Maybe min_lenuserarea is too large",
			errstr, SZ_LINE)
		    goto err_
		}

		bufsize = bufsize / 2
		retry = retry + 1
		call sfree (sp)
		goto retry_
	    case SYS_FTOOMANYFILES, SYS_IKIOPEN, SYS_IKIOPIX, SYS_FOPEN, SYS_FWTNOACC:
		if (project)
		    goto err_
		stack1 = YES
		call sfree (sp)
		goto retry_
	    default:
err_
		if (stack1 == YES) {
		    iferr (call imdelete (input))
			;
		    if (Memc[bpmstack] != EOS) {
			iferr (call imdelete (Memc[bpmstack]))
			    ;
		    }
		}
		call fixmem (oldsize)
		while (imtgetim (list, input, SZ_FNAME)!=EOF)
		    ;
		call sfree (sp)
		call error (err, errstr)
	    }
	}

	# Unmap all the images, close the log file, and restore memory.
	if (out[2] != NULL)
	    iferr (call imunmap (out[2]))
		call erract (EA_WARN)
	if (out[3] != NULL)
	    iferr (call imunmap (out[3]))
		call erract (EA_WARN)
	if (out[4] != NULL) {
	    # Close the output first so that there is no confusion with
	    # inheriting the output header.  Then update the WCS for the
	    # extra dimension.  Note that this may not be correct with
	    # axis reduced WCS.
	    iferr {
		call imunmap (out[4])
		out[4] = immap (rmask, READ_WRITE, 0)
		i = IM_NDIM(out[4])
		call imaddi (out[4], "WCSDIM", i)
		call sprintf (errstr, SZ_LINE, "LTM%d_%d")
		    call pargi (i)
		    call pargi (i)
		call imaddr (out[4], errstr, 1.)
		call sprintf (errstr, SZ_LINE, "CD%d_%d")
		    call pargi (i)
		    call pargi (i)
		call imaddr (out[4], errstr, 1.)
		call imunmap (out[4])
	    } then
		call erract (EA_WARN)
	}
	if (out[5] != NULL)
	    iferr (call imunmap (out[5]))
		call erract (EA_WARN)
	if (out[6] != NULL)
	    iferr (call imunmap (out[6]))
		call erract (EA_WARN)
	if (out[1] != NULL) {
	    call imunmap (out[1])
	    if (headers[1] != EOS) {
		# Write input headers to a multiextension file if desired.
		# This might be the same as the output image.
		iferr {
		    do i = 1, nimages {
			im = Memi[in+i-1]
			call imstats (im, IM_IMAGENAME, input, SZ_FNAME)
			if (strmatch (headers, ".fits$") == 0) {
			    call sprintf (errstr, SZ_LINE, "%s.fits[append]")
				call pargstr (headers)
			} else {
			    call sprintf (errstr, SZ_LINE, "%s[append]")
				call pargstr (headers)
			}
			tmp = immap (errstr, NEW_COPY, im)
			IM_NDIM(tmp) = 0
			do j = 1, IM_NDIM(im) {
			    call sprintf (errstr, SZ_LINE, "AXLEN%d")
				call pargi (j)
			    call imaddi (tmp, errstr, IM_LEN(im,j))
			}
			call imastr (tmp, "INIMAGE", input)
			call imastr (tmp, "OUTIMAGE", output)
			call imastr (tmp, "EXTNAME", input)
			call imunmap (tmp)
		    }
		    if (logfd != NULL) {
			call eprintf ("  Headers = %s\n")
			    call pargstr (headers)
		    }
		} then
		    call erract (EA_WARN)
	    }
	}
	if (!project) {
	    do i = 2, nimages {
		if (Memi[in+i-1] != NULL)
		    call xt_imunmap (Memi[in+i-1], i)
	    }
	}
	if (Memi[in] != NULL)
	    call xt_imunmap (Memi[in], 1)
	if (in1 != NULL)
	    call imunmap (in1)
	if (stack1 == YES) {
	    call imdelete (input)
	    if (Memc[bpmstack] != EOS)
		call imdelete (Memc[bpmstack])
	    project = proj
	}
	if (logfd != NULL)
	    call close (logfd)
	call ic_mclose (nimages)
	call fixmem (oldsize)
	call sfree (sp)
end
