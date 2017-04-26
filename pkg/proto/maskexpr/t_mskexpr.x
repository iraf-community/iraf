include <fset.h>
include <ctype.h>
include <imhdr.h>

# T_MSKEXPR -- Create a list of pixel masks using an expression and a list of
# reference images.
#
# The input expression may be any legal EVVEXPR expression which can be
# converted to a valid integer mask pixel value. The input operands must be one
# of, i for the reference image, i.keyword for a reference image header
# keyword, m for the input mask image, m.keyword for the input mask image
# header keyword a numeric constant, a builtin function, or the pixel operands
# I, J, K, etc. May be desirable to replace the reference image operand
# i with $I. This is a detail however.
#
# This task uses the get tokens library in images to expand the macros.
# This library should probably be removed from images and put in xtools
# for the applications programmers or in the core system as a useful
# library maybe in fmtio like imexpr. There is a similar if not identical(?)
# library in qpoe.

procedure t_mskexpr()

pointer	expr, st, xexpr, refim, pmim, refmsk
pointer	sp, exprdb, dims, uaxlen, mskname, imname, refname
int	i, ip, op, msklist, imlist, rmsklist, len_exprbuf, fd, nchars, ch
int	undim, npix, depth
bool	verbose

pointer	me_getexprdb(), me_expandtext(), immap(), yt_mappm(), me_mkmask()
long	fstatl()
int	imtopenp(), imtlen(), open(), getci(), imtgetim(), ctoi()
int	clgeti(), strmatch(), imaccess()
bool	clgetb(), strne()
errchk	immap(), yt_pmmap()

begin
	# Get the expression parameter.
	call malloc (expr, SZ_COMMAND, TY_CHAR)
	call clgstr ("expr", Memc[expr], SZ_COMMAND)

	# Get the output mask list.
	msklist = imtopenp ("masks")
	if (imtlen (msklist) <= 0) {
	    call eprintf ("The output mask list is empty\n")
	    call imtclose (msklist)
	    call mfree (expr, TY_CHAR)
	    return
	}

	# Get the input reference image list.
	imlist = imtopenp ("refimages")
	if (imtlen (imlist) > 0 && imtlen (imlist) != imtlen (msklist)) {
	    call eprintf (
	    "The reference image and output mask lists are not the same size\n")
	    call imtclose (imlist) 
	    call imtclose (msklist)
	    call mfree (expr, TY_CHAR)
	    return
	}

	# Get the input reference mask list.
	rmsklist = imtopenp ("refmasks")
	if (imtlen (rmsklist) > 0 && imtlen (rmsklist) != imtlen (msklist)) {
	    call eprintf (
	    "The reference image and output mask lists are not the same size\n")
	    call imtclose (rmsklist)
	    call imtclose (msklist)
	    call imtclose (imlist) 
	    call mfree (expr, TY_CHAR)
	    return
	}

	# Get some working space.
	call smark (sp)
	call salloc (exprdb, SZ_FNAME, TY_CHAR)
	call salloc (dims, SZ_FNAME, TY_CHAR)
	call salloc (uaxlen, IM_MAXDIM, TY_LONG)
	call salloc (mskname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (refname, SZ_FNAME, TY_CHAR)

	# Get remaining parameters,
	call clgstr ("exprdb", Memc[exprdb], SZ_PATHNAME)
	call clgstr ("dims", Memc[dims], SZ_FNAME)
	depth = clgeti ("depth")
	verbose = clgetb ("verbose")

	# Load the expression database if any.
	if (strne (Memc[exprdb], "none"))
	    st = me_getexprdb (Memc[exprdb])
	else
	    st = NULL

	# Get the expression to be evaluated and expand any file inclusions
	# or macro references.
	len_exprbuf = SZ_COMMAND
	if (Memc[expr] == '@') {
	    fd = open (Memc[expr+1], READ_ONLY, TEXT_FILE)
	    nchars = fstatl (fd, F_FILESIZE)
	    if (nchars > len_exprbuf) {
		len_exprbuf = nchars
		call realloc (expr, len_exprbuf, TY_CHAR)
	    }
	    for (op = expr;  getci(fd, ch) != EOF;  op = op + 1) {
		if (ch == '\n')
		    Memc[op] = ' '
		else
		    Memc[op] = ch
	    }
	    Memc[op] = EOS
	    call close (fd)
	}
	if (st != NULL) {
	    xexpr = me_expandtext (st, Memc[expr])
	    call mfree (expr, TY_CHAR)
	    expr = xexpr
	}
	if (verbose) {
	    call printf ("Expr: %s\n")
	        call pargstr (Memc[expr])
	    call flush (STDOUT)
	}

	# Determine the default dimension and size of the output image.  If the 
	# reference image is defined then the dimensions of the reference
	# image determine the dimensions of the output mask. Otherwise the
	# default dimensions are used.

	undim = 0
	call aclrl (Meml[uaxlen], IM_MAXDIM)
	for (ip = 1;  ctoi (Memc[dims], ip, npix) > 0;  ) {
	    Meml[uaxlen+undim] = npix
	    undim = undim + 1
	    for (ch = Memc[dims+ip-1];  IS_WHITE(ch) || ch == ',';
	        ch = Memc[dims+ip-1])
	        ip = ip + 1
	}

	# Loop over the output mask names.
	while (imtgetim (msklist, Memc[mskname], SZ_FNAME) != EOF) {

	    # Add .pl to output mask name.
	    if (strmatch (Memc[mskname], ".pl$") == 0)
		call strcat (".pl", Memc[mskname], SZ_FNAME)

	    # Check whether the output mask already exists.
	    if (imaccess (Memc[mskname], 0) == YES) {
		if (verbose) {
		    call printf ("Mask %s already exists\n")
			call pargstr (Memc[mskname])
		}
		next
	    }

	    # Open the reference image.
	    if (imtlen (imlist) > 0) {
		if (imtgetim (imlist, Memc[imname], SZ_FNAME) != EOF) {
		    iferr (refim = immap (Memc[imname], READ_ONLY, 0)) {
			refim = NULL
		        call printf (
			    "Cannot open reference image %s for mask %s\n")
			    call pargstr (Memc[imname])
			    call pargstr (Memc[mskname])
		        next
		    }
		} else {
		    refim = NULL
		    call printf ("Cannot open reference image for mask %s\n")
			call pargstr (Memc[mskname])
		    next
		}
	    } else
		refim = NULL

	    # Open the reference mask.
	    if (imtlen (rmsklist) > 0) {
		if (imtgetim (rmsklist, Memc[refname], SZ_FNAME) != EOF) {
		    if (refim != NULL) {
		        iferr (refmsk = yt_mappm (Memc[refname], refim,
			    "logical", Memc[refname], SZ_FNAME))
			    refmsk = NULL
		    } else {
			iferr (refmsk = immap (Memc[refname], READ_ONLY, 0))
			    refmsk = NULL
		    }
		    if (refmsk == NULL) {
		        call printf (
			    "Cannot open reference mask %s for mask %s\n")
			    call pargstr (Memc[refname])
			    call pargstr (Memc[mskname])
		        if (refim != NULL)
		            call imunmap (refim)
		        next
		    } else if (refim != NULL) {
			if (IM_NDIM(refim) != IM_NDIM(refmsk)) {
		            call printf (
			    "Reference image and mask for %s don't match\n")
			        call pargstr (Memc[mskname])
			    call imunmap (refmsk)
			    if (refim != NULL)
			        call imunmap (refim)
			    next
			} else {
			    do i = 1, IM_NDIM(refim) {
				if (IM_LEN(refim,i) == IM_LEN(refmsk,i))
				    next
				else
				    break
			    }
			    if (i <= IM_NDIM(refim)) {
		                call printf (
			        "Reference image and mask for %s don't match\n")
			            call pargstr (Memc[mskname])
			        call imunmap (refmsk)
			        if (refim != NULL)
			            call imunmap (refim)
				next
			    }
			}
		    }
		} else {
		    refmsk = NULL
		    call printf ("Cannot open reference mask for mask %s\n")
			call pargstr (Memc[refname])
		    if (refim != NULL)
		        call imunmap (refim)
		    next
		}
	    } else
		refmsk = NULL

	    if (verbose) {
		if (refim != NULL && refmsk != NULL) {
		    call printf ("Creating mask %s\n")
			call pargstr (Memc[mskname])
		    call printf ("    Using reference image %s and mask %s\n")
			call pargstr (Memc[imname])
			call pargstr (Memc[refname])
		} else if (refim != NULL) {
		    call printf ("Creating mask %s using reference image %s\n")
			call pargstr (Memc[mskname])
			call pargstr (Memc[imname])
		} else if (refmsk != NULL) {
		    call printf ("Creating mask %s using reference image %s\n")
			call pargstr (Memc[mskname])
			call pargstr (Memc[refname])
		} else {
		    call printf ("Creating mask %s\n")
			call pargstr (Memc[mskname])
		}
	    }

	    # Evalute the expression return a mask image pointer.
	    if (refim != NULL)
	        pmim = me_mkmask (Memc[expr], Memc[mskname], refim, refmsk,
	            IM_NDIM(refim), IM_LEN(refim,1), depth)
	    else if (refmsk != NULL)
	        pmim = me_mkmask (Memc[expr], Memc[mskname], refim, refmsk,
	            IM_NDIM(refmsk), IM_LEN(refmsk,1), depth)
	    else
	        pmim = me_mkmask (Memc[expr], Memc[mskname], refim, refmsk,
	            undim, Meml[uaxlen], depth)

	    # Save the mask.
	    call imunmap (pmim)

	    # Close the reference image.
	    if (refim != NULL)
	        call imunmap (refim)

	    # Close the reference mask.
	    if (refmsk != NULL)
	        call imunmap (refmsk)
	}

	# Cleanup.
	call mfree (expr, TY_CHAR) 
	if (st != NULL)
	    call stclose (st)
	call imtclose (rmsklist)
	call imtclose (msklist)
	call imtclose (imlist)
	call sfree (sp)
end

