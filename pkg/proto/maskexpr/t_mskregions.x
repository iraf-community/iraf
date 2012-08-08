include <fset.h>
include <ctype.h>
include <imhdr.h>
include <imset.h>
include <pmset.h>

define  RG_NUMOPTIONS	"|constant|number|"
define	RG_CONSTANT	1
define	RG_NUMBER	2

# T_MSKREGIONS -- Create or edit a list of pixel masks using regions
# descriptors and a list of reference images.
#
# The regions descriptor may define a single region or a region expression.
# For example a circle may be defined as a single region, e.g.
# 
# circle xc yc radius
#
# whereas the overlap of two circular regions may be defined as a region
# expression
#
# circle (xc1, yc1, r1) && circle (xc2, yc2, r2) 
#
# note that brackets are necessary in one case and not the other and can
# be used to decide whether or not to send the regions  descriptor off to
# the parser as opposed to sending it off to a simple interpreter.
#
# The regions input operands must be one of the builtin region functions.
#

procedure t_mskregions()

pointer	sp, exprdb, dims, regnumber, uaxlen, mskname, imname, regfname
pointer	st, refim, pmim, expr, xexpr
int	reglist, msklist, imlist, undim, regval, depth, regfd, pregval
int	ip, npix, ch, regno, pregno
char	lbrackett
bool	verbose, append

pointer	pl

pointer	me_getexprdb(), immap(), me_expandtext(), pl_create()
int	clpopnu(), imtopenp(), clplen(), imtlen(), clgeti(), ctoi(), clgfil()
int	imtgetim(), imaccess(), strmatch(), imstati(), fscan(), open()
int	strdic(), stridx()
bool	clgetb(), strne()
data	lbrackett /'('/
errchk	immap()

begin
	# Get the regions file list.
	reglist = clpopnu ("regions")
	if (clplen (reglist) <= 0) {
	    call eprintf ("The regions file list is empty\n")
	    call clpcls (reglist)
	    return
	}

	# Get the output mask list.
	msklist = imtopenp ("masks")
	if (imtlen (msklist) <= 0) {
	    call eprintf ("The output mask list is empty\n")
	    call imtclose (msklist)
	    call clpcls (reglist)
	    return
	} else if (clplen (reglist) > 1 && clplen (reglist) !=
	    imtlen (msklist)) {
	    call eprintf ("The regions and mask list have different sizes\n")
	    call imtclose (msklist)
	    call clpcls (reglist)
	    return
	}

	# Get the output image list.
	imlist = imtopenp ("refimages")
	if (imtlen (imlist) > 0 && imtlen (imlist) != imtlen (msklist)) {
	    call eprintf (
	    "The reference image and mask lists are not the same size\n")
	    call imtclose (imlist) 
	    call imtclose (msklist)
	    call clpcls (reglist)
	    return
	}

	# Get some working space.
	call smark (sp)
	call salloc (exprdb, SZ_FNAME, TY_CHAR)
	call salloc (dims, SZ_FNAME, TY_CHAR)
	call salloc (regnumber, SZ_FNAME, TY_CHAR)
	call salloc (uaxlen, IM_MAXDIM, TY_LONG)
	call salloc (mskname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (regfname, SZ_FNAME, TY_CHAR)

	# Get remaining parameters,
	call clgstr ("dims", Memc[dims], SZ_FNAME)
	call clgstr ("regnumber", Memc[regnumber], SZ_FNAME)
	regno = strdic (Memc[regnumber], Memc[regnumber], SZ_FNAME,
	    RG_NUMOPTIONS)
	regval = clgeti ("regval")
	depth = clgeti ("depth")
	call clgstr ("exprdb", Memc[exprdb], SZ_PATHNAME)
	append = clgetb ("append")
	verbose = clgetb ("verbose")

	# Load the expression database if any.
	if (strne (Memc[exprdb], "none"))
	    st = me_getexprdb (Memc[exprdb])
	else
	    st = NULL

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
	regfd = NULL
	while (imtgetim (msklist, Memc[mskname], SZ_FNAME) != EOF) {

	    # Add .pl to output mask name.
	    if (strmatch (Memc[mskname], ".pl$") == 0)
		call strcat (".pl", Memc[mskname], SZ_FNAME)

	    # Check whether the output mask already exists.
	    if (imaccess (Memc[mskname], 0) == YES) {
		if (! append) {
		    if (verbose) {
		        call printf ("Mask %s already exists\n")
			    call pargstr (Memc[mskname])
		    }
		    next
		}
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

	    # Open the output mask.
	    if (imaccess (Memc[mskname], 0) == YES) {
		pmim = immap (Memc[mskname], READ_WRITE, 0) 
	    } else {
	        if (refim != NULL) {
		    pmim = immap (Memc[mskname], NEW_COPY, refim) 
	        } else {
		    pmim = immap (Memc[mskname], NEW_IMAGE, 0) 
		    IM_NDIM(pmim) = undim
		    call amovl (Meml[uaxlen], IM_LEN(pmim,1), undim)
	        }
		IM_PIXTYPE(pmim) = TY_INT
		pl = imstati (pmim, IM_PLDES)
		call pl_close (pl)
		#pl = pl_create (undim, Meml[uaxlen], depth)
		pl = pl_create (IM_NDIM(pmim), IM_LEN(pmim,1), depth)
		call imseti (pmim, IM_PLDES, pl)
		call imunmap (pmim)
		pmim = immap (Memc[mskname], READ_WRITE, 0) 
	    }

	    # Open the regions list.
	    if (clgfil (reglist, Memc[regfname], SZ_FNAME) != EOF) {
		if (regfd != NULL)
		    call close (regfd)
		regfd = open (Memc[regfname], READ_ONLY, TEXT_FILE)
	    } else if (regfd != NULL)
	        call seek (regfd, BOF)

	    # Print a header banner.
	    if (verbose) {
		if (refim == NULL) {
		    call printf ("Creating mask %s\n")
			call pargstr (Memc[mskname])
		} else {
		    call printf ("Creating mask %s using reference image %s\n")
			call pargstr (Memc[mskname])
			call pargstr (Memc[imname])
		}
		call printf ("    Using regions file %s\n")
		    call pargstr (Memc[regfname])
	    }

	    # Loop over the regions file.
	    pregval = regval
	    pregno = 1
	    while (fscan (regfd) != EOF) {

		# Get the expression.
	        call malloc (expr, SZ_LINE, TY_CHAR)
		call gargstr (Memc[expr], SZ_LINE)

		# Determine whether or not the region specificationis an
		# expression or a region description. If the string is
		# an expression expand it as necessary.
		if (stridx (lbrackett, Memc[expr]) > 0) {
		    if (st != NULL) {
	    	        xexpr = me_expandtext (st, Memc[expr])
		        call mfree (expr, TY_CHAR)
		        expr = xexpr
		    } 
		    call me_setexpr (Memc[expr], pmim, pregno, pregval, verbose)
		} else {
		    call me_setreg (Memc[expr], pmim, pregno, pregval, verbose)
		}

		# Increment the region number if appropriate.
		pregno = pregno + 1
		if (regno == RG_NUMBER)
		    pregval = pregval + 1

	        call mfree (expr, TY_CHAR)
	    }

	    # Save the output mask.
	    call imunmap (pmim)

	    # Close the reference image.
	    if (refim != NULL)
	        call imunmap (refim)

	}

	# Close the last regions file.
	if (regfd != NULL)
	    call close (regfd)

	# Close the expression database symbol table.
	if (st != NULL)
	    call stclose (st)

	# Close the various image and file lists.
	call imtclose (imlist)
	call imtclose (msklist)
	call clpcls (reglist)

	call sfree (sp)
end

