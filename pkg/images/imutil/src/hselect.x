# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<evexpr.h>
include	<ctype.h>

define	LEN_USERAREA	28800		# allow for the largest possible header


# HSELECT -- Perform a relational select operation upon a set of images.
# Our function is to select all images from the input set matching some
# criteria, printing the listed fields of each selected image on the standard
# output in list form.
#
# N.B.: this task shares code with the HEDIT task.

procedure t_hselect()

pointer	sp, im, image, fields, expr, missing, section
int	imlist, ip, min_lenuserarea
int	imtopenp(), imtgetim(), envfind(), ctoi()
pointer	immap()

begin
	call smark (sp)
	call salloc (image,   SZ_FNAME, TY_CHAR)
	call salloc (fields,  SZ_LINE,  TY_CHAR)
	call salloc (expr,    SZ_LINE,  TY_CHAR)
	call salloc (missing, SZ_LINE,  TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	# Get the primary operands.
	imlist = imtopenp ("images")
	call clgstr ("fields", Memc[fields], SZ_LINE)
	call clgstr ("expr",   Memc[expr],   SZ_LINE)
	call clgstr ("missing", Memc[missing], SZ_LINE)

	# Main processing loop.  An image is processed in each pass through
	# the loop.

	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Check size of user area
	    if (envfind ("min_lenuserarea", Memc[section], SZ_FNAME) > 0) {
		ip = 1
		if (ctoi (Memc[section], ip, min_lenuserarea) <= 0)
		    min_lenuserarea = LEN_USERAREA
		else
		    min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
	    } else
		min_lenuserarea = LEN_USERAREA

	    # Open the image.
	    iferr (im = immap (Memc[image], READ_ONLY, min_lenuserarea)) {
		call erract (EA_WARN)
		next
	    }

	    call he_getopsetimage (im, Memc[image], Memc[image])
	    call hs_select (im, Memc[image], Memc[fields], Memc[expr],
	        Memc[missing])

	    call imunmap (im)
	    call flush (STDOUT)
	}

	call imtclose (imlist)
	call sfree (sp)
end


# HS_SELECT -- Evaluate the user supplied boolean expression using the
# header parameter values for an image, and print the values of the listed
# parameters on the standard output if the expression is true.

procedure hs_select (im, image, fields, expr, missing)

pointer	im			# image descriptor
char	image[ARB]		# name of image being evaluated
char	fields[ARB]		# fields to be passed if record is selected
char	expr[ARB]		# exression to be evaluated
char	missing[ARB]		# missing output value

int	fieldno
pointer	o, sp, field, value, flist
pointer	evexpr(), imofnlu()
int	locpr(), imgnfn()
extern	he_getop()
errchk	evexpr, imofnlu, imgnfn

begin
	call smark (sp)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_LINE,  TY_CHAR)

	# Evaluate selection criteria.
	o = evexpr (expr, locpr(he_getop), 0)
	if (O_TYPE(o) != TY_BOOL)
	    call error (1, "expression must be boolean")

	# Print the values of the listed fields if the record was selected.
	if (O_VALB(o)) {
	    flist = imofnlu (im, fields)

	    fieldno = 1
	    while (imgnfn (flist, Memc[field], SZ_FNAME) != EOF) {
		iferr {
		    call he_gval (im, image, Memc[field], Memc[value], SZ_LINE)
		} then {
		    call printf ("\t%s")
		        call pargstr (missing)
		} else {
		    if (fieldno == 1) {
			call printf ("%s")
			    call he_pargstr (Memc[value])
		    } else {
			call printf ("\t%s")
			    call he_pargstr (Memc[value])
		    }
		}
		fieldno = fieldno + 1
	    }
	    call printf ("\n")

	    call imcfnl (flist)
	    call flush (STDOUT)
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end
