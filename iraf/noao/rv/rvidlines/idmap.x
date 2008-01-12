include	<ctype.h>
include	<imhdr.h>
include	<pkg/gtools.h>
include	<smw.h>
include	"identify.h"

# Sepcial section words.
define	SPECIAL "|first|middle|x|y|z|last|column|line|band|"
define	FIRST	1
define	MIDDLE	2
define	X	3
define	Y	4
define	Z	5
define	LAST	6
define	COLUMN	7
define	LINE	8
define	BAND	9

# ID_MAP -- Map an image for IDENTIFY/REIDENTIFY
# The image must 1, 2, or 3 dimensional.  An image section may be given with
# the image name or with the CL parameter "section".  The CL parameter can
# have one of the following formats:
#	1. An IMIO image section
#	2. [line|column|x|y|z] [#|middle|last] [#|middle|last]
#	3. [#|middle|last] [#|middle|last] [line|column|x|y|z]
# where # is a line or column number.  The strings may be abbreviated.
# The task returns and error if it cannot map the image or determine
# the 1D line or column desired.

procedure id_map (id)

pointer	id			# IDENTIFY data structure pointer

int	i, j, k, l, a, b, c, x1[3], x2[3], xs[3]
pointer	sp, wrd1, wrd2, wrd3, im

int	imaccess(), strdic(), ctoi(), nscan()
pointer	immap()
errchk	immap, id_maphdr

begin
	# Separate the image name and image section and map the full image.
	call imgsection (Memc[ID_IMAGE(id)], Memc[ID_SECTION(id)], SZ_FNAME)
	call imgimage (Memc[ID_IMAGE(id)], Memc[ID_IMAGE(id)], SZ_FNAME)
	call id_noextn (Memc[ID_IMAGE(id)])
	im = immap (Memc[ID_IMAGE(id)], READ_ONLY, 0)

	# If no image section is found use the "section" parameter.
	if (Memc[ID_SECTION(id)] == EOS && IM_NDIM(im) > 1) {
	    call clgstr ("section", Memc[ID_SECTION(id)], SZ_FNAME)
	    call xt_stripwhite (Memc[ID_SECTION(id)])

	    # If not an image section construct one.
	    if (Memc[ID_SECTION(id)] != '[') {
	        call smark (sp)
	        call salloc (wrd1, SZ_FNAME, TY_CHAR)
	        call salloc (wrd2, SZ_FNAME, TY_CHAR)
	        call salloc (wrd3, SZ_FNAME, TY_CHAR)

		call sscan (Memc[ID_SECTION(id)])

		# Parse axis and elements.
		call gargwrd (Memc[wrd1], SZ_FNAME)
		call gargwrd (Memc[wrd2], SZ_FNAME)
		call gargwrd (Memc[wrd3], SZ_FNAME)
		switch (nscan()) {
		case 0:
		    a = X
		    b = MIDDLE
		    c = MIDDLE
		case 1:
		    a = strdic (Memc[wrd1], Memc[wrd1], SZ_FNAME, SPECIAL)
		    b = MIDDLE
		    c = MIDDLE
		case 2:
		    a = strdic (Memc[wrd1], Memc[wrd1], SZ_FNAME, SPECIAL)
		    if (a >= X)
			b = strdic (Memc[wrd2], Memc[wrd2], SZ_FNAME, SPECIAL)
		    else {
			b = a
			a = strdic (Memc[wrd2], Memc[wrd2], SZ_FNAME, SPECIAL)
			call strcpy (Memc[wrd1], Memc[wrd2], SZ_FNAME)
		    }
		    c = MIDDLE
		    call strcpy (Memc[wrd2], Memc[wrd3], SZ_FNAME)
		case 3:
		    a = strdic (Memc[wrd1], Memc[wrd1], SZ_FNAME, SPECIAL)
		    if (a >= X) {
			b = strdic (Memc[wrd2], Memc[wrd2], SZ_FNAME, SPECIAL)
			c = strdic (Memc[wrd3], Memc[wrd3], SZ_FNAME, SPECIAL)
		    } else {
			b = a
			a = strdic (Memc[wrd2], Memc[wrd2], SZ_FNAME, SPECIAL)
			if (a >= X) {
			    c = strdic (Memc[wrd3], Memc[wrd3],SZ_FNAME,SPECIAL)
			    call strcpy (Memc[wrd1], Memc[wrd2], SZ_FNAME)
			} else {
			    c = b
			    b = a
			    a = strdic (Memc[wrd3], Memc[wrd3],SZ_FNAME,SPECIAL)
			    call strcpy (Memc[wrd2], Memc[wrd3], SZ_FNAME)
			    call strcpy (Memc[wrd1], Memc[wrd2], SZ_FNAME)
			}
		    }
		}

		switch (a) {
		case X, LINE:
		    i = 1
		    j = 2
		    k = 3
		case Y, COLUMN:
		    i = 2
		    j = 1
		    k = 3
		case Z, BAND:
		    i = 3
		    j = 1
		    k = 2
		default:
		    call imunmap (im)
		    call error (1,
		    "Error in section specification or non-unique abbreviation")
		}

		switch (b) {
		case FIRST:
		    ID_LINE(id,1) = 1
		case MIDDLE:
		    ID_LINE(id,1) = (1 + IM_LEN(im,j)) / 2
		case LAST:
		    ID_LINE(id,1) = IM_LEN(im,j)
		default:
		    l = 1
		    if (ctoi (Memc[wrd2], l, ID_LINE(id,1)) == 0)
			call error (1, "Error in section specification")
		}

		switch (c) {
		case FIRST:
		    ID_LINE(id,2) = 1
		case MIDDLE:
		    ID_LINE(id,2) = (1 + IM_LEN(im,k)) / 2
		case LAST:
		    ID_LINE(id,2) = IM_LEN(im,k)
		default:
		    l = 1
		    if (ctoi (Memc[wrd3], l, ID_LINE(id,2)) == 0)
			call error (1, "Error in section specification")
		}

		# Format section.
		switch (IM_NDIM(im)) {
		case 2:
		    switch (i) {
		    case 1:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[*,%d]")
		    case 2:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[%d,*]")
		    default:
			call error (1, "Error in section specification")
		    }
		    call pargi (ID_LINE(id,1))
		case 3:
		    switch (i) {
		    case 1:
		        call sprintf (Memc[ID_SECTION(id)],SZ_FNAME,"[*,%d,%d]")
		    case 2:
		        call sprintf (Memc[ID_SECTION(id)],SZ_FNAME,"[%d,*,%d]")
		    case 3:
		        call sprintf (Memc[ID_SECTION(id)],SZ_FNAME,"[%d,%d,*]")
		    }
		    call pargi (ID_LINE(id,1))
		    call pargi (ID_LINE(id,2))
		case 4:
		    call error (1, "Image dimension greater than 3 not allowed")
		}

		call sfree (sp)
	    }
	}

	# Parse the image section.
	x1[1] = 1; x2[1] = IM_LEN(im,1); xs[1] = 1
	x1[2] = 1; x2[2] = IM_LEN(im,2); xs[2] = 1
	x1[3] = 1; x2[3] = IM_LEN(im,3); xs[3] = 1
	call id_section (Memc[ID_SECTION(id)], x1, x2, xs, 3)

	# Set the axes.  The axis to be identified is the longest one.
	i = 1
	if (IM_NDIM(im) > 1 && abs (x1[2]-x2[2]) >= abs (x1[i]-x2[i]))
	    i = 2
	if (IM_NDIM(im) > 2 && abs (x1[3]-x2[3]) >= abs (x1[i]-x2[i]))
	    i = 3
	if (IM_NDIM(im) > 3)
	    call error (1, "Image dimension greater than 3 not allowed")

	switch (i) {
	case 1:
	    j = 2
	    k = 3
	case 2:
	    j = 1
	    k = 3
	case 3:
	    j = 1
	    k = 2
	}

	ID_LINE(id,1) = (x1[j] + x2[j]) / 2
	ID_LINE(id,2) = (x1[k] + x2[k]) / 2
	ID_MAXLINE(id,1) = IM_LEN(im, j)
	ID_MAXLINE(id,2) = IM_LEN(im, k)
	ID_NSUM(id,1) = min (ID_MAXLINE(id,1), ID_NSUM(id,1))
	ID_NSUM(id,2) = min (ID_MAXLINE(id,2), ID_NSUM(id,2))
	call smw_daxis (NULL, NULL, i, ID_NSUM(id,1), ID_NSUM(id,2))

	call id_maphdr (id, im)

	# Open the image READ_WRITE if possible in order to add REFSPEC.
	# This is not done earlier to avoid updating of the WCS.

	call imunmap (im)
	if (imaccess (Memc[ID_IMAGE(id)], READ_WRITE) == YES)
	    im = immap (Memc[ID_IMAGE(id)], READ_WRITE, 0)
	else
	    im = immap (Memc[ID_IMAGE(id)], READ_ONLY, 0)
	IM(ID_SH(id)) = im
end


# ID_MAPHDR -- Map image header.

procedure id_maphdr (id, im)

pointer	id				# ID pointer
pointer	im				# IMIO pointer

int	i
pointer	mw, sh, smw_openim(), smw_sctran()
errchk	smw_openim(), shdr_open(), smw_sctran

begin
	mw = smw_openim (im)
	if (SMW_TRANS(mw) == YES) {
	    if (SMW_PAXIS(mw,1) == 1)
		call smw_daxis (mw, im, 2, INDEFI, INDEFI)
	    else
		call smw_daxis (mw, im, 1, INDEFI, INDEFI)
	    call smw_saxes (mw, NULL, im)
	}
	call shdr_open (im, mw, ID_LINE(id,1), ID_LINE(id,2),
	    INDEFI, SHHDR, ID_SH(id))
	sh = ID_SH(id)

	if (SMW_FORMAT(mw) == SMW_MS || SMW_FORMAT(mw) == SMW_ES) {
	    ID_MAXLINE(id,1) = IM_LEN(im,2)
	    ID_MAXLINE(id,2) = IM_LEN(im,3)
	    ID_NSUM(id,1) = 1
	    ID_NSUM(id,2) = 1
	    ID_LINE(id,1) = max (1, min (ID_MAXLINE(id,1), ID_LINE(id,1)))
	    ID_LINE(id,2) = 1
	    call mfree (ID_APS(id), TY_INT)
	    call malloc (ID_APS(id), ID_MAXLINE(id,1), TY_INT)
	    do i = 1, ID_MAXLINE(id,1) {
		call shdr_open (im, mw, i, 1, INDEFI, SHHDR, sh)
		Memi[ID_APS(id)+i-1] = AP(sh)
	    }
	    ID_AP(id,1) = Memi[ID_APS(id)+ID_LINE(id,1)-1]
	    ID_AP(id,2) = 1
	} else {
	    call mfree (ID_APS(id), TY_INT)
	    ID_AP(id,1) = ID_LINE(id,1)
	    ID_AP(id,2) = ID_LINE(id,2)
	}
	ID_NPTS(id) = IM_LEN(im, SMW_LAXIS(mw,1))

	call gt_sets (ID_GT(id), GTXLABEL, LABEL(sh))
	call ic_pstr (ID_IC(id), "ylabel", LABEL(sh))
	call gt_sets (ID_GT(id), GTXUNITS, UNITS(sh))
	call ic_pstr (ID_IC(id), "yunits", UNITS(sh))

	# Set logical / physical transformations
	i = 2 ** (SMW_PAXIS(mw,1) - 1)
	ID_LP(id) = smw_sctran (mw, "logical", "physical", i)
	ID_PL(id) = smw_sctran (mw, "physical", "logical", i)
end


# ID_SECTION -- Parse an image section into its elements.
# 1. The default values must be set by the caller.
# 2. A null image section is OK.
# 3. The first nonwhitespace character must be '['.
# 4. The last interpreted character must be ']'.
#
# This procedure should be replaced with an IMIO procedure at some
# point.

procedure id_section (section, x1, x2, xs, ndim)

char	section[ARB]		# Image section
int	x1[ndim]		# Starting pixel
int	x2[ndim]		# Ending pixel
int	xs[ndim]		# Step
int	ndim			# Number of dimensions

int	i, ip, a, b, c, temp, ctoi()
define	error_	99

begin
	# Decode the section string.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1
	if (section[ip] == '[')
	    ip = ip + 1
	else if (section[ip] == EOS)
	    return
	else
	    goto error_

	do i = 1, ndim {
	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ']')
		break

	    # Default values
	    a = x1[i]
	    b = x2[i]
	    c = xs[i]

	    # Get a:b:c.  Allow notation such as "-*:c"
	    # (or even "-:c") where the step is obviously negative.

	    if (ctoi (section, ip, temp) > 0) {			# a
		a = temp
	        if (section[ip] == ':') {	
		    ip = ip + 1
		    if (ctoi (section, ip, b) == 0)		# a:b
		        goto error_
	        } else
		    b = a
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*')			# *
	        ip = ip + 1
	    if (section[ip] == ':') {				# ..:step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }
	    if (a > b && c > 0)
	        c = -c

	    x1[i] = a
	    x2[i] = b
	    xs[i] = c

	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ',')
		ip = ip + 1
	}

	if (section[ip] != ']')
	    goto error_

	return
error_
	call error (0, "Error in image section specification")
end
