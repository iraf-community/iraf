include	<ctype.h>
include	<imhdr.h>
include	<pkg/gtools.h>
include	"identify.h"
include	"../shdr.h"

# Sepcial section words.
define	SPECIAL "|column|line|middle|last|"
define	COLUMN	1
define	LINE	2
define	MIDDLE	3
define	LAST	4

# ID_MAP -- Map an image for IDENTIFY/REIDENTIFY
# The image must 1 or 2 dimensional.  An image section may be given with
# the image name or with the CL parameter section.  The CL parameter can
# have one of the following formats:
#	1. An IMIO image section
#	2. [line|column] [#|middle|last]
#	3. [#|middle|last] [line|column]
# where # is a line or column number.  The strings may be abbreviated.
# The task returns and error if it cannot map the image or determine
# the 1D line or column desired.  It returns the axis perpendicular to
# the one in which features are to be identified and the starting line
# or column.

procedure id_map (id)

pointer	id			# IDENTIFY data structure pointer

int	i, j, k, x1[2], x2[2], xs[2]
pointer	sp, wrd1, wrd2, im

int	imaccess(), strdic(), ctoi(), nscan()
pointer	immap()
errchk	immap, id_maphdr

begin
	# Separate the image name and image section and map the full image.
	call imgsection (Memc[ID_IMAGE(id)], Memc[ID_SECTION(id)], SZ_FNAME)
	call imgimage (Memc[ID_IMAGE(id)], Memc[ID_IMAGE(id)], SZ_FNAME)
	if (imaccess (Memc[ID_IMAGE(id)], READ_WRITE) == YES)
	    im = immap (Memc[ID_IMAGE(id)], READ_WRITE, TY_REAL)
	else
	    im = immap (Memc[ID_IMAGE(id)], READ_ONLY, TY_REAL)

	# Treat the case of 1D or 2D images.
	switch (min (2, IM_NDIM(im))) {
	case 1:
	    ID_LINE(id) = 1
	    ID_MAXLINE(id) = 1
	case 2:
	    x1[1] = 1
	    x2[1] = IM_LEN(im, 1)
	    xs[1] = 1
	    x1[2] = 1
	    x2[2] = IM_LEN(im, 2)
	    xs[2] = 1

	    # Get image section from CL parameter if needed.
	    if (Memc[ID_SECTION(id)] == EOS) {
		call clgstr ("section", Memc[ID_SECTION(id)], SZ_FNAME)
		call xt_stripwhite (Memc[ID_SECTION(id)])
	    }

	    # Convert special format to standard image section.
	    if (Memc[ID_SECTION(id)] != '[') {
	        call smark (sp)
	        call salloc (wrd1, SZ_FNAME, TY_CHAR)
	        call salloc (wrd2, SZ_FNAME, TY_CHAR)

		call sscan (Memc[ID_SECTION(id)])
		call gargwrd (Memc[wrd1], SZ_FNAME)
		if (nscan() > 0) {
		    i = strdic (Memc[wrd1], Memc[wrd1], SZ_FNAME, SPECIAL)
		    call gargwrd (Memc[wrd2], SZ_FNAME)
		    j = strdic (Memc[wrd2], Memc[wrd2], SZ_FNAME, SPECIAL)
		} else {
		    i = LINE
		    j = MIDDLE
		}

		if ((j == 1) || (j == 2)) {
		    k = i
		    i = j
		    j = k
		    call strcpy (Memc[wrd1], Memc[wrd2], SZ_FNAME)
		}

		call sfree (sp)

		switch (i) {
		case COLUMN:
		    switch (j) {
		    case MIDDLE:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[%d,*]")
		            call pargi ((x1[1] + x2[1]) / 2)
		    case LAST:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[%d,*]")
		            call pargi (x2[1])
		    default:
			i = 1
			if (ctoi (Memc[wrd2], i, j) == 0)
			    call error (1, "Bad column number")
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[%d,*]")
		            call pargi (j)
		    }
		case LINE:
		    switch (j) {
		    case MIDDLE:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[*,%d]")
		            call pargi ((x1[2] + x2[2]) / 2)
		    case LAST:
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[*,%d]")
		            call pargi (x2[2])
		    default:
			i = 1
			if (ctoi (Memc[wrd2], i, j) == 0)
			    call error (1, "Bad line number")
		        call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[*,%d]")
		            call pargi (j)
		    }
		default:
		    call imunmap (im)
		    call error (1,
	    "Unknown section specification - Possible non-unique abbreviation")
		}
	    }

	    # Parse the standard image section.
	    call id_section (Memc[ID_SECTION(id)], x1, x2, xs, 2)

	    # The axis to be identified is the longer one.
	    if (abs (x1[1] - x2[1]) >= abs (x1[2] - x2[2])) {
		call shdr_2d (NULL, 1, ID_NSUM(id))
		ID_LINE(id) = (x1[2] + x2[2]) / 2
		ID_MAXLINE(id) = IM_LEN(im, 2)
	    } else {
		call shdr_2d (NULL, 2, ID_NSUM(id))
		ID_LINE(id) = (x1[1] + x2[1]) / 2
		ID_MAXLINE(id) = IM_LEN(im, 1)
	    }
	default:
	    call imunmap (im)
	    call error (1, "Image dimensions greater than 2 are not supported")
	}

	ID_NSUM(id) = min (ID_MAXLINE(id), ID_NSUM(id))

	call id_maphdr (id, im)
end


# ID_MAPHDR -- Map image header.

procedure id_maphdr (id, im)

pointer	id				# ID pointer
pointer	im				# IMIO pointer

int	i
pointer	mw, sh, smw_openim(), mw_sctran()
errchk	smw_openim(), shdr_open(), mw_sctran

begin
	mw = smw_openim (im)
	call shdr_open (im, mw, ID_LINE(id), 1, INDEFI, SHHDR, ID_SH(id))
	sh = ID_SH(id)

	if (FORMAT(sh) == MULTISPEC) {
	    ID_MAXLINE(id) = IM_LEN(im,2)
	    ID_NSUM(id) = 1
	    ID_LINE(id) = max (1, min (ID_MAXLINE(id), ID_LINE(id)))
	    call mfree (ID_APS(id), TY_INT)
	    call malloc (ID_APS(id), ID_MAXLINE(id), TY_INT)
	    do i = 1, ID_MAXLINE(id) {
		call shdr_open (im, mw, i, 1, INDEFI, SHHDR, sh)
		Memi[ID_APS(id)+i-1] = AP(sh)
	    }
	    ID_AP(id) = Memi[ID_APS(id)+ID_LINE(id)-1]
	} else {
	    #ID_AP(id) = 0
	    ID_AP(id) = ID_LINE(id)
	    call mfree (ID_APS(id), TY_INT)
	    ID_NPTS(id) = IM_LEN(im, DAXIS(sh))
	}

	call gt_sets (ID_GT(id), GTXLABEL, LABEL(sh))
	call ic_pstr (ID_IC(id), "ylabel", LABEL(sh))
	call gt_sets (ID_GT(id), GTXUNITS, UNITS(sh))
	call ic_pstr (ID_IC(id), "yunits", UNITS(sh))

	# Set logical / physical transformations
	ID_LP(id) = mw_sctran (mw, "logical", "physical", DAXISP(sh))
	ID_PL(id) = mw_sctran (mw, "physical", "logical", DAXISP(sh))
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
