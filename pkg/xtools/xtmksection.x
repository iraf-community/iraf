# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# XT_MKSECTION -- Convert an generalized image section string to an IMIO
# section string.  The generalized image section string has one of the
# following formats:
#	1. An IMIO image section
#	2. [line|column] [#|middle|last]
#	3. [#|middle|last] [line|column]
# where # is a line or column number.  The strings may be abbreviated.
# This procedure will work for images of dimension greater than 2  provided
# that missing sections references default to 1.

define	SZ_WRD	10

procedure xt_mksection (image, secstr, section, sz_section)

char	image[ARB]		# Image
char	secstr[ARB]		# Image section string
char	section[sz_section]	# Returned image section string
int	sz_section		# Maximum size of image section string

char	wrd1[SZ_WRD], wrd2[SZ_WRD]
int	ndim, len1, len2, i, j, k
pointer	im

int	strdic(), ctoi()
pointer	immap()
errchk	immap()

begin
	im = immap (image, READ_ONLY, 0)
	ndim = IM_NDIM(im)
	len1 = IM_LEN(im, 1)
	len2 = IM_LEN(im, 2)
	call imunmap (im)

	switch (ndim) {
	case 1:
	    section[1] = EOS
	default:
	    if (len2 == 1) {
		section[1] = EOS
		return
	    }
 
	    if (secstr[1] == '[')
	        call strcpy (secstr, section, sz_section)
	    else {
		call sscan (secstr)
		call gargwrd (wrd1, SZ_WRD)
		i = strdic (wrd1, wrd1, SZ_WRD, "|column|line|middle|last|")
		call gargwrd (wrd2, SZ_WRD)
		j = strdic (wrd2, wrd2, SZ_WRD, "|column|line|middle|last|")

		if ((j == 1) || (j == 2)) {
		    k = i
		    i = j
		    j = k
		    call strcpy (wrd1, wrd2, SZ_WRD)
		}

		switch (i) {
		case 1:
		    switch (j) {
		    case 3:
		        call sprintf (section, sz_section, "[%d,*]")
		            call pargi ((len1 + 1) / 2)
		    case 4:
		        call sprintf (section, sz_section, "[%d,*]")
		            call pargi (len1)
		    default:
			i = 1
			if (ctoi (wrd2, i, len1) == 0)
			    call error (0, "Bad column number")
		        call sprintf (section, sz_section, "[%d,*]")
		            call pargi (len1)
		    }
		case 2:
		    switch (j) {
		    case 3:
		        call sprintf (section, sz_section, "[*,%d]")
		            call pargi ((len2 + 1) / 2)
		    case 4:
		        call sprintf (section, sz_section, "[*,%d]")
		            call pargi (len2)
		    default:
			i = 1
			if (ctoi (wrd2, i, len1) == 0)
			    call error (0, "Bad line number")
		        call sprintf (section, sz_section, "[*,%d]")
		            call pargi (len1)
		    }
		default:
		    call error (0,
	    "Unknown section specification - Possible non-unique abbreviation")
		}
	    }
	}
end


# XT_MKIMSEC -- Apply a generalized image section to an image.

procedure xt_mkimsec (image, secstr, imagesec, sz_fname)

char	image[ARB]			# Image name
char	secstr[ARB]			# Image section string
char	imagesec[sz_fname]		# Image with section
int	sz_fname			# Maximum size of image name

char	section[SZ_FNAME]
errchk	xt_mksection()

begin
	call xt_mksection (image, secstr, section, SZ_FNAME)
	call sprintf (imagesec, sz_fname, "%s%s")
	    call pargstr (image)
	    call pargstr (section)
end


# XT_MK1D -- In some applications a one dimensional image is expected.
# This procedure checks to see if the image is one dimensional.  If it is
# not then a section is added to the image name.  This procedure should
# not be used and xt_mkimsec should be used instead.

procedure xt_mk1d (image, secstr, sz_fname)

char	image[sz_fname]			# Image name
char	secstr[ARB]			# Image section string
int	sz_fname			# Maximum size of image name

char	section[SZ_FNAME]
errchk	xt_mksection()

begin
	call xt_mksection (image, secstr, section, SZ_FNAME)
	call strcat (section, image, sz_fname)
end
