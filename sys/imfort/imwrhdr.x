# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include "imfort.h"
include "imhv1.h"
include "imhv2.h"
include "oif.h"

# IMWRHDR -- Write an OIF image header.

int procedure imwrhdr (fp, im, htype)

pointer	fp			#I header file descriptor
pointer	im			#I image descriptor
int	htype			#I TY_IMHDR or TY_PIXHDR

pointer	sp, v1, fname
int	status, hdrlen, len_userarea
int	bfseek(), bfwseq(), i_miiwrc(), i_miiwri(), i_miiwrl(), i_miiwrr()
int	strlen()

define	v1done_  91
define	v2start_ 92
define	v2done_  93

begin
	switch (IM_HDRVER(im)) {
	case V1_VERSION:
	    # Old V1 image header.
	    # ----------------------

	    status = ERR
	    call smark (sp)
	    call salloc (v1, LEN_V1IMHDR, TY_STRUCT)

	    # Initialize the output image header.
	    switch (htype) {
	    case TY_IMHDR:
		call strcpy (V1_MAGIC, IM_V1MAGIC(v1), SZ_IMMAGIC)
		hdrlen = LEN_V1IMHDR
	    case TY_PIXHDR:
		call strcpy (V1_PMAGIC, IM_V1MAGIC(v1), SZ_IMMAGIC)
		hdrlen = LEN_V1PIXHDR
	    default:
		goto v1done_
	    }

	    # The following is the length of the user area in chars.
	    len_userarea = strlen (Memc[IM_USERAREA(im)]) + 1
	    IM_V1HDRLEN(v1) = LEN_V1IMHDR +
		(len_userarea + SZ_MII_INT-1) / SZ_MII_INT

	    IM_V1PIXTYPE(v1) = IM_PIXTYPE(im)
	    IM_V1NDIM(v1) = IM_NDIM(im)
	    call amovl (IM_LEN(im,1), IM_V1LEN(v1,1), IM_MAXDIM)
	    call amovl (IM_PHYSLEN(im,1), IM_V1PHYSLEN(v1,1), IM_MAXDIM)

	    IM_V1SSMTYPE(v1) = IM_SSMTYPE(im)
	    IM_V1LUTOFF(v1) = IM_LUTOFF(im)
	    IM_V1PIXOFF(v1) = IM_PIXOFF(im)
	    IM_V1HGMOFF(v1) = IM_HGMOFF(im)
	    IM_V1CTIME(v1) = IM_CTIME(im)
	    IM_V1MTIME(v1) = IM_MTIME(im)
	    IM_V1LIMTIME(v1) = IM_LIMTIME(im)
	    IM_V1MAX(v1) = IM_MAX(im)
	    IM_V1MIN(v1) = IM_MIN(im)

	    if (strlen(IM_PIXFILE(im)) > SZ_V1IMPIXFILE)
		goto v1done_
	    if (strlen(IM_HDRFILE(im)) > SZ_V1IMHDRFILE)
		goto v1done_

	    call strcpy (IM_PIXFILE(im), IM_V1PIXFILE(v1), SZ_V1IMPIXFILE)
	    call strcpy (IM_HDRFILE(im), IM_V1HDRFILE(v1), SZ_V1IMHDRFILE)
	    call strcpy (IM_TITLE(im), IM_V1TITLE(v1), SZ_V1IMTITLE)
	    call strcpy (IM_HISTORY(im), IM_V1HISTORY(v1), SZ_V1IMHIST)

	    # For historical reasons the pixel file header stores the host
	    # pathname of the header file in the PIXFILE field of the pixel
	    # file header.

	    if (htype == TY_PIXHDR)
		call fpathname (IM_HDRFILE(im), IM_V1PIXFILE(v1),
		    SZ_V1IMPIXFILE)

	    # Write the file header.
	    if (bfseek (fp, BOFL) == ERR)
		goto v1done_
	    if (bfwseq (fp, IM_V1MAGIC(v1), hdrlen * SZ_MII_INT) == ERR)
		goto v1done_

	    # Write the user area.
	    if (htype == TY_IMHDR)
		if (bfwseq (fp, Memc[IM_USERAREA(im)], len_userarea) == ERR)
		    goto v1done_

	    status = OK
v1done_
	    call sfree (sp)

	case V2_VERSION:
	    # Newer V2 image header.
	    # ----------------------
v2start_
	    status = ERR
	    call smark (sp)
	    call salloc (fname, SZ_PATHNAME, TY_CHAR)

	    if (bfseek (fp, BOFL) == ERR)
		goto v2done_

	    # Initialize the output image header.
	    switch (htype) {
	    case TY_IMHDR:
		if (i_miiwrc (fp, V2_MAGIC, SZ_IMMAGIC) == ERR)
		    goto v2done_
		hdrlen = LEN_V2IMHDR
	    case TY_PIXHDR:
		if (i_miiwrc (fp, V2_PMAGIC, SZ_IMMAGIC) == ERR)
		    goto v2done_
		hdrlen = LEN_V2PIXHDR
	    default:
		goto v2done_
	    }

	    # The following is the length of the user area in SU.
	    len_userarea = strlen (Memc[IM_USERAREA(im)]) + 1
	    hdrlen = LEN_V2IMHDR + (len_userarea + SZ_MII_INT-1) / SZ_MII_INT

	    if (i_miiwri (fp, hdrlen, 1) == ERR)
		goto v2done_
	    if (i_miiwri (fp, IM_PIXTYPE(im), 1) == ERR)
		goto v2done_

	    # Record the byte swapping used for this image.  When writing a
	    # new image we use the native data type of the host and don't
	    # swap bytes, so IM_SWAPPED is YES if the host architecture is
	    # byte swapped.

	    switch (IM_ACMODE(im)) {
	    case NEW_IMAGE, NEW_COPY, TEMP_FILE:
		IM_SWAPPED(im) = -1
		switch (IM_PIXTYPE(im)) {
		case TY_SHORT, TY_USHORT:
		    IM_SWAPPED(im) = BYTE_SWAP2
		case TY_INT, TY_LONG:
		    IM_SWAPPED(im) = BYTE_SWAP4
		case TY_REAL:
		    if (IEEE_USED == YES)
			IM_SWAPPED(im) = IEEE_SWAP4
		case TY_DOUBLE:
		    if (IEEE_USED == YES)
			IM_SWAPPED(im) = IEEE_SWAP8
		}
	    default:
		# IM_SWAPPED should already be set in header.
	    }

	    if (i_miiwri (fp, IM_SWAPPED(im), 1) == ERR)
		goto v2done_
	    if (i_miiwri (fp, IM_NDIM(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_LEN(im,1), IM_MAXDIM) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_PHYSLEN(im,1), IM_MAXDIM) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_SSMTYPE(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_LUTOFF(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_PIXOFF(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_HGMOFF(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_BLIST(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_SZBLIST(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_NBPIX(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_CTIME(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_MTIME(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrl (fp, IM_LIMTIME(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrr (fp, IM_MAX(im), 1) == ERR)
		goto v2done_
	    if (i_miiwrr (fp, IM_MIN(im), 1) == ERR)
		goto v2done_

	    if (strlen(IM_PIXFILE(im)) > SZ_V2IMPIXFILE)
		goto v2done_
	    if (strlen(IM_HDRFILE(im)) > SZ_V2IMHDRFILE)
		goto v2done_

	    # For historical reasons the pixel file header stores the host
	    # pathname of the header file in the PIXFILE field of the pixel
	    # file header.

	    if (htype == TY_PIXHDR) {
		call aclrc (Memc[fname], SZ_PATHNAME)
		call fpathname (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME)
		if (i_miiwrc (fp, Memc[fname], SZ_V2IMPIXFILE) == ERR)
		    goto v2done_
		status = OK
		goto v2done_
	    } else if (i_miiwrc (fp, IM_PIXFILE(im), SZ_V2IMPIXFILE) == ERR)
		goto v2done_

	    call oif_trim (IM_HDRFILE(im), SZ_V2IMHDRFILE)
	    if (i_miiwrc (fp, IM_HDRFILE(im), SZ_V2IMHDRFILE) == ERR)
		goto v2done_

	    call oif_trim (IM_TITLE(im), SZ_V2IMTITLE)
	    if (i_miiwrc (fp, IM_TITLE(im), SZ_V2IMTITLE) == ERR)
		goto v2done_

	    call oif_trim (IM_HISTORY(im), SZ_V2IMHIST)
	    if (i_miiwrc (fp, IM_HISTORY(im), SZ_V2IMHIST) == ERR)
		goto v2done_

	    # Write the variable-length user area.
	    if (i_miiwrc (fp, Memc[IM_USERAREA(im)], len_userarea) == ERR)
		goto v2done_

	    status = OK
v2done_	
	    call sfree (sp)

	default:
	    IM_HDRVER(im) = V2_VERSION
	    goto v2start_
	}

	return (status)
end


# OIF_TRIM -- Trim trailing garbage at the end of a string.  This does not
# affect the value of the string, but makes the contents of the output file
# clearer when examined with file utilities.

procedure oif_trim (s, nchars)

char	s[ARB]
int	nchars

int	n
int	strlen()

begin
	n = strlen(s) + 1
	call aclrc (s[n], nchars - n)
end
