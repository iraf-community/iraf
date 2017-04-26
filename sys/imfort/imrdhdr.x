# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include "imfort.h"
include "imhv1.h"
include "imhv2.h"
include "oif.h"


# IMRDHDR -- Read the image header.  Either the main image header or the
# pixel file header can be read.

int procedure imrdhdr (fp, im, uchars, htype)

pointer	fp			#I header file descriptor
pointer	im			#I image descriptor
int	uchars			#I maxchars of user area data to read
int	htype			#I TY_IMHDR or TY_PIXHDR

pointer	sp, v1
char	immagic[SZ_IMMAGIC]
int	sulen_userarea, hdrlen, nchars, status

bool	streq()
int	i_miirdc(), i_miirdi(), i_miirdl(), i_miirdr()
int	btoi(), bfrseq(), bfseek()

define	readerr_ 91

begin
	# Determine the file type.
	if (bfseek (fp, BOFL) == ERR)
	    return (ERR)
	if (bfrseq (fp, immagic, SZ_IMMAGIC) != SZ_IMMAGIC)
	    return (ERR)

	if (htype == TY_PIXHDR && streq (immagic, V1_PMAGIC)) {
	    # V1 Pixel file header.
	    return (OK)

	} else if (htype == TY_IMHDR && streq (immagic, V1_MAGIC)) {
	    # Old V1 image header.

	    call smark (sp)
	    call salloc (v1, LEN_V1IMHDR, TY_STRUCT)

	    if (bfseek (fp, BOFL) == ERR) {
		call sfree (sp)
		return (ERR)
	    }
	    nchars = LEN_V1IMHDR * SZ_MII_INT
	    if (bfrseq (fp, IM_V1MAGIC(v1), nchars) != nchars) {
		call sfree (sp)
		return (ERR)
	    }

	    # Initialize the output image header.
	    call strcpy (IMH_MAGICSTR, IM_MAGIC(im), SZ_IMMAGIC)
	    IM_HDRVER(im) = V1_VERSION

	    # The following is the length of the user area in SU.
	    sulen_userarea = IM_V1HDRLEN(v1) - LEN_V1IMHDR
	    IM_HDRLEN(im) = LEN_IMHDR + sulen_userarea

	    IM_SWAP(im) = NO
	    IM_SWAPPED(im) = -1
	    IM_PIXTYPE(im) = IM_V1PIXTYPE(v1)

	    IM_NDIM(im) = IM_V1NDIM(v1)
	    call amovl (IM_V1LEN(v1,1), IM_LEN(im,1), IM_MAXDIM)
	    call amovl (IM_V1PHYSLEN(v1,1), IM_PHYSLEN(im,1), IM_MAXDIM)

	    IM_SSMTYPE(im) = IM_V1SSMTYPE(v1)
	    IM_LUTOFF(im) = IM_V1LUTOFF(v1)
	    IM_PIXOFF(im) = IM_V1PIXOFF(v1)
	    IM_HGMOFF(im) = IM_V1HGMOFF(v1)
	    IM_CTIME(im) = IM_V1CTIME(v1)
	    IM_MTIME(im) = IM_V1MTIME(v1)
	    IM_LIMTIME(im) = IM_V1LIMTIME(v1)
	    IM_MAX(im) = IM_V1MAX(v1)
	    IM_MIN(im) = IM_V1MIN(v1)

	    call strcpy (IM_V1PIXFILE(v1), IM_PIXFILE(im), SZ_IMPIXFILE)
	    call strcpy (IM_V1HDRFILE(v1), IM_HDRFILE(im), SZ_IMHDRFILE)
	    call strcpy (IM_V1TITLE(v1), IM_TITLE(im), SZ_IMTITLE)
	    call strcpy (IM_V1HISTORY(v1), IM_HISTORY(im), SZ_IMHIST)

	    # Read and output the user area.
	    if (uchars > 0) {
		nchars = min (uchars, sulen_userarea * SZ_MII_INT)
		if (bfrseq (fp, Memc[IM_USERAREA(im)], nchars) <= 0)
		    return (ERR)
	    }

	    call sfree (sp)
	    return (OK)
	}

	# Check for a new format header.
	if (bfseek (fp, BOFL) == ERR)
	    return (ERR)
	if (i_miirdc (fp, immagic, SZ_IMMAGIC) < 0)
	    return (ERR)

	if (htype == TY_PIXHDR && streq (immagic, V2_PMAGIC)) {
	    # V2 Pixel file header.
	    return (OK)

	} else if (htype == TY_IMHDR && streq (immagic, V2_MAGIC)) {
	    # Newer V2 image header.
	    status = ERR

	    # Initialize the output image header.
	    call strcpy (IMH_MAGICSTR, IM_MAGIC(im), SZ_IMMAGIC)
	    IM_HDRVER(im) = V2_VERSION

	    # "sulen_userarea" is the length of the user area in SU.
	    if (i_miirdi (fp, hdrlen, 1) != 1)
		goto readerr_
	    sulen_userarea = hdrlen - LEN_V2IMHDR
	    IM_HDRLEN(im) = LEN_IMHDR + sulen_userarea

	    if (i_miirdi (fp, IM_PIXTYPE(im), 1) != 1)
		goto readerr_

	    # Determine whether to byte swap the pixels.
	    if (i_miirdi (fp, IM_SWAPPED(im), 1) != 1)
		goto readerr_

	    IM_SWAP(im) = NO
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT:
		IM_SWAP(im) = btoi (IM_SWAPPED(im) != BYTE_SWAP2)
	    case TY_INT, TY_LONG:
		IM_SWAP(im) = btoi (IM_SWAPPED(im) != BYTE_SWAP4)
	    case TY_REAL:
		if (IEEE_USED == YES)
		    IM_SWAP(im) = btoi (IM_SWAPPED(im) != IEEE_SWAP4)
	    case TY_DOUBLE:
		if (IEEE_USED == YES)
		    IM_SWAP(im) = btoi (IM_SWAPPED(im) != IEEE_SWAP8)
	    }

	    # Read the fixed-format fields of the header.
	    if (i_miirdi (fp, IM_NDIM(im), 1) < 0)
		goto readerr_
	    if (i_miirdi (fp, IM_LEN(im,1), IM_MAXDIM) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_PHYSLEN(im,1), IM_MAXDIM) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_SSMTYPE(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_LUTOFF(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_PIXOFF(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_HGMOFF(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_BLIST(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_SZBLIST(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_NBPIX(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_CTIME(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_MTIME(im), 1) < 0)
		goto readerr_
	    if (i_miirdl (fp, IM_LIMTIME(im), 1) < 0)
		goto readerr_

	    if (i_miirdr (fp, IM_MAX(im), 1) < 0)
		goto readerr_
	    if (i_miirdr (fp, IM_MIN(im), 1) < 0)
		goto readerr_

	    if (i_miirdc (fp, IM_PIXFILE(im), SZ_V2IMPIXFILE) < 0)
		goto readerr_
	    if (i_miirdc (fp, IM_HDRFILE(im), SZ_V2IMHDRFILE) < 0)
		goto readerr_
	    if (i_miirdc (fp, IM_TITLE(im), SZ_V2IMTITLE) < 0)
		goto readerr_
	    if (i_miirdc (fp, IM_HISTORY(im), SZ_V2IMHIST) < 0)
		goto readerr_

	    # Read the variable-length user area.
	    if (uchars > 0) {
		nchars = min (uchars, sulen_userarea * SZ_MII_INT)
		if (i_miirdc (fp, Memc[IM_USERAREA(im)], nchars) < 0)
		    goto readerr_
	    }

	    status = OK
readerr_
	    return (status)
	}

	return (ERR)
end
