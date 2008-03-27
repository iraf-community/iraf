# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <imhdr.h>
include <mach.h>
include <imio.h>
include "imhv1.h"
include "imhv2.h"
include "oif.h"

# OIF_WRHDR -- Write an OIF image header.

procedure oif_wrhdr (fd, im, htype)

int	fd			#I header file descriptor
pointer	im			#I image descriptor
int	htype			#I TY_IMHDR or TY_PIXHDR

size_t	sz_val
long	lval
size_t	c_1
pointer	sp, v1, fname
int	status, hdrlen, len_userarea
errchk	write, miiwritec, miiwritei, miiwritel32, miiwriter
int	strlen()

define	v1done_  91
define	v2start_ 92
define	v2done_  93

begin
	c_1 = 1
	switch (IM_HDRVER(im)) {
	case V1_VERSION:
	    # Old V1 image header.
	    # ----------------------

	    status = ERR
	    call smark (sp)
	    sz_val = LEN_V1IMHDR
	    call salloc (v1, sz_val, TY_STRUCT)

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
		(len_userarea + SZ_STRUCT-1) / SZ_STRUCT

	    IM_V1PIXTYPE(v1) = IM_PIXTYPE(im)
	    IM_V1NDIM(v1) = IM_NDIM(im)
	    sz_val = IM_MAXDIM
	    call amovl (IM_LEN(im,1), IM_V1LEN(v1,1), sz_val)
	    call amovl (IM_PHYSLEN(im,1), IM_V1PHYSLEN(v1,1), sz_val)

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
	    lval = BOFL
	    call seek (fd, lval)
	    sz_val = hdrlen * SZ_STRUCT
	    call write (fd, IM_V1MAGIC(v1), sz_val)

	    # Write the user area.
	    if (htype == TY_IMHDR) {
		sz_val = len_userarea
		call write (fd, Memc[IM_USERAREA(im)], sz_val)
	    }

	    status = OK
v1done_
	    call sfree (sp)
	    if (status != OK)
		call syserrs (SYS_IKIUPDHDR, IM_NAME(im))

	case V2_VERSION:
	    # Newer V2 image header.
	    # ----------------------
v2start_
	    status = ERR
	    call smark (sp)
	    sz_val = SZ_PATHNAME
	    call salloc (fname, sz_val, TY_CHAR)

	    lval = BOFL
	    call seek (fd, lval)

	    # Initialize the output image header.
	    switch (htype) {
	    case TY_IMHDR:
		sz_val = SZ_IMMAGIC
		call miiwritec (fd, V2_MAGIC, sz_val)
		hdrlen = LEN_V2IMHDR
	    case TY_PIXHDR:
		sz_val = SZ_IMMAGIC
		call miiwritec (fd, V2_PMAGIC, sz_val)
		hdrlen = LEN_V2PIXHDR
	    default:
		goto v2done_
	    }

	    # The following is the length of the user area in SU.
	    len_userarea = strlen (Memc[IM_USERAREA(im)]) + 1
	    hdrlen = LEN_V2IMHDR + (len_userarea + SZ_STRUCT-1) / SZ_STRUCT

	    call miiwritei (fd, hdrlen, c_1)
	    call miiwritei (fd, IM_PIXTYPE(im), c_1)

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

	    call miiwritei (fd, IM_SWAPPED(im), c_1)
	    call miiwritei (fd, IM_NDIM(im), c_1)
	    sz_val = IM_MAXDIM
	    call miiwritel32 (fd, IM_LEN(im,1), sz_val)
	    call miiwritel32 (fd, IM_PHYSLEN(im,1), sz_val)
	    call miiwritel32 (fd, IM_SSMTYPE(im), c_1)
	    call miiwritel32 (fd, IM_LUTOFF(im), c_1)
	    call miiwritel32 (fd, IM_PIXOFF(im), c_1)
	    call miiwritel32 (fd, IM_HGMOFF(im), c_1)
	    call miiwritel32 (fd, IM_BLIST(im), c_1)
	    call miiwritel32 (fd, IM_SZBLIST(im), c_1)
	    call miiwritel32 (fd, IM_NBPIX(im), c_1)
	    call miiwritel32 (fd, IM_CTIME(im), c_1)
	    call miiwritel32 (fd, IM_MTIME(im), c_1)
	    call miiwritel32 (fd, IM_LIMTIME(im), c_1)
	    call miiwriter (fd, IM_MAX(im), c_1)
	    call miiwriter (fd, IM_MIN(im), c_1)

	    if (strlen(IM_PIXFILE(im)) > SZ_V2IMPIXFILE)
		goto v2done_
	    if (strlen(IM_HDRFILE(im)) > SZ_V2IMHDRFILE)
		goto v2done_

	    # For historical reasons the pixel file header stores the host
	    # pathname of the header file in the PIXFILE field of the pixel
	    # file header.

	    if (htype == TY_PIXHDR) {
		sz_val = SZ_PATHNAME
		call aclrc (Memc[fname], sz_val)
		call fpathname (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME)
		sz_val = SZ_V2IMPIXFILE
		call miiwritec (fd, Memc[fname], sz_val)
		status = OK
		goto v2done_
	    } else {
		sz_val = SZ_V2IMPIXFILE
		call miiwritec (fd, IM_PIXFILE(im), sz_val)
	    }

	    sz_val = SZ_V2IMHDRFILE
	    call oif_trim (IM_HDRFILE(im), SZ_V2IMHDRFILE)
	    call miiwritec (fd, IM_HDRFILE(im), sz_val)

	    sz_val = SZ_V2IMTITLE
	    call oif_trim (IM_TITLE(im), SZ_V2IMTITLE)
	    call miiwritec (fd, IM_TITLE(im), sz_val)

	    sz_val = SZ_V2IMHIST
	    call oif_trim (IM_HISTORY(im), SZ_V2IMHIST)
	    call miiwritec (fd, IM_HISTORY(im), sz_val)

	    # Write the variable-length user area.
	    sz_val = len_userarea
	    call miiwritec (fd, Memc[IM_USERAREA(im)], sz_val)

	    status = OK
v2done_	
	    call sfree (sp)
	    if (status != OK)
		call syserrs (SYS_IKIUPDHDR, IM_NAME(im))

	default:
	    IM_HDRVER(im) = V2_VERSION
	    goto v2start_
	}
end


# OIF_TRIM -- Trim trailing garbage at the end of a string.  This does not
# affect the value of the string, but makes the contents of the output file
# clearer when examined with file utilities.

procedure oif_trim (s, nchars)

char	s[ARB]
int	nchars

size_t	sz_val
int	n, ntrim
int	strlen()

begin
	n = strlen(s) + 1
	ntrim = nchars - n

	if (ntrim > 0) {
	    sz_val = ntrim
	    call aclrc (s[n], sz_val)
	}
end
