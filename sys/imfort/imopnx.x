# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<imhdr.h>
include	<mach.h>
include	"imfort.h"

# IMOPNX -- Open an existing imagefile.  Only host system filenames are
# permitted, image sections are not permitted, no out of bounds references,
# and so on.

procedure imopnx (image, acmode, im, ier)

char	image[ARB]	# HOST name of image header file
int	acmode		# image access mode (RO, WO)
pointer	im		# receives image descriptor pointer
int	ier		# receives error status

pointer	sp, pix_fp, hdr_fp, pixfile, hdrfile, root, extn
int	len_hdrmem, status, nchars, n, ip

bool	strne()
pointer	bfopnx()
long	clktime()
int	bfread(), sizeof(), stridxs()
errchk	calloc

begin
	call smark (sp)
	call salloc (hdrfile, SZ_FNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Construct name of image header file.
	call imf_parse (image, Memc[root], Memc[extn])
	if (Memc[extn] == EOS)
	    call strcpy ("imh", Memc[extn], SZ_FNAME)

	call strcpy (Memc[root], Memc[hdrfile], SZ_FNAME)
	call strcat (".", Memc[hdrfile], SZ_FNAME)
	call strcat (Memc[extn], Memc[hdrfile], SZ_FNAME)

	# Open image header file.
	hdr_fp = bfopnx (Memc[hdrfile], acmode, RANDOM)
	if (hdr_fp == ERR) {
	    call sfree (sp)
	    ier = IE_OPEN
	    call im_seterrop (ier, Memc[hdrfile])
	    return
	}

	# Allocate image descriptor.
	len_hdrmem = LEN_IMHDR + LEN_USERAREA
	call calloc (im, LEN_IMDES + len_hdrmem, TY_STRUCT)

	# Read image header into descriptor.  Close the file after reading in
	# the header if we are opening the image read only.

	nchars = len_hdrmem * SZ_STRUCT
	n = bfread (hdr_fp, IM_MAGIC(im), nchars, long(1))

	if (n < (LEN_IMHDR * SZ_STRUCT) || strne (IM_MAGIC(im), "imhdr")) {
	    call bfclos (hdr_fp, status)
	    call mfree (im, TY_STRUCT)
	    call sfree (sp)
	    ier = IE_NOTIMH
	    call im_seterrop (ier, Memc[hdrfile])
	    return
	} else if (acmode == RO) {
	    call bfclos (hdr_fp, status)
	    hdr_fp = NULL
	}

	# Get the name of the pixel storage file from the image header,
	# strip any node name prefix, and open the file.  Quit if the
	# file cannot be opened.

	call strcpy (Memc[hdrfile], IM_HDRFILE(im), SZ_IMHDRFILE)
	call imf_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im), Memc[pixfile],
	    SZ_PATHNAME)
	ip = pixfile + stridxs ("!", Memc[pixfile])
	pix_fp = bfopnx (Memc[ip], acmode, SEQUENTIAL)

	if (pix_fp == ERR) {
	    call mfree (im, TY_STRUCT)
	    call sfree (sp)
	    ier = IE_OPNPIX
	    call im_seterrop (ier, Memc[ip])
	    return
	}

	# Initialize the runtime image descriptor and return.

	IM_HDRFP(im)     = hdr_fp
	IM_PIXFP(im)     = pix_fp
	IM_LINESIZE(im)  = IM_PHYSLEN(im,1) * sizeof (IM_PIXTYPE(im))
	IM_SZPIXEL(im)   = sizeof (IM_PIXTYPE(im))
	IM_LENHDRMEM(im) = len_hdrmem
	IM_LINEBUFP(im)  = NULL
	IM_UABLOCKED(im) = -1

	# If opening the image with write permission, assume that the image
	# data will be modified (invalidating datamin/datamax).

	if (acmode != RO)
	    IM_MTIME(im) = clktime (long(0))

	ier = OK
	call sfree (sp)
end
