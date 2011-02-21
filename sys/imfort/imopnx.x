# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<imhdr.h>
include	<mach.h>
include	"imfort.h"
include	"oif.h"

# IMOPNX -- Open an existing imagefile.  Only host system filenames are
# permitted, image sections are not permitted, no out of bounds references,
# and so on.

procedure imopnx (image, acmode, im, ier)

char	image[ARB]	#I HOST name of image header file
int	acmode		#I image access mode (RO, WO)
pointer	im		#O receives image descriptor pointer
int	ier		#O receives error status

pointer	sp, pix_fp, hdr_fp
pointer	pixfile, hdrfile, root, extn, envvar, valstr
int	len_hdrmem, len_ua, status, ip

pointer	bfopnx()
long	clktime()
int	imrdhdr(), sizeof(), stridxs(), ctoi()
errchk	calloc

begin
	call smark (sp)
	call salloc (hdrfile, SZ_FNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)
	call salloc (envvar, SZ_FNAME, TY_CHAR)
	call salloc (valstr, SZ_FNAME, TY_CHAR)
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

	# Determine the user area size.
	len_ua = -1
	call strpak ("min_lenuserarea", Memc[envvar], SZ_FNAME)
	call zgtenv (Memc[envvar], Memc[valstr], SZ_FNAME, status)
	if (status > 0) {
	    ip = 1
	    call strupk (Memc[valstr], Memc[valstr], SZ_FNAME)
	    if (ctoi (Memc[valstr], ip, len_ua) <= 0)
		len_ua = -1
	}
	if (len_ua < 0)
	    len_ua = LEN_USERAREA

	# Allocate image descriptor.
	len_hdrmem = LEN_IMHDR + (len_ua / SZ_MII_INT)
	call calloc (im, LEN_IMDES + len_hdrmem, TY_STRUCT)

	IM_ACMODE(im) = acmode

	# Read image header into descriptor.  Close the file after reading in
	# the header if we are opening the image read only.

	if (imrdhdr (hdr_fp, im, len_ua, TY_IMHDR) == ERR) {
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
