# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<fio.h>
include	"imfort.h"
include	"oif.h"

# IMCREX -- Create a new image of the indicated size and pixel type.
# Both the header and pixel file are created at the same time.  For
# simplicity we put both files in the same directory.  The name of the
# pixel file is the same as that of the header file, but with the
# extension ".pix".

procedure imcrex (image, axlen, naxis, pixtype, ier)

char	image[ARB]		# HOST filename of image
int	axlen[IM_MAXDIM]	# receives axis lengths
int	naxis			# receives number of axes
int	pixtype			# receives pixel type
int	ier			# receives error status

long	pfsize, clktime, cputime
pointer	sp, hdrfile, pixfile, osfn, root, extn, im
int	fp, status, nbytes, nchars, i

pointer	bfopnx()
int	imwphdr(), fnldir(), imgdirx()
define	done_ 91
errchk	calloc

begin
	call smark (sp)
	call salloc (hdrfile, SZ_FNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Verify image size and datatype operands.

	ier = OK
	if (naxis < 1 || naxis > MAX_NAXIS)
	    ier = IE_NAXIS
	if (ier == OK)
	    do i = 1, naxis
		if (axlen[i] < 1)
		    ier = IE_AXLEN
	if (ier == OK)
	    if (pixtype != TY_SHORT && pixtype != TY_REAL)
		ier = IE_PIXTYPE

	if (ier != OK) {
	    call im_seterrop (ier, image)
	    goto done_
	}

	# Construct name of image header file.
	call imf_parse (image, Memc[root], Memc[extn])
	if (Memc[extn] == EOS)
	    call strcpy (OIF_EXTN, Memc[extn], SZ_FNAME)

	call strcpy (Memc[root], Memc[hdrfile], SZ_FNAME)
	call strcat (".", Memc[hdrfile], SZ_FNAME)
	call strcat (Memc[extn], Memc[hdrfile], SZ_FNAME)

	# Open the header file.
	call strpak (Memc[hdrfile], Memc[osfn], SZ_PATHNAME)
	call zopnbf (Memc[osfn], NEW_FILE, fp)
	if (fp == ERR) {
	    call sfree (sp)
	    ier = IE_CREHDR
	    call im_seterrop (ier, Memc[hdrfile])
	    return
	}

	# Allocate and initialize the image header.

	call calloc (im, LEN_IMDES + LEN_IMHDR, TY_STRUCT)
	call zgtime (clktime, cputime)

	call amovi (axlen, IM_LEN(im,1), naxis)
	IM_NDIM(im) = naxis
	IM_PIXTYPE(im) = pixtype
	IM_HDRLEN(im) = LEN_IMHDR
	IM_CTIME(im) = clktime
	IM_MTIME(im) = clktime
	call imf_initoffsets (im, SZ_DEVBLK)
	pfsize = IM_HGMOFF(im) - 1

	# Construct the pixel file name.  Initialize the remaining fields
	# of the image header.

	nchars = fnldir (Memc[root], Memc[pixfile], SZ_IMPIXFILE)
	if (nchars > 0)
	    call strcpy (Memc[root+nchars], Memc[root], SZ_IMPIXFILE)

	nchars = imgdirx (IM_PIXFILE(im), SZ_IMPIXFILE)
	call strcat (Memc[root], IM_PIXFILE(im), SZ_IMPIXFILE)
	call strcat (".", IM_PIXFILE(im), SZ_IMPIXFILE)
	call strcat ("pix", IM_PIXFILE(im), SZ_IMPIXFILE)

	call strcpy (Memc[hdrfile], IM_HDRFILE(im), SZ_IMHDRFILE)
	call imf_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im), Memc[pixfile],
	    SZ_PATHNAME)
	call strcpy ("imhdr", IM_MAGIC(im), SZ_IMMAGIC)

	# Write the image header and close the header file.  Do not use BFIO
	# to write the header, because we want the file to be odd sized.

	nbytes = IM_HDRLEN(im) * SZ_STRUCT * SZB_CHAR
	call zawrbf (fp, IM_MAGIC(im), nbytes, long(1))
	call zawtbf (fp, status)

	if (status != nbytes) {
	    call zclsbf (fp, status)
	    status = ERR
	} else
	    call zclsbf (fp, status)

	if (status == ERR) {
	    ier = IE_WRHDR
	    call im_seterrop (ier, Memc[hdrfile])
	    return
	}

	# Create the pixel storage file.
	call bfalcx (Memc[pixfile], pfsize, status)
	if (status == ERR) {
	    ier = IE_ALCPIX
	    call im_seterrop (ier, Memc[pixfile])
	    goto done_
	}

	# Write the backpointing pixel header into the pixel file.
	fp = bfopnx (Memc[pixfile], WRITE_ONLY, RANDOM)
	if (fp == ERR) {
	    status = ERR
	} else if (imwphdr (fp, im, image) == ERR) {
	    call bfclos (fp, status)
	    status = ERR
	} else
	    call bfclos (fp, status)

	call mfree (im, TY_STRUCT)
	if (status == ERR) {
	    ier = IE_ACCPIX
	    call im_seterrop (ier, Memc[pixfile])
	} else
	    ier = OK
done_
	call sfree (sp)
end
