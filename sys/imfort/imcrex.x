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
pointer	sp, hdrfile, pixfile, osfn, root, extn, sval, im
int	fp, status, nbytes, i

pointer	bfopnx()
int	imwphdr()
define	done_ 91
define	operr_ 92
errchk	calloc

begin
	call smark (sp)
	call salloc (hdrfile, SZ_FNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (sval, SZ_FNAME, TY_CHAR)

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
	    call strcpy (OIF_HDREXTN, Memc[extn], SZ_FNAME)

	call strcpy (Memc[root], Memc[hdrfile], SZ_FNAME)
	call strcat (".", Memc[hdrfile], SZ_FNAME)
	call strcat (Memc[extn], Memc[hdrfile], SZ_FNAME)

	# Check to see if the new image would overwrite an existing one.
	# This is an error, unless "clobber" is defined in the user
	# environment.

	call strpak (Memc[hdrfile], Memc[osfn], SZ_PATHNAME)
	call zfacss (Memc[osfn], 0, 0, status)
	if (status == YES) {
	    call strpak ("clobber", Memc[sval], SZ_FNAME)
	    call zgtenv (Memc[sval], Memc[sval], SZ_FNAME, status)
	    if (status != ERR) {
		call imdelx (image, ier)
		if (ier != OK) {
		    ier = IE_CREHDR
		    goto operr_
		}
	    } else {
		ier = IE_CLOBBER
		goto operr_
	    }
	}

	# Create the new image.
	call zopnbf (Memc[osfn], NEW_FILE, fp)
	if (fp == ERR) {
	    ier = IE_CREHDR
operr_	    call sfree (sp)
	    call im_seterrop (ier, Memc[hdrfile])
	    return
	}

	# Allocate and initialize the image header.
	call calloc (im, LEN_IMDES + LEN_IMHDR, TY_STRUCT)
	call zgtime (clktime, cputime)

	call strcpy ("imhdr", IM_MAGIC(im), SZ_IMMAGIC)
	call amovi (axlen, IM_LEN(im,1), naxis)
	IM_NDIM(im) = naxis
	IM_PIXTYPE(im) = pixtype
	IM_HDRLEN(im) = LEN_IMHDR
	IM_CTIME(im) = clktime
	IM_MTIME(im) = clktime
	call imf_initoffsets (im, SZ_DEVBLK)
	pfsize = IM_HGMOFF(im) - 1

	# Get a unique pixel file name.
	call strcpy (Memc[hdrfile], IM_HDRFILE(im), SZ_IMHDRFILE)
	call imf_mkpixfname (im, Memc[pixfile], SZ_IMPIXFILE, ier)
	if (ier != OK)
	    goto done_

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
