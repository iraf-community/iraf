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

char	image[ARB]		#I HOST filename of image
int	axlen[IM_MAXDIM]	#I axis lengths
int	naxis			#I number of axes
int	pixtype			#I pixel type
int	ier			#O receives error status

int	fp, status, ip, i
long	pfsize, clktime, cputime
pointer	sp, hdrfile, pixfile, osfn, root, extn, sval, im

pointer	bfopnx()
int	imwrhdr(), ctoi()
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

	# Construct the name of the image header file.
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
	fp = bfopnx (Memc[hdrfile], NF, RANDOM)
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
	IM_ACMODE(im) = NEW_IMAGE
	IM_NDIM(im) = naxis
	IM_PIXTYPE(im) = pixtype
	IM_HDRLEN(im) = LEN_IMHDR
	IM_CTIME(im) = clktime
	IM_MTIME(im) = clktime
	Memc[IM_USERAREA(im)] = EOS
	call imf_initoffsets (im, SZ_DEVBLK)
	pfsize = IM_HGMOFF(im) - 1

	# Get the image format version for new images.
	call strpak (ENV_OIFVER, Memc[sval], SZ_FNAME)
	call zgtenv (Memc[sval], Memc[sval], SZ_FNAME, status)
	if (status != ERR) {
	    ip = 1
	    call strupk (Memc[sval], Memc[sval], SZ_FNAME)
	    if (ctoi (Memc[sval], ip, IM_HDRVER(im)) <= 0)
		IM_HDRVER(im) = DEF_VERSION
	} else
	    IM_HDRVER(im) = DEF_VERSION

	# Get a unique pixel file name.
	call aclrc (IM_HDRFILE(im), SZ_IMHDRFILE)
	call strcpy (Memc[hdrfile], IM_HDRFILE(im), SZ_IMHDRFILE)
	call imf_mkpixfname (im, Memc[pixfile], SZ_IMPIXFILE, ier)
	if (ier != OK)
	    goto done_

	# Write the image header and close the header file.
	if (imwrhdr (fp, im, TY_IMHDR) == ERR) {
	    call bfclos (fp, status)
	    status = ERR
	} else
	    call bfclos (fp, status)

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
	fp = bfopnx (Memc[pixfile], WO, RANDOM)
	if (fp == ERR) {
	    status = ERR
	} else if (imwrhdr (fp, im, TY_PIXHDR) == ERR) {
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
