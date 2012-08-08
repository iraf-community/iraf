# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"stf.h"

# STF_RDHEADER -- Read the STF format image header for a single group into the
# IMIO descriptor.  The standard fields are processed into the fields of the
# descriptor.  The GPB binary parameters are encoded as FITS cards and placed
# in the IMIO user area, followed by all extra cards in the FITS format STF
# group header.  Note that no distinction is made between the common FITS
# keywords and the GPB group parameters at the IMIO level and above.

procedure stf_rdheader (im, group, acmode)

pointer	im		# image descriptor
int	group		# group to be accessed
int	acmode		# access mode

long	pixoff
long	fi[LEN_FINFO]
real	datamin, datamax
pointer	sp, stf, lbuf, root, extn, op
int	compress, devblksz, ival, ch, i , junk
int	fits, fitslen, sz_userarea, sz_gpbhdr, len_hdrmem
long	totpix, mtime, ctime

real	imgetr()
int	fnroot(), strlen(), finfo(), imaccf()
errchk	stf_rfitshdr, stf_rgpb, open, realloc, imaddb, imaddi, imgetr

include <szpixtype.inc>

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	stf = IM_KDES(im)

	# Read the FITS header, setting the values of all reserved fields
	# in the STF descriptor and saving all the user FITS cards in the
	# save buffer "fits".

	call stf_rfitshdr (im, fits, fitslen)

	# Process the reserved keywords (set in the STF descriptor) into the
	# corresponding fields of the IMIO descriptor.

        # Set group keywords if STF_GROUPS is NO (BPS 12.06.91).
        if (STF_GROUPS(stf) == NO) {
            STF_GCOUNT(stf) = 1
            STF_PCOUNT(stf) = 0
            STF_PSIZE(stf) = 0
        }

	if (acmode != NEW_COPY) {
	    IM_NDIM(im) = STF_NAXIS(stf)		# IM_NDIM
	    do ival = 1, IM_MAXDIM			# IM_LEN
		IM_LEN(im,ival) = STF_LENAXIS(stf,ival)
	}

	ch = STF_DATATYPE(stf)				# IM_PIXTYPE
	switch (STF_BITPIX(stf)) {
	case 16:
	    if (ch == 'U')
		ival = TY_USHORT
	    else
	        ival = TY_SHORT
	case 32:
	    if (ch == 'R')
		ival = TY_REAL
	    else
		ival = TY_LONG
	case 64:
	    if (ch == 'R')
		ival = TY_DOUBLE
	    else
		ival = TY_COMPLEX
	default:
	    ival = ERR
	}
	IM_PIXTYPE(im) = ival

	call iki_parse (IM_HDRFILE(im), Memc[root], Memc[extn])
	call stf_mkpixfname (Memc[root], Memc[extn], IM_PIXFILE(im),
	    SZ_IMPIXFILE)

	if (finfo (IM_PIXFILE(im), fi) != ERR) {
	    mtime = FI_MTIME(fi)
	    ctime = FI_CTIME(fi)
	}

	IM_NBPIX(im)   = 0			# no. bad pixels
	IM_CTIME(im)   = ctime			# creation time
	IM_MTIME(im)   = mtime			# modify time
	IM_LIMTIME(im) = mtime - 1		# time max/min last updated
	IM_UABLOCKED(im) = YES			# ua cards blocked to 80 chars

	IM_HISTORY(im) = EOS
	junk = fnroot (IM_HDRFILE(im), Memc[lbuf], SZ_LINE)
	call strupr (Memc[lbuf])
	call sprintf (IM_TITLE(im), SZ_IMTITLE, "%s[%d/%d]")
	    call pargstr (Memc[lbuf])
	    call pargi (STF_GROUP(stf))
	    call pargi (STF_GCOUNT(stf))

	# Compute the size of each group in the pixel file, in chars.
	totpix = IM_LEN(im,1)
	do i = 2, IM_NDIM(im)
	    totpix = totpix * IM_LEN(im,i)

	STF_SZGROUP(stf) = totpix * pix_size[IM_PIXTYPE(im)] +
	    STF_PSIZE(stf) / (SZB_CHAR * NBITS_BYTE)

	# Write GPB related cards to the beginning of the IMIO user area.
	call imaddb (im, "GROUPS", STF_GROUPS(stf) == YES)
	call imaddi (im, "GCOUNT", STF_GCOUNT(stf))
	call imaddi (im, "PCOUNT", STF_PCOUNT(stf))
	call imaddi (im, "PSIZE", STF_PSIZE(stf))

	# Extract the group parameter block from the pixfile, encoding the
	# group parameters as FITS cards and appending to the cards above.
	# Get the values of DATAMIN and DATAMAX from the GPB so that we can
	# update the IMIO min/max fields.

	call stf_rgpb (im, group, acmode, datamin, datamax)

	# Reallocate the image descriptor to allow space for the spooled user
	# FITS cards plus a little extra for new parameters.

	sz_gpbhdr = strlen (Memc[IM_USERAREA(im)])
	sz_userarea = sz_gpbhdr + fitslen + SZ_EXTRASPACE

	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_MII_INT-1) / SZ_MII_INT
	len_hdrmem = LEN_IMHDR +
	    (sz_userarea+1 + SZ_MII_INT-1) / SZ_MII_INT

	if (IM_LENHDRMEM(im) < len_hdrmem) {
	    IM_LENHDRMEM(im) = len_hdrmem
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}

	# Append the saved FITS cards from the STF header to the user area.
	# Any cards which redefine GPB cards were already deleted when the
	# fits save buffer was created (we don't want the GPB cards since
	# we already output a FITS card for each GPB parameter above).

	op = IM_USERAREA(im) + sz_gpbhdr
	call amovc (Memc[fits], Memc[op], fitslen+1)

	# Set the IMIO min/max fields.  If the GPB datamin >= datamax the
	# values are invalidated by setting IM_LIMTIME to before the image
	# modification time.  Although datamin/datamax were returned by
	# stg_rgpb above, we refetch the values here to pick up the values
	# from the spooled main header in case there were no entries for
	# these keywords in the GPB (if there are values in the GPB they
	# will override those in the main header).

	if (imaccf (im, "DATAMIN") == YES)
	    datamin = imgetr (im, "DATAMIN")
	if (imaccf (im, "DATAMAX") == YES)
	    datamax = imgetr (im, "DATAMAX")

	IM_MIN(im) = datamin
	IM_MAX(im) = datamax
	if (datamin < datamax)
	    IM_LIMTIME(im) = IM_MTIME(im) + 1
	else
	    IM_LIMTIME(im) = IM_MTIME(im) - 1

	# Call up IMIO set set up the remaining image header fields used to
	# define the physical offsets of the pixels in the pixfile.

	compress = YES		# do not align image lines on blocks
	devblksz = 1		# disable all alignment

	pixoff = (group - 1) * STF_SZGROUP(stf) + 1
	call imioff (im, pixoff, compress, devblksz)

	call sfree (sp)
end
