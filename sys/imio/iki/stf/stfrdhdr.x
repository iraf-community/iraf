# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<imhdr.h>
include	<imio.h>
include	<fset.h>
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
pointer	sp, stf, lbuf, root, extn
int	compress, devblksz, ival, ch, i , junk
int	spool, sz_userarea, user_header, sz_gpbhdr

long	totpix, mtime, ctime, fstatl()
int	open(), stropen(), strlen(), sizeof(), finfo(), fnroot()
errchk	stf_rfitshdr, stf_rgpb, open

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	stf = IM_KDES(im)

	# We need to write out the FITS encoded GPB before the user FITS
	# cards, so open a memory spool file to spool the user FITS cards.

	spool = open ("fits_spool", READ_WRITE, SPOOL_FILE)

	# Read the FITS header, setting the values of all reserved fields
	# in the STF descriptor and spooling the remaining parameters.
	# This reads the entire FITS header into either the STF descriptor
	# or the spool file.

	call stf_rfitshdr (im, spool)

	# Process the reserved keywords (set in the STF descriptor) into the
	# corresponding fields of the IMIO descriptor.

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

	STF_SZGROUP(stf) = totpix * sizeof (IM_PIXTYPE(im)) +
	    STF_PSIZE(stf) / (SZB_CHAR * NBITS_BYTE)

	# Extract the group parameter block from the pixfile, encoding the
	# group parameters as FITS cards at the beginning of the IMIO user
	# area.  Return the values of DATAMIN and DATAMAX from the GPB so
	# that we can update the IMIO min/max fields.

	call stf_rgpb (im, group, acmode, datamin, datamax)

	# Set the IMIO min/max fields.  If the GPB datamin >= datamax the
	# values are invalidated by setting IM_LIMTIME to before the image
	# modification time.

	IM_MIN(im) = datamin
	IM_MAX(im) = datamax
	if (datamin < datamax)
	    IM_LIMTIME(im) = IM_MTIME(im) + 1
	else
	    IM_LIMTIME(im) = IM_MTIME(im) - 1

	# Reallocate the image descriptor to allow space for the spooled user
	# FITS cards plus a little extra for new parameters.

	sz_gpbhdr = strlen (Memc[IM_USERAREA(im)])
	sz_userarea = sz_gpbhdr + fstatl (spool, F_FILESIZE) + SZ_EXTRASPACE
	STF_SZGPBHDR(stf) = sz_gpbhdr

	# Following line to update amount of space in header 
	# descriptor actually used--added by dlb--6/4/87
	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_STRUCT-1) / SZ_STRUCT

	IM_LENHDRMEM(im) = LEN_IMHDR +
	    (sz_userarea+1 + SZ_STRUCT-1) / SZ_STRUCT
	call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)

	# Append the spooled FITS cards from the STF header to the user area,
	# opening the user area as a string file.

	user_header = stropen (Memc[IM_USERAREA(im)], sz_userarea, APPEND)
	call seek (spool, BOFL)
	call fcopyo (spool, user_header)
	call close (user_header)
	call close (spool)

	# Call up IMIO set set up the remaining image header fields used to
	# define the physical offsets of the pixels in the pixfile.

	compress = YES		# do not align image lines on blocks
	devblksz = 1		# disable all alignment

	pixoff = (group - 1) * STF_SZGROUP(stf) + 1
	call imioff (im, pixoff, compress, devblksz)

	call sfree (sp)
end
