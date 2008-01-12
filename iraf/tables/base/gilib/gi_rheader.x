include	<finfo.h>
include	<imhdr.h>
include	<imio.h>
include	<fset.h>
include	<mach.h>
include	"gi.h"

# GI_RHEADER -- Read the STF format image header for a single group into the
# IMIO descriptor.  The standard fields are processed into the fields of the
# descriptor.  The GPB binary parameters are encoded as FITS cards and placed
# in the IMIO user area, followed by all extra cards in the FITS format STF
# group header.  Note that no distinction is made between the common FITS
# keywords and the GPB group parameters at the IMIO level and above.

procedure gi_rheader (fd, im)

int	fd		# input file descriptor
pointer	im		# image descriptor

long	fi[LEN_FINFO]
pointer	sp, stf, lbuf, root, extn,pn,pp, op
int	compress, devblksz, ival, ch, i , junk
int	sz_userarea, sz_gpbhdr

long	totpix, mtime, ctime
int	strlen(), sizeof(), finfo(), fnroot()
errchk	stf_rfitshdr
int	fits, fitslen, group, pixoff, len_hdrmem
real	offset, sz_param
string badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	stf = IM_KDES(im)


	# Read the FITS header, setting the values of all reserved fields
	# in the STF descriptor and spooling the remaining parameters.
	# This reads the entire FITS header into either the STF descriptor
	# or the spool file.

	# Reset value of input file descriptor
	# Close the file openned by immap before reassing the fd.
	call close (IM_HFD(im))
	IM_HFD(im) = fd
	call stf_rfitshdr (im, fits, fitslen)

	# Process the reserved keywords (set in the STF descriptor) into the
	# corresponding fields of the IMIO descriptor.

	IM_NDIM(im) = STF_NAXIS(stf)		# IM_NDIM
	do ival = 1, IM_NDIM(im)			# IM_LEN
	   IM_LEN(im,ival) = STF_LENAXIS(stf,ival)
	do ival = IM_NDIM(im)+1, IM_MAXDIM
	   IM_LEN(im,ival) = 1

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

	# The gpb will be from a special routine to handle different
	# architectures. At this time we are reading from a SUN (ieee standard)
	# and want to convert the floating point to a VMS ones.

	# Put the gpb keyword at the beginning of the user area.
	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
		
	    # Fill in the unitialized fields of the GPB parameter descriptor.
	    P_OFFSET(pp) = offset
	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR

	    switch (P_PDTYPE(pp)) {
 	    # changed case for int to short and long--dlb 11/3/87
	    case 'I':
		if (sz_param == SZ_SHORT)
		    P_SPPTYPE(pp) = TY_SHORT
		else
		    P_SPPTYPE(pp) = TY_LONG
		P_LEN(pp) = 1
	    case 'R':
		if (sz_param == SZ_REAL)
		    P_SPPTYPE(pp) = TY_REAL
		else
		    P_SPPTYPE(pp) = TY_DOUBLE
		P_LEN(pp) = 1
	    case 'C':
		P_SPPTYPE(pp) = TY_CHAR
		# calculate length directly from PSIZE to avoid truncation error
		P_LEN(pp) = min (SZ_LINE, P_PSIZE(pp) / NBITS_BYTE)
	    case 'L':
		P_SPPTYPE(pp) = TY_BOOL
		P_LEN(pp) = 1
	    default:
		call error (1, badtype)
	    }

		switch (P_SPPTYPE(pp)) {
		case TY_BOOL:
		    call imaddb (im, P_PTYPE(pp), 1)
		# changed case for int to short and long--dlb 11/3/87
		case TY_SHORT:
		    call imadds (im, P_PTYPE(pp), 1)
		case TY_LONG:
		    call imaddl (im, P_PTYPE(pp), 1)
		case TY_REAL:
		    call imaddr (im, P_PTYPE(pp), 1.0)
		case TY_DOUBLE:
		    call imaddd (im, P_PTYPE(pp), 1.0d0)
		case TY_CHAR:
		    call imastr (im, P_PTYPE(pp), ' ')
		default:
		    call error (1, badtype)
		}
	offset = offset + sz_param
	}
	
	# Reallocate the image descriptor to allow space for the spooled user
	# FITS cards plus a little extra for new parameters.

	sz_gpbhdr = strlen (Memc[IM_USERAREA(im)])
	sz_userarea = sz_gpbhdr + fitslen + SZ_EXTRASPACE

	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_STRUCT-1) / SZ_STRUCT
	len_hdrmem = LEN_IMHDR +
	    (sz_userarea+1 + SZ_STRUCT-1) / SZ_STRUCT
	
	if (IM_LENHDRMEM(im) < len_hdrmem) {
	   IM_LENHDRMEM(im) = len_hdrmem
	   call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}

	# Append the spooled FITS cards from the STF header to the user area,
	# opening the user area as a string file.

	op = IM_USERAREA(im) + sz_gpbhdr
	call amovc (Memc[fits], Memc[op], fitslen+1)

	# Call up IMIO set set up the remaining image header fields used to
	# define the physical offsets of the pixels in the pixfile.

	compress = YES		# do not align image lines on blocks
	devblksz = 1		# disable all alignment

	group = 1
	pixoff = (group - 1) * STF_SZGROUP(stf) + 1
	call imioff (im, pixoff, compress, devblksz)

	call sfree (sp)
end
