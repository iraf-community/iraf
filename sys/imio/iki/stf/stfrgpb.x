# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"stf.h"

# STF_RGPB -- Read the group data block into the first few cards of the user
# area of the IMIO image header.  The GPB is stored as a binary data structure
# in the STF pixfile.  The values of the standard GPB parameters DATAMIN and
# DATAMAX are returned as output arguments.
#
# DLB--11/03/87: Made changes to allow i*2 and i*4 integer parameters in GPB.
# DLB--11/11/87: Changed calculation of character string length in GPB to
# avoid integer truncation error by using P_PSIZE directly.

procedure stf_rgpb (im, group, acmode, datamin, datamax)

pointer	im			# IMIO image descriptor
int	group			# group to be accessed
int	acmode			# image access mode
real	datamin, datamax	# min,max pixel values from GPB

#int	i
#char	pname[SZ_KEYWORD]

long	offset
bool	newgroup
pointer	sp, stf, gpb, lbuf, pp
int	pfd, pn, sz_param, sz_gpb
errchk	imaddb, imadds, imaddl, imaddr, imaddd, imastr
errchk	imputd, impstr, open, read
int	open(), read(), imaccf()
real	imgetr()

string	readerr "cannot read group data block - no such group?"
string	badtype "illegal group data parameter datatype"
define	minmax_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)
	pfd = STF_PFD(stf)

	# Skip ahead if there is no group parameter block.
	if (STF_PSIZE(stf) == 0)
	    goto minmax_

	# Open the pixel file if not already open.
	if (pfd == NULL) {
	    iferr {
		if (IM_ACMODE(im) == READ_ONLY)
		    pfd = open (IM_PIXFILE(im), READ_ONLY, BINARY_FILE)
		else
		    pfd = open (IM_PIXFILE(im), READ_WRITE, BINARY_FILE)
		STF_PFD(stf) = pfd
	    } then {
		call eprintf ("Warning: Cannot open pixfile to read GPB (%s)\n")
		    call pargstr (IM_NAME(im))
		pfd = NULL
	    }
	}

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Read the GPB into a buffer.  The GPB is located at the very end of
	# the data storage area for the group.  If we are opening a new,
	# uninitialized group (acmode = new_image or new_copy), do not
	# physically read the GPB as it is will be uninitialized data.

	newgroup = (acmode == NEW_IMAGE || acmode == NEW_COPY || pfd == NULL)
	if (newgroup)
	    call aclrc (Memc[gpb], sz_gpb)
	else {
	    offset = (group * STF_SZGROUP(stf) + 1) - sz_gpb
	    call seek (pfd, offset)
	    if (read (pfd, Memc[gpb], sz_gpb) != sz_gpb)
		call error (1, readerr)
	}

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

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

	    # Extract the binary parameter value and add a FITS encoded card
	    # to the IMIO user area.  In the case of a new copy image, the
	    # GPB values will already be in the image header, do not modify
	    # the parameter value, but add the parameter if it was not
	    # inherited from the old image.

	    if (acmode != NEW_COPY || imaccf (im, P_PTYPE(pp)) == NO) {
		switch (P_SPPTYPE(pp)) {
		case TY_BOOL:
		    call imaddb (im, P_PTYPE(pp), Memc[gpb+offset])
		# changed case for int to short and long--dlb 11/3/87
		case TY_SHORT:
		    call imadds (im, P_PTYPE(pp), Memc[gpb+offset])
		case TY_LONG:
		    call imaddl (im, P_PTYPE(pp), Memc[gpb+offset])
		case TY_REAL:
		    call imaddr (im, P_PTYPE(pp), Memc[gpb+offset])
		case TY_DOUBLE:
		    call imaddd (im, P_PTYPE(pp), Memc[gpb+offset])
		case TY_CHAR:
		    call chrupk (Memc[gpb+offset], 1, Memc[lbuf], 1, P_LEN(pp))
		    Memc[lbuf+P_LEN(pp)] = EOS
		    call imastr (im, P_PTYPE(pp), Memc[lbuf])
		default:
		    call error (1, badtype)
		}
	    }

	    offset = offset + sz_param
	}

	# If writing to a new element of a group format image, in which case
	# the GPB has been zeroed, provide the default pixel WCS.

#	if (newgroup)
#	    do i = 1, IM_NDIM(im) {
#		call sprintf (pname, SZ_KEYWORD, "CTYPE%d")
#		    call pargi (i)
#		call impstr (im, pname, "PIXEL")
#
#		call sprintf (pname,  SZ_KEYWORD, "CD%d_%d")
#		    call pargi (i)
#		    call pargi (i)
#		call imputd (im, pname, 1.0D0)
#	    }

minmax_
	# Return DATAMIN, DATAMAX.  This is done by searching the user area so
	# that ordinary keywords may be used to set datamin and datamax if the
	# GPB is not used.

	datamin = 0.0; datamax = 0.0
	if (imaccf (im, "DATAMIN") == YES)
	    datamin = imgetr (im, "DATAMIN")
	if (imaccf (im, "DATAMAX") == YES)
	    datamax = imgetr (im, "DATAMAX")

	call sfree (sp)
end
