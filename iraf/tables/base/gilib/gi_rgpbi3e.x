include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"gi.h"

# GI_RGPB_I3E -- A take from stf_rgpb to read gpb values from an image
# residing in a remote UNIX node. Each value is read and converted to vms
# floating point representation or to vms integer.
# Nelson Zarate Aug 1989
# Read the group data block into the first few cards of the user
# area of the IMIO image header.  The GPB is stored as a binary data structure
# in the STF pixfile.  The values of the standard GPB parameters DATAMIN and
# DATAMAX are returned as output arguments.
#

procedure gi_rgpb_i3e (pfd, im, datamin, datamax)

int	pfd			# Pixel file descriptor
pointer	im			# IMIO image descriptor
real	datamin, datamax	# min,max pixel values from GPB

long	offset
pointer	sp, stf, gpb, lbuf, pp
int	pn, sz_param, sz_gpb
real	imgetr()
int	read(), imaccf()
errchk	read

short	bufs
int	bufl

 
string	readerr "cannot read group data block"
string	badtype "illegal group data parameter datatype"
define	minmax_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)

	# Skip ahead if there is no group parameter block.
	if (STF_PSIZE(stf) == 0)
	    goto minmax_

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Read the GPB into a buffer.  The GPB is located at the very end of
	# the data storage area for the group.  If we are opening a new,
	# uninitialized group (acmode = new_image or new_copy), do not
	# physically read the GPB as it is will be uninitialized data.

	# Read directly from the pixel file since the offset is already 
	# done when reading the pixels???
        if (read (pfd, Memc[gpb], sz_gpb) != sz_gpb)
	    call error (1, readerr)

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

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
	       call bswap4 (Memc[gpb+offset], 1, bufl, 1, 4)
	       call imaddb (im, P_PTYPE(pp),  bufl)
	    # changed case for int to short and long--dlb 11/3/87
	    case TY_SHORT:
	       call bswap2 (Memc[gpb+offset], 1, bufs, 1, 2)
	       call imadds (im, P_PTYPE(pp), bufs)
	    case TY_LONG, TY_INT:
	       call bswap4 (Memc[gpb+offset], 1, bufl, 1, 4)
	       call imaddl (im, P_PTYPE(pp), bufl)
	    case TY_REAL:
	       call ieeupkr (Memc[gpb+offset])
	       call imaddr (im, P_PTYPE(pp),  Memc[gpb+offset])
	    case TY_DOUBLE:
	       call ieeupkd (Memc[gpb+offset])
	       call imaddd (im, P_PTYPE(pp), Memc[gpb+offset])
	    case TY_CHAR:
	       call chrupk (Memc[gpb+offset], 1, Memc[lbuf], 1, P_LEN(pp))
	       Memc[lbuf+P_LEN(pp)] = EOS
	       call imastr (im, P_PTYPE(pp), Memc[lbuf])
	    default:
	       call error (1, badtype)
	    }

	    offset = offset + sz_param
	}

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
