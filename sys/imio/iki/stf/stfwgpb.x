# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"stf.h"

# STF_WGPB -- Write the group parameter block data back into the pixel file.
# The GPB is described by a structure member list in the STF descriptor.
# The values of the GPB parameters are encoded as FITS cards in the user
# area of the IMIO descriptor.
#
# DLB--11/3/87: Made changes to allow i*2 and i*4 integer parameters in gpb.

procedure stf_wgpb (im, group, datamin, datamax)

pointer	im			# IMIO image descriptor
int	group			# group to be accessed
real	datamin, datamax	# new min, max pixel values

long	offset
pointer	sp, stf, gpb, lbuf, pp, op
int	pfd, pn, sz_param, sz_gpb, i

int	open(), strlen()
bool	bval, imgetb()
# changed to short and long for short integers in gpb
short	sval, imgets()
long	lval, imgetl()
#
real	rval, imgetr()
double	dval, imgetd()
errchk	open, seek
int	imaccf()

string	writerr "cannot update group parameter block"
string	badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)
	pfd = STF_PFD(stf)

	# Not all images have group parameter blocks.
	if (STF_PSIZE(stf) == 0) {
	    call sfree (sp)
	    return
	}

	# Open the pixel file if not already open.
	if (pfd == NULL) {
	    pfd = open (IM_PIXFILE(im), READ_WRITE, BINARY_FILE)
	    STF_PFD(stf) = pfd
	}

	# Update the values of DATAMIN, DATAMAX.
	if (imaccf (im, "DATAMIN") == YES && 
            imaccf (im, "DATAMAX") == YES) {

	    iferr {
	        call imputr (im, "DATAMIN", datamin)
	        call imputr (im, "DATAMAX", datamax)
	    } then
	        call erract (EA_WARN)
	}

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
	    op = gpb + offset

	    # Fetch the value of the parameter from IMIO and write it into
	    # the GPB binary data structure.

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		iferr (bval = imgetb (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    bval = false
		}
		# Memb[(op-1)/SZ_BOOL+1] = bval
		if (SZ_INT != SZ_INT32) {
		    call i64to32 (bval, bval, 1)
		    call amovc (bval, Memc[op], SZ_INT32)
		} else
		    call amovc (bval, Memc[op], SZ_BOOL)

	    # changed case for int to short and long 
	    # to allow i*2 in gpb--dlb 11/3/87
	    case TY_SHORT:
		iferr (sval = imgets (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    sval = 0
		}
		call amovc (sval, Memc[op], SZ_SHORT)

	    case TY_LONG:
		iferr (lval = imgetl (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    lval = 0
		}
		if (SZ_INT != SZ_INT32) {
		    call i64to32 (lval, lval, 1)
		    call amovc (lval, Memc[op], SZ_INT32)
		} else
		    call amovc (lval, Memc[op], SZ_LONG)

	    case TY_REAL:
		iferr (rval = imgetr (im, P_PTYPE(pp))) {
		    # Currently with MWCS, WCS cards such as CRVAL, CDi_j,
		    # etc. (always type real or double) are omitted from the
		    # header if their value is zero.  Hence if the card is
		    # missing assume a value of zero rather than issue a
		    # warning.

		    # call erract (EA_WARN)
		    rval = 0.0
		}
		# Memr[(op-1)/SZ_REAL+1] = rval
		call amovc (rval, Memc[op], SZ_REAL)

	    case TY_DOUBLE:
		iferr (dval = imgetd (im, P_PTYPE(pp))) {
		    # Skip warning as assume zero, as above or TY_REAL.
		    # call erract (EA_WARN)
		    dval = 0.0D0
		}
		# Memd[(op-1)/SZ_DOUBLE+1] = dval
		call amovc (dval, Memc[op], SZ_DOUBLE)

	    case TY_CHAR:
		# Blank fill the string buffer.
		do i = 1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Fetch the string value of the parameter.
		iferr (call imgstr (im, P_PTYPE(pp), Memc[lbuf], SZ_LINE))
		    call erract (EA_WARN)

		# Replace the EOS delimiter by a blank.
		i = strlen (Memc[lbuf])
		Memc[lbuf+i] = ' '

		# Pack the blank filled array into the GPB.
		call chrpak (Memc[lbuf], 1, Memc[gpb+offset], 1, P_LEN(pp))

	    default:
		call error (1, badtype)
	    }

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}

	# Write the GPB into the pixfile.  The GPB is located at the very end
	# of the data storage area for the group.

	offset = (group * STF_SZGROUP(stf) + 1) - sz_gpb
	call seek (pfd, offset)
	iferr (call write (pfd, Memc[gpb], sz_gpb))
	    call error (5, writerr)

	call sfree (sp)
end
