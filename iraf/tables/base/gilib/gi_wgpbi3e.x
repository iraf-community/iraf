include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"gi.h"

# GI_WGPB_I3E -- A take from stf_wgpb to write the gpb values into
# a pixel file in ieee floating point format for the input
# vms floating point number or to UNIX integer order for the input
# vms integer numbers.
# Nelson Zarate Aug. 1989
# STF_WGPB -- Write the group parameter block data back into the pixel file.
# The GPB is described by a structure member list in the STF descriptor.
# The values of the GPB parameters are encoded as FITS cards in the user
# area of the IMIO descriptor.
#
# DLB--11/3/87: Made changes to allow i*2 and i*4 integer parameters in gpb.

procedure gi_wgpb_i3e (im, fd, group)

pointer	im			# IMIO image descriptor
pointer fd			# output file descriptor
int	group			# group to be accessed

long	offset
pointer	sp, stf, gpb, lbuf, pp, op
int	pfd, pn, sz_param, sz_gpb, i

int	strlen()
bool	bval, imgetb()
# changed to short and long for short integers in gpb
short	sval, imgets()
long	lval, imgetl()
#
real	rval, imgetr()
double	dval, imgetd()

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
		call bswap4 (bval, 1, Memc[op], 1, 4)

	    # changed case for int to short and long 
	    # to allow i*2 in gpb--dlb 11/3/87
	    case TY_SHORT:
		iferr (sval = imgets (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    sval = 0
		}
		call bswap2 (sval, 1, Memc[op], 1, 2)

	    case TY_LONG, TY_INT:
		iferr (lval = imgetl (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    lval = 0
		}
		call bswap4 (lval, 1, Memc[op], 1, 4)

	    case TY_REAL:
		iferr (rval = imgetr (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    rval = 0.0
		}
		# Memr[(op-1)/SZ_REAL+1] = rval
#		call vx2sunr (rval, Memc[op], 1)
		call ieevpakr (rval, Memc[op], 1)

	    case TY_DOUBLE:
		iferr (dval = imgetd (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    dval = 0.0D0
		}
		# Memd[(op-1)/SZ_DOUBLE+1] = dval
#		call vx2sund (dval, Memc[op], 1)
		call ieevpakd (dval, Memc[op], 1)

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

	iferr {
	    call write (fd, Memc[gpb], sz_gpb)
	} then
	    call error (5, writerr)

	call sfree (sp)
end
