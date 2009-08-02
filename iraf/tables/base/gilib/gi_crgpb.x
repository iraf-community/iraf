include <imio.h>
include <mach.h>
include	"gi.h"

# GI_CRGPB -- Pack the buffer read from the FITS table into the GPB

procedure gi_crgpb (im, group, buffer)

pointer	im		# i: image descriptor
int	group		# i: group number
char	buffer[ARB]	# i: buffer containing gpb values
#--
size_t	sz_val, c_1
bool	bval
double	dval
int	ival
long	lval
real	rval
short	sval
long	gpboff
size_t	sz_gpb, sz_param
int	bufoff, pfd, pn, i, nch, ip
pointer	sp, stf, gpb, lbuf, pp, op

bool	streq()
int	ctowrd(), ctol(), ctor(), ctod(), ctoi(), gstrcpy(), strlen()
errchk	seek

string	writerr "cannot update group parameter block"
string	badtype "illegal group data parameter datatype"

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (lbuf, sz_val, TY_CHAR)

	stf = IM_KDES(im)
	pfd = STF_PFD(stf)

	# Allocate a buffer for the GPB.

	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	bufoff = 1
	gpboff = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
	    op = gpb + gpboff

	    ip = 1
	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		nch = ctowrd (buffer[bufoff], ip, Memc[lbuf], SZ_LINE)
		bval = streq (Memc[lbuf], "T")
		sz_val = SZ_BOOL
		call amovc (bval, Memc[op], sz_val)

	    case TY_SHORT:
		nch = ctoi (buffer[bufoff], ip, ival)
		sval = ival
		sz_val = SZ_SHORT
		call amovc (sval, Memc[op], sz_val)

	    case TY_INT:
		nch = ctoi (buffer[bufoff], ip, ival)
		sz_val = SZ_INT
		call amovc (ival, Memc[op], sz_val)

	    case TY_LONG:
		nch = ctol (buffer[bufoff], ip, lval)
		sz_val = SZ_LONG
		call amovc (lval, Memc[op], sz_val)

	    case TY_REAL:
		nch = ctor (buffer[bufoff], ip, rval)
		sz_val = SZ_REAL
		call amovc (rval, Memc[op], sz_val)

	    case TY_DOUBLE:
		nch = ctod (buffer[bufoff], ip, dval)
		sz_val = SZ_DOUBLE
		call amovc (dval, Memc[op], sz_val)

	    case TY_CHAR:
		# Blank fill the string buffer.
		nch = gstrcpy (buffer[bufoff], Memc[lbuf], P_LEN(pp))
		do i = nch+1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Pack the blank filled array into the GPB.
		sz_val = P_LEN(pp)
		call chrpak (Memc[lbuf], c_1, Memc[gpb+gpboff], c_1, sz_val)

	    default:
		call error (1, badtype)
	    }

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    bufoff = bufoff + strlen (buffer[bufoff]) + 1
	    gpboff = gpboff + sz_param
	}

	# Write the GPB into the pixfile.  The GPB is located at the very end
	# of the data storage area for the group .

	gpboff = (group * STF_SZGROUP(stf) + 1) - sz_gpb
	call seek (pfd, gpboff)
	iferr (call write (pfd, Memc[gpb], sz_gpb))
	    call error (5, writerr)

	call sfree (sp)
end
