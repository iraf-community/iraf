include <imio.h>
include <mach.h>
include	"gi.h"

# GI_CRGPB -- Pack the buffer read from the FITS table into the GPB

procedure gi_crgpb (im, group, buffer)

pointer	im		# i: image descriptor
int	group		# i: group number
char	buffer[ARB]	# i: buffer containing gpb values
#--
bool	bval
double	dval
long	lval
real	rval
short	sval
int	gpboff, bufoff, pfd, pn, sz_param, sz_gpb, i, nch
pointer	sp, stf, gpb, lbuf, pp, op, ip, ival

bool	streq()
int	ctowrd(), ctol(), ctor(), ctod(), ctoi(), gstrcpy(), strlen()
errchk	seek

string	writerr "cannot update group parameter block"
string	badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

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
		call amovc (bval, Memc[op], SZ_BOOL)

	    case TY_SHORT:
		nch = ctoi (buffer[bufoff], ip, ival)
		sval = ival
		call amovc (sval, Memc[op], SZ_SHORT)

	    case TY_INT,TY_LONG:
		nch = ctol (buffer[bufoff], ip, lval)
		call amovc (lval, Memc[op], SZ_LONG)

	    case TY_REAL:
		nch = ctor (buffer[bufoff], ip, rval)
		call amovc (rval, Memc[op], SZ_REAL)

	    case TY_DOUBLE:
		nch = ctod (buffer[bufoff], ip, dval)
		call amovc (dval, Memc[op], SZ_DOUBLE)

	    case TY_CHAR:
		# Blank fill the string buffer.
		nch = gstrcpy (buffer[bufoff], Memc[lbuf], P_LEN(pp))
		do i = nch+1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Pack the blank filled array into the GPB.
		call chrpak (Memc[lbuf], 1, Memc[gpb+gpboff], 1, P_LEN(pp))

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
