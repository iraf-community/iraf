# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<qpset.h>
include	"qpf.h"

# QPF_COPYPARAMS -- Copy parameters from the QPOE datafile header into the
# image header.  Only scalar parameters are copied.

procedure qpf_copyparams (im, qp)

pointer	im			#I image descriptor
pointer	qp			#I QPOE descriptor

int	nelem, dtype, maxelem, flags
pointer	sp, param, text, comment, datatype, fl, qpf, mw, io

pointer	qp_ofnlu(), qpio_loadwcs()
int	qp_gnfn(), qp_queryf(), stridx(), strdic()
errchk	qp_ofnlu, qp_gnfn, qp_queryf, imaddi, qp_geti, mw_saveim

bool	qp_getb()
short	qp_gets()
int	qp_geti(), qp_gstr()
real	qp_getr()
double	qp_getd()

begin
	call smark (sp)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_COMMENT, TY_CHAR)
	call salloc (datatype, SZ_DATATYPE, TY_CHAR)

	qpf = IM_KDES(im)

	# Copy QPOE special keywords.
	call imaddi (im, "NAXES",  qp_geti(qp,"naxes"))
	call imaddi (im, "AXLEN1", qp_geti(qp,"axlen[1]"))
	call imaddi (im, "AXLEN2", qp_geti(qp,"axlen[2]"))
	call imaddr (im, "XBLOCK", QPF_XBLOCK(qpf))
	call imaddr (im, "YBLOCK", QPF_YBLOCK(qpf))

	# Output the QPOE filter.
	iferr (call qpf_wfilter (qpf, im))
	    call erract (EA_WARN)

	# Compute and output any filter attributes.
	iferr (call qpf_wattr (qpf, im))
	    call erract (EA_WARN)

	# Copy the WCS, if any.
	io = QPF_IO(qpf)
	if (io != NULL)
	    ifnoerr (mw = qpio_loadwcs (io)) {
		call mw_saveim (mw, im)
		call mw_close (mw)
	    }

	# Copy general keywords.
	fl = qp_ofnlu (qp, "*")

	while (qp_gnfn (fl, Memc[param], SZ_FNAME) != EOF) {
	    # Get the next scalar parameter which has a nonnull value.
	    nelem = qp_queryf (qp, Memc[param], Memc[datatype], maxelem,
		Memc[comment], flags)
	    if (strdic (Memc[param], Memc[text], SZ_LINE, OMIT) > 0)
		next

	    dtype = stridx (Memc[datatype], "bcsilrdx")

	    # Make entry for a parameter which has no value, or an unprintable
	    # value.

	    if (nelem == 0 || (nelem > 1 && dtype != TY_CHAR) ||
		dtype < TY_BOOL || dtype > TY_COMPLEX) {

		call sprintf (Memc[text], SZ_LINE, "%14s[%03d] %s")
		    call pargstr (Memc[datatype])
		    call pargi (nelem)
		    call pargstr (Memc[comment])

		iferr (call imastr (im, Memc[param], Memc[text]))
		    call erract (EA_WARN)
		next
	    }

	    # Copy parameter to image header.
	    iferr {
		switch (dtype) {
		case TY_BOOL:
		    call imaddb (im, Memc[param], qp_getb(qp,Memc[param]))
		case TY_CHAR:
		    if (qp_gstr (qp, Memc[param], Memc[text], SZ_LINE) > 0)
			call imastr (im, Memc[param], Memc[text])
		case TY_SHORT:
		    call imadds (im, Memc[param], qp_gets(qp,Memc[param]))
		case TY_INT, TY_LONG:
		    call imaddi (im, Memc[param], qp_geti(qp,Memc[param]))
		case TY_REAL:
		    call imaddr (im, Memc[param], qp_getr(qp,Memc[param]))
		case TY_DOUBLE:
		    call imaddd (im, Memc[param], qp_getd(qp,Memc[param]))
		case TY_COMPLEX:
		    ; # not supported.
		}
	    } then {
		call erract (EA_WARN)
		break
	    }
	}

	call qp_cfnl (fl)
	call sfree (sp)
end
