# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include "qpoe.h"
include "qpio.h"

# QPIO_LOADMASK -- Load the named region mask into the QPIO descriptor.
# The mask name may be the name of a header parameter containing the mask
# as the stored array value (TY_OPAQUE parameter), the name of a header
# parameter containing the name of the mask (TY_CHAR), or the name of a
# mask storage file (.pl extension).

procedure qpio_loadmask (io, mask, merge)

pointer	io			#I QPIO descriptor
char	mask[ARB]		#I mask to be loaded
int	merge			#I merge with old mask?

int	niter
int	naxes, axlen[PL_MAXDIM], v[PL_MAXDIM]
pointer	sp, title, mp, sym, plbuf, qp, o_pl, n_pl, b_pl

pointer	pl_open(), qp_gpsym()
int	strmatch(), qp_accessf(), qp_read(), qp_gstr()
errchk	pl_open, pl_close, pl_loadf, qp_read, syserrs, qp_gstr, malloc
define	tryfile_ 91

begin
	call smark (sp)
	call salloc (title, SZ_FNAME, TY_CHAR)
	call salloc (mp, SZ_FNAME, TY_CHAR)

	if (IO_DEBUG(io) > 0) {
	    call eprintf ("load mask `%s'\n")
		call pargstr (mask)
	}

	qp = IO_QP(io)
	o_pl = IO_PL(io)
	call strcpy (mask, Memc[mp], SZ_FNAME)

	# Open new mask.
	for (niter=0;  Memc[mp] != EOS;  niter=niter+1) {
	    if (strmatch (Memc[mp], ".pl$") > 0) {
		# Mask is stored in a file.
tryfile_
		n_pl = pl_open (NULL)
		call pl_loadf (n_pl, Memc[mp], Memc[title], SZ_FNAME)

	    } else if (qp_accessf (qp, Memc[mp]) == YES) {
		# Named parameter contains or points to mask.

		sym = qp_gpsym (qp, Memc[mp])
		if (S_DTYPE(sym) == TY_OPAQUE) {
		    # Parameter value is stored mask.
		    call salloc (plbuf, S_NELEM(sym) / SZ_SHORT, TY_SHORT)
		    if (qp_read (qp, Memc[mp], Mems[plbuf], S_NELEM(sym), 1,
			"opaque") < S_NELEM(sym)) {
			call syserrs (SYS_QPBADVAL, Memc[mp])
		    } else {
			n_pl = pl_open (plbuf)	# no deref
		    }

		} else if (S_DTYPE(sym) == TY_CHAR) {
		    # Parameter value is pointer to mask.
		    if (qp_gstr (qp, Memc[mp], Memc[mp], SZ_FNAME) > 0) {
			if (niter < MAX_INDIR)
			    next
			else
			    call syserrs (SYS_QPMRECUR, Memc[mp])
		    }
		} else
		    goto tryfile_
	    } else
		goto tryfile_

	    break
	}

	# Check that mask and image are the same size, and get mask depth.
	call pl_gsize (n_pl, naxes, axlen, IO_MDEPTH(io))
	if (axlen[1] != IO_NCOLS(io) || axlen[2] != IO_NLINES(io))
	    call syserrs (SYS_QPPLSIZE, Memc[mp])

	# Merge the old and new mask if so indicated.  The result mask is the
	# same as the new mask, but only those pixels also present (nonzero)
	# in the old mask are preserved.

	if (merge == YES && o_pl != NULL) {
	    b_pl = pl_open (NULL)
	    call amovkl (1, v, PL_MAXDIM)
	    call pl_ssize (b_pl, naxes, axlen, 1)
	    call pl_rop (o_pl, v, b_pl, v, axlen, PIX_SRC)
	    call pl_rop (b_pl, v, n_pl, v, axlen, and(PIX_SRC,PIX_DST))
	    call pl_close (b_pl)
	}

	# Close old mask, if any.
	if (IO_PL(io) != NULL && IO_PLCLOSE(io) == YES)
	    call pl_close (IO_PL(io))

	# Install new mask.
	IO_PL(io) = n_pl
	IO_PLCLOSE(io) = YES
	call strcpy (Memc[mp], Memc[IO_MASK(io)], SZ_FNAME)

	# Allocate a range list buffer if i/o is indexed.
	if (IO_INDEXLEN(io) > 0) {
	    if (IO_RL(io) != NULL)
		call mfree (IO_RL(io), TY_INT)
	    if (IO_PL(io) != NULL)
		call malloc (IO_RL(io), RL_MAXLEN(IO_PL(io)), TY_INT)
	    else
		call malloc (IO_RL(io), RL_LENELEM*2, TY_INT)
	}

	call sfree (sp)
end
