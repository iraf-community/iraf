# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PL_SAVE -- Save a mask in a buffer in a machine independent format.  The user
# supplied buffer will be resized if necessary to hold the full encoded mask.
# If a NULL buffer pointer is given, a new buffer will be allocated with the
# pointer value overwriting the NULL (hence NULL must not be passed as a
# constant in the argument list).  The length of the encoded mask in words is
# returned as the function value; this value will be less than BUFLEN is the
# user supplied buffer is larger than it needs to be to store the mask, i.e.,
# the user buffer is not resized or reallocated if it is large enough to store
# the mask.  The existing contents of the buffer will be overwritten.  Multiple
# calls may be made to checkpoint the mask, allowing rollback to an earlier
# state.

int procedure pl_save (pl, bp, buflen, flags)

pointer	pl			#I mask descriptor
pointer	bp			#U buffer pointer (to short), or NULL
int	buflen			#U buffer length, shorts
int	flags			#I not used at present

pointer	sp, index, ex, op
int	sz_index, n_buflen
int	pl_p2li()
pointer	coerce()
errchk	malloc, realloc, pl_compress

begin
	call smark (sp)
	call salloc (ex, LEN_PLEXTERN, TY_STRUCT)
	call salloc (index, PL_NLP(pl) * 3 + LL_CURHDRLEN, TY_SHORT)

	# Eliminate any wasted space in the mask, and compute the amount
	# of space needed to store the compressed mask.  Compress the index
	# first to eliminate wasted space; this can make a big difference
	# for a sparse or empty mask.

	call pl_compress (pl)
	sz_index = pl_p2li (PL_LP(pl,1), 1, Mems[index], PL_NLP(pl))
	#n_buflen = (LEN_PLEXTERN * SZ_STRUCT + PL_LLLEN(pl) * SZ_SHORT +
	n_buflen = (LEN_PLEXTERN * SZ_MII_INT + PL_LLLEN(pl) * SZ_SHORT +
	    sz_index * SZ_SHORT) / SZ_SHORT

	# Allocate or resize the output buffer.
	if (bp == NULL) {
	    call malloc (bp, n_buflen, TY_SHORT)
	    buflen = n_buflen
	} else if (n_buflen > buflen) {
	    call realloc (bp, n_buflen, TY_SHORT)
	    buflen = n_buflen
	}

	# Encode and output the external format header structure.
	call aclri (Memi[ex], LEN_PLEXTERN)

	PLE_MAGIC(ex)	= PL_MAGIC(pl)
	PLE_NAXES(ex)	= PL_NAXES(pl)
	PLE_LLOP(ex)	= PL_LLOP(pl)
	PLE_LLLEN(ex)	= PL_LLLEN(pl)
	PLE_MAXLINE(ex) = PL_MAXLINE(pl)
	PLE_MAXVAL(ex)  = PL_MAXVAL(pl)
	PLE_NLP(ex)	= PL_NLP(pl)
	PLE_NLPX(ex)	= sz_index
	PLE_EXLEN(ex)	= n_buflen

	op = bp
	call amovl (PL_AXLEN(pl,1), PLE_AXLEN(ex,1), PL_MAXDIM)
	call miipak32 (Memi[ex], Memi[coerce(op,TY_SHORT,TY_INT)],
	    LEN_PLEXTERN, TY_STRUCT)
	#op = op + (LEN_PLEXTERN * SZ_STRUCT) / SZ_SHORT
	op = op + (LEN_PLEXTERN * SZ_MII_INT) / SZ_SHORT

	# Append the compressed index...
	call miipak16 (Mems[index], Mems[op], sz_index, TY_SHORT)
	op = op + sz_index

	# and the line list buffer.
	call miipak16 (LL(pl,0), Mems[op], PL_LLLEN(pl), TY_SHORT)
	op = op + PL_LLLEN(pl)

	call sfree (sp)
	return (op - bp)
end
