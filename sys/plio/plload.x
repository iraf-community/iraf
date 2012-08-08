# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <plset.h>
include	<plio.h>

# PL_LOAD -- Load a mask from a buffer encoded in a machine independent format
# in a previous call to PL_SAVE.  The given mask descriptor may be either
# inactive or active.  In the case of a load into an active mask, the existing
# mask is clobbered, and the mask may change size.

procedure pl_load (pl, bp)

pointer	pl			#I mask descriptor
pointer	bp			#I buffer pointer (to short)

pointer	sp, index, ex, ip
int	o_lllen, o_nlp, sz_index, flags, nlp, i
errchk	malloc, realloc, syserr
int	pl_l2pi()
pointer	coerce()

begin
	call smark (sp)
	call salloc (ex, LEN_PLEXTERN, TY_STRUCT)

	o_lllen = PL_LLLEN(pl)
	o_nlp = PL_NLP(pl)

	# Decode the external format header structure, a fixed size structure
	# stored in 32 bit MII integer format.

	call miiupk32 (Memi[coerce(bp,TY_SHORT,TY_INT)], Memi[ex],
	    LEN_PLEXTERN, TY_STRUCT)
	if (PLE_MAGIC(ex) != PL_MAGICVAL)
	    call syserr (SYS_PLBADSAVEF)

	call amovi (PLE_AXLEN(ex,1), PL_AXLEN(pl,1), PL_MAXDIM)
	PL_MAGIC(pl)    = PLE_MAGIC(ex)
	PL_NAXES(pl)	= PLE_NAXES(ex)
	PL_LLOP(pl)	= PLE_LLOP(ex)
	PL_LLLEN(pl)	= PLE_LLLEN(ex)
	PL_MAXLINE(pl)	= PLE_MAXLINE(ex)
	PL_MAXVAL(pl)	= PLE_MAXVAL(ex)
	PL_NLP(pl)	= PLE_NLP(ex)
	sz_index	= PLE_NLPX(ex)
	flags		= PLE_FLAGS(ex)

	# Get the (compressed) line index.  If the descriptor is already active
	# the new mask may be a different size than the old one.

	nlp = 1
	do i = 2, PL_NAXES(pl)
	    nlp = nlp * PL_AXLEN(pl,i)
	if (PL_LPP(pl) == NULL)
	    call malloc (PL_LPP(pl), nlp, TY_INT)
	else if (nlp != o_nlp)
	    call realloc (PL_LPP(pl), nlp, TY_INT)

	call salloc (index, sz_index, TY_SHORT)
	#ip = bp + (LEN_PLEXTERN * SZ_STRUCT) / SZ_SHORT
	ip = bp + (LEN_PLEXTERN * SZ_MII_INT) / SZ_SHORT
	call miiupk16 (Mems[ip], Mems[index], sz_index, TY_SHORT)
	PL_NLP(pl) = pl_l2pi (Mems[index], 1, PL_LP(pl,1), nlp)

	# Allocate or resize the line list buffer.
	if (PL_LLBP(pl) == NULL)
	    call malloc (PL_LLBP(pl), PL_LLLEN(pl), TY_SHORT)
	else if (PL_LLLEN(pl) != o_lllen)
	    call realloc (PL_LLBP(pl), PL_LLLEN(pl), TY_SHORT)

	# Read the stored line list.
	ip = ip + sz_index
	call miiupk16 (Mems[ip], LL(pl,0), PL_LLLEN(pl), TY_SHORT)

	# Update the remaining fields of the mask descriptor.
	PL_LLFREE(pl) = 0
	PL_LLNUPDATES(pl) = 0
	PL_LLINC(pl) = PL_STARTINC
	call amovki (1, PL_PLANE(pl,1), PL_MAXDIM)

	call sfree (sp)
end
