# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PL_SSIZE -- Set the size of a mask, i.e., given an existing open mask
# descriptor, create an empty mask with the given dimensionality and size.

procedure pl_ssize (pl, naxes, axlen, depth)

pointer	pl			#I mask descriptor
int	naxes			#I number of axes (dimensionality of mask)
long	axlen[ARB]		#I length of each axis
int	depth			#I mask depth, bits

size_t	sz_val
long	lval
size_t	npix
int	i
pointer	sp, px, lp
int	pl_p2ls(), modi()
errchk	malloc, calloc, mfree

begin
	npix = axlen[1]
	call smark (sp)
	call salloc (px, npix, TY_SHORT)

	# Initialize the old descriptor.
	if (PL_LPP(pl) != NULL)
	    call mfree (PL_LPP(pl), TY_INT)
	if (PL_LLBP(pl) != NULL)
	    call mfree (PL_LLBP(pl), TY_SHORT)
	sz_val = PL_MAXDIM
	lval = 1
	call amovkl (lval, PL_PLANE(pl,1), sz_val)

	# Set up the empty descriptor.
	PL_NAXES(pl) = naxes
	if (depth > 0) {
	    PL_MAXVAL(pl) = MV(depth)
	} else {
	    PL_MAXVAL(pl) = MV(PL_MAXDEPTH)
	}
	sz_val = naxes
	call amovl (axlen, PL_AXLEN(pl,1), sz_val)
	do i = naxes + 1, PL_MAXDIM
	    PL_AXLEN(pl,i) = 1

	# Allocate the line list buffer.
	PL_MAXLINE(pl) = (axlen[1] * 3) + LL_CURHDRLEN
	PL_LLLEN(pl) = PL_LLBUFLEN
	sz_val = PL_LLBUFLEN
	call malloc (PL_LLBP(pl), sz_val, TY_SHORT)
	lp = PL_LLBP(pl)

	# Encode the empty line line-list.
	call aclrs (Mems[px], npix)
	lval = 1
	PL_LLOP(pl) = pl_p2ls (Mems[px], lval, Mems[lp], npix)

	# Set up the initial line list index (all lines empty).
	PL_NLP(pl) = 1
	do i = 2, naxes
	    PL_NLP(pl) = PL_NLP(pl) * axlen[i]
	call calloc (PL_LPP(pl), PL_NLP(pl), TY_INT)

	# Set up the LL header for the empty line.
	LP_NREFS(lp) = PL_NLP(pl)
	LP_SETBLEN(lp, PL_LLOP(pl))

	call sfree (sp)
end
