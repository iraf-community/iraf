include	<imhdr.h>
include <mach.h>
include	"../lib/allstardef.h"
include "../lib/daophotdef.h"

define	FUDGE	1.2		# fudge factor for memory allocation

# DP_CACHE -- Determine whether it is possible to store all the data
# in memory or not.

procedure dp_cache (dao, im, subim, cache)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
pointer	subim			# pointer to the output subtracted image
int	cache			# cache the data ?

int	npix, req_mem1, req_mem2, req_mem3, old_mem, max_mem
pointer	sp, temp, allstar, data, weights, subt
int	sizeof(), begmem()
pointer	immap()
errchk	begmem(), calloc()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	# Define the allstar pointer.
	allstar = DP_ALLSTAR(dao)

	# Store the old working set size.
	DP_SZOLDCACHE(allstar) = begmem (0, old_mem, max_mem)

	# Figure out the memory requirements.
	npix = IM_LEN(im,1) * IM_LEN(im,2)
	req_mem1 = FUDGE *  npix * sizeof (TY_REAL)
	req_mem2 = req_mem1 + req_mem1
	req_mem3 = req_mem2 + req_mem1

	# Initialize.
	DP_CACHE (allstar,A_DCOPY) = NO
	DP_CACHE (allstar,A_SUBT) = NO
	DP_CACHE (allstar,A_WEIGHT) = NO

	DP_DBUF(allstar) = NULL
	DP_SBUF(allstar) = NULL
	DP_WBUF(allstar) = NULL

	# Use IRAF images as temporary storage space.
	if (cache == NO) {

	    # The output subtracted image.
	    DP_DATA (allstar) = subim

	    # The scratch image.
	    call mktemp ("subt", Memc[temp], SZ_FNAME)
	    DP_SUBT (allstar) = immap (Memc[temp], NEW_COPY, im)
	    IM_NDIM(DP_SUBT(allstar)) = 2
	    IM_PIXTYPE(DP_SUBT(allstar)) = TY_REAL

	    # The weights image.
	    call mktemp ("wt", Memc[temp], SZ_FNAME)
	    DP_WEIGHTS (allstar) = immap (Memc[temp], NEW_COPY, im)
	    IM_NDIM(DP_WEIGHTS(allstar)) = 2
	    IM_PIXTYPE(DP_WEIGHTS(allstar)) = TY_REAL

	    # Set the type of caching to be used.
	    DP_ISCACHE(allstar) = NO
	    DP_ALLCACHE(allstar) = NO

	} else {

	    # Is the memory available.
	    if (old_mem >= req_mem3) {
		DP_CACHE(allstar,A_DCOPY) = YES
		DP_CACHE(allstar,A_WEIGHT) = YES
	        DP_CACHE (allstar,A_SUBT) = YES
	    } else {
	        if (begmem (req_mem1, old_mem, max_mem) >= req_mem1)
		    DP_CACHE(allstar,A_SUBT) = YES
	        if (begmem (req_mem2, old_mem, max_mem) >= req_mem2)
		    DP_CACHE(allstar,A_WEIGHT) = YES
	        if (begmem (req_mem3, old_mem, max_mem) >= req_mem3)
		    DP_CACHE(allstar,A_DCOPY) = YES
	    }

	    # Allocate space for the scratch image.
	    subt = NULL
	    if (DP_CACHE(allstar, A_SUBT) == YES) {
		iferr {
	            call calloc (subt, npix, TY_REAL)
		} then {
		    if (subt != NULL)
		        call mfree (subt, TY_REAL)
		    call mktemp ("subt", Memc[temp], SZ_FNAME)
		    subt = immap (Memc[temp], NEW_COPY, im)
		    IM_NDIM(subt) = 2
		    IM_PIXTYPE(subt) = TY_REAL
		    DP_SUBT(allstar) = subt
		    DP_CACHE(allstar,A_SUBT) = NO
		} else
		    DP_SUBT(allstar) = subt
	    } else {
		call mktemp ("subt", Memc[temp], SZ_FNAME)
		subt = immap (Memc[temp], NEW_COPY, im)
		IM_NDIM(subt) = 2
		IM_PIXTYPE(subt) = TY_REAL
		DP_SUBT(allstar) = subt
	    }

	    # Allocate space for the weights image.
	    weights = NULL
	    if (DP_CACHE(allstar, A_WEIGHT) == YES) {
		iferr {
	            call calloc (weights, npix, TY_REAL)
		} then {
		    if (weights != NULL)
		        call mfree (weights, TY_REAL)
		    call mktemp ("wt", Memc[temp], SZ_FNAME)
		    weights = immap (Memc[temp], NEW_COPY, im)
		    IM_NDIM(weights) = 2
		    IM_PIXTYPE(weights) = TY_REAL
		    DP_WEIGHTS(allstar) = weights
		    DP_CACHE(allstar,A_WEIGHT) = NO
		} else
		    DP_WEIGHTS(allstar) = weights
	    } else {
		call mktemp ("wt", Memc[temp], SZ_FNAME)
		weights = immap (Memc[temp], NEW_COPY, im)
		IM_NDIM(weights) = 2
		IM_PIXTYPE(weights) = TY_REAL
		DP_WEIGHTS(allstar) = weights
	    }

	    # Allocate space for the output subtracted image.
	    data = NULL
	    if (DP_CACHE(allstar, A_DCOPY) == YES) {
		iferr {
	            call calloc (data, npix, TY_REAL)
		} then {
		    if (data != NULL)
		        call mfree (data, TY_REAL)
		    DP_DATA(allstar) = subim
		    DP_CACHE(allstar,A_DCOPY) = NO
		} else {
		    DP_DATA(allstar) = data
		}
	    } else
		DP_DATA(allstar) = subim

	    # Set the type of caching to be used.
	    if (DP_CACHE(allstar,A_DCOPY) == NO && DP_CACHE(allstar,A_SUBT) ==
		NO && DP_CACHE(allstar,A_WEIGHT) == NO) {
		DP_ISCACHE(allstar) = NO
		DP_ALLCACHE(allstar) = NO
	    } else if (DP_CACHE(allstar,A_DCOPY) == YES && DP_CACHE(allstar,
	        A_SUBT) == YES && DP_CACHE(allstar,A_WEIGHT) == YES) {
		DP_ISCACHE(allstar) = YES
		DP_ALLCACHE(allstar) = YES
	    } else {
		DP_ISCACHE(allstar) = YES
		DP_ALLCACHE(allstar) = NO
	    }

	}

	call sfree (sp)
end


# DP_UNCACHE -- Release all the stored memory.

procedure dp_uncache (dao, subim, savesub)

pointer	dao		# pointer to the daophot strucuture
pointer	subim		# pointer to the output subtracted image
int	savesub		# save the subtracted image ?

int	j, ncol, nline
pointer	sp, imname, v, data, buf, allstar
pointer	impnlr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Define the allstar pointer.
	allstar = DP_ALLSTAR(dao)

	ncol = IM_LEN(subim,1)
	nline = IM_LEN(subim,2)

	# Write out the subtracted image.
	if (DP_CACHE(allstar,A_DCOPY) == YES && savesub == YES) {
	    data = DP_DBUF(allstar)
	    call amovkl (long(1), Meml[v], IM_MAXDIM)
	    do j = 1, nline {
		if (impnlr (subim, buf, Meml[v]) != EOF)
		    call amovr (Memr[data], Memr[buf], ncol)
	        data = data + ncol
	    }
	}
	DP_DATA(allstar) = NULL

	# Release any memory used by the subtracted image.
	if (DP_DBUF(allstar) != NULL)
	    call mfree (DP_DBUF(allstar), TY_REAL)
	DP_DBUF(allstar) = NULL

	# Delete the scratch image if any.
	if (DP_CACHE(allstar,A_SUBT) == NO) {
	    call strcpy (IM_HDRFILE(DP_SUBT(allstar)), Memc[imname], SZ_FNAME)
	    call imunmap (DP_SUBT(allstar)) 
	    call imdelete (Memc[imname])
	}
	DP_SUBT(allstar) = NULL

	# Release any memory used by the scratch image.
	if (DP_SBUF(allstar) != NULL)
	    call mfree (DP_SBUF(allstar), TY_REAL)
	DP_SBUF(allstar) = NULL

	# Delete the weights image if any.
	if (DP_CACHE(allstar,A_WEIGHT) == NO) {
	    call strcpy (IM_HDRFILE(DP_WEIGHTS(allstar)), Memc[imname],
	        SZ_FNAME)
	    call imunmap (DP_WEIGHTS(allstar)) 
	    call imdelete (Memc[imname])
	}
	DP_WEIGHTS(allstar) = NULL

	# Release any memory used by the weights buffer.
	if (DP_WBUF(allstar) != NULL)
	    call mfree (DP_WBUF(allstar), TY_REAL)
	DP_WBUF(allstar) = NULL

	# Reset the working set size to the previous value.
	call fixmem (DP_SZOLDCACHE(allstar))

	call sfree (sp)
end
