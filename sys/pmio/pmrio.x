# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

.help PMRIO
.nf ---------------------------------------------------------------------------
PMRIO -- A small package used to provide a means for efficient random
sampling (at the pixel level) of large PMIO masks.  In other words, if we have
a large mask and want to determine the values of successive mask pixels at
random locations in the mask, this package provides a more efficient means
for doing so than calling a routine such as PM_GLPI.  The mask must already
exist; means are not provided within this package for creating or editing
masks, only for reading them.

		 pmr = pmr_open (pm, plane, buflimit)
		    pmr_setrect (pmr, x1,y1, x2,y2)
	      mval = pmr_getpix (pmr, x, y)
		      pmr_close (pmr)

PMR_OPEN opens the indicated 2 dimensional plane of the N dimensional mask PM.
Buffer space used to provide an efficient means of randomly sampling the mask
will be kept to within approximately BUFLIMIT integer units of storage (the
internal table used to sample the mask is type integer, so BUFLIMIT is the
approximate number of entries in the table).  Random sampling of the mask is
provided by the integer function PMR_GETPIX, which returns the mask value at
the point [i,j] within the specified plane.  PMR_SETRECT may be called before
calling PMR_GETPIX to set the clipping rectangle, which defaults to the
boundaries of the mask.  If a PMR_GETPIX call references outside the clipping
region, ERR will be returned as the mask value (normal mask values are >= 0).
Use of a clipping region other than the boundaries of the full mask can avoid
the need for redundant clipping operations in the client.  PMR_CLOSE should
be called to free the PMRIO table space (which can be extensive) when no longer
needed.

This package is a front end to the PLRIO package in PLIO, which does all the
real work.
.endhelp ----------------------------------------------------------------------

# The following definitions must agree with those in plio$plrio.x.
define	PMR_PL		Memi[$1]		# backpointer to PLIO descriptor
define	PMR_PLANE	Memi[$1+10+($2)-1]	# defines 2D plane in ND mask


# PMR_OPEN -- Open a PMIO mask for random pixel access.  Provides efficient
# random pixel level access to any size mask.  This is a 2-dimensional
# operator, but can be used to sample any 2-dim plane of an N-dim mask.

pointer procedure pmr_open (pl, plane, buflimit)

pointer	pl			#I PMIO/PLIO descriptor
int	plane[ARB]		#I 2-dim plane to be accessed
int	buflimit		#I approximate table size, or 0 if don't care

pointer	plr_open()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call imaplv (PM_REFIM(pl), plane, v1, PM_MAXDIM)
	    return (plr_open (pl, v1, buflimit))
	} else
	    return (plr_open (pl, plane, buflimit))
end


# PMR_GETPIX -- Return the value of the given mask pixel, identified by the
# 2-dim coordinates of the pixel relative to the plane of the N-dim mask
# specified at open time.

int procedure pmr_getpix (pmr, i, j)

pointer	pmr			#I PMR descriptor
int	i, j			#I plane-relative coordinates of pixel

pointer	pl
int	plr_getpix()
include	"pmio.com"

begin
	pl = PMR_PL(pmr)
	if (PM_MAPXY(pl) == YES) {
	    PMR_PLANE(pmr,1) = i
	    PMR_PLANE(pmr,2) = j
	    call imaplv (PM_REFIM(pl), PMR_PLANE(pmr,1), v1, PM_MAXDIM)
	    return (plr_getpix (pmr, v1[1], v1[2]))
	} else
	    return (plr_getpix (pmr, i, j))
end


# PMR_SETRECT -- Set the clipping region for PMR_GETPIX.

procedure pmr_setrect (pmr, x1,y1, x2,y2)

pointer	pmr			#I PMR descriptor
int	x1,y1			#I lower left corner of region
int	x2,y2			#I upper right corner of region

pointer	pl
include	"pmio.com"

begin
	pl = PMR_PL(pmr)
        if (PM_MAPXY(pl) == YES) {
	    call amovi (PMR_PLANE(pmr,1), v1, PM_MAXDIM)
            v1[1] = x1;  v1[2] = y1
            call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    call amovi (PMR_PLANE(pmr,1), v3, PM_MAXDIM)
            v3[1] = x2;  v3[2] = y2
            call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    call plr_setrect (pmr, v2[1],v2[2], v4[1],v4[2])
	} else
	    call plr_setrect (pmr, x1,y1, x2,y2)
end


# PMR_CLOSE -- Free a PMRIO descriptor.

procedure pmr_close (pmr)

pointer	pmr			#I PMR descriptor

begin
	call plr_close (pmr)
end
