# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plio.h>

.help PLRIO
.nf ---------------------------------------------------------------------------
PLRIO -- A small package used to provide a means for efficient random
sampling (at the pixel level) of large PLIO masks.  In other words, if we have
a large mask and want to determine the values of successive mask pixels at
random locations in the mask, this package provides a more efficient means
for doing so than calling a routine such as PL_GLPI.  The mask must already
exist; means are not provided within this package for creating or editing
masks, only for reading them.

		 plr = plr_open (pl, plane, buflimit)
		    plr_setrect (plr, x1,y1, x2,y2)
	      mval = plr_getpix (plr, x, y)
		     plr_getlut (plr, bufp, xsize,ysize, xblock,yblock)
		      plr_close (plr)

PLR_OPEN opens the indicated 2 dimensional plane of the N dimensional mask PL.
Buffer space used to provide an efficient means of randomly sampling the mask
will be kept to within approximately BUFLIMIT integer units of storage (the
internal table used to sample the mask is type integer, so BUFLIMIT is the
approximate number of entries in the table).  Random sampling of the mask is
provided by the integer function PLR_GETPIX, which returns the mask value at
the point [i,j] within the specified plane.  PLR_SETRECT may be called before
calling PLR_GETPIX to set the clipping rectangle, which defaults to the
boundaries of the mask.  If a PLR_GETPIX call references outside the clipping
region, ERR will be returned as the mask value (normal mask values are >= 0).
Use of a clipping region other than the boundaries of the full mask can avoid
the need for redundant clipping operations in the client.  For cases when
even the function call overhead of PLR_GETPIX is too much, the lookup table
may be directly accessed via a call to PLR_GETLUT.  Table references which
resolve to a negative valued table entry should call PLR_GETPIX to get the
mask value, otherwise the table value is the mask value.  PLR_CLOSE should
be called to free the PLRIO table space (which can be extensive) when no longer
needed.
.endhelp ----------------------------------------------------------------------

define	DEF_BUFLIMIT	(128*128)		# internal buffer size
define	LEN_STACK	(4*32)			# max mask size = 2**LEN_STACK

# If any of the following are changed check that pmio$pmrio.x is consistent.
define	LEN_PLRDES	20
define	PLR_PL		Memi[$1]		# main PLIO descriptor
define	PLR_NCOLS	Memi[$1+1]		# table width
define	PLR_NLINES	Memi[$1+2]		# table height
define	PLR_XBLOCK	Memi[$1+3]		# table blocking factor, X
define	PLR_YBLOCK	Memi[$1+4]		# table blocking factor, Y
define	PLR_BUFP	Memi[$1+5]		# buffer pointer
define	PLR_X1		Memi[$1+6]		# clipping rectangle
define	PLR_Y1		Memi[$1+7]		# clipping rectangle
define	PLR_X2		Memi[$1+8]		# clipping rectangle
define	PLR_Y2		Memi[$1+9]		# clipping rectangle
define	PLR_PLANE	Memi[$1+10+($2)-1]	# plane to be accessed

define	COMPLEX		-1			# table bin -> compex region
define	LEN_REGDES	4			# region descriptor
define	V1		Memi[$1+($2)-1]
define	V2		Memi[$1+2+($2)-1]


# PLR_OPEN -- Open a PLIO mask for random pixel access.  Provides efficient
# random pixel level access to any size mask.  This is a 2-dimensional
# operator, but can be used to sample any 2-dim plane of an N-dim mask.

pointer procedure plr_open (pl, plane, buflimit)

pointer	pl			#I PLIO descriptor
int	plane[ARB]		#I 2-dim plane to be accessed
int	buflimit		#I approximate table size, or 0 if don't care

int	v1[PL_MAXDIM], v2[PL_MAXDIM]
int	maxpix, ndim, npix, mval, i, j
int	msize[2], tsize[2], block[2], vm[2]
pointer	sp, stack, plr, bufp, el, rp
errchk	calloc, malloc, plvalid
bool	pl_sectnotconst()

begin
	call plvalid (pl)
	call smark (sp)
	call salloc (stack, LEN_STACK * LEN_REGDES, TY_STRUCT)

	# Allocate the PLRIO descriptor.
	call calloc (plr, LEN_PLRDES, TY_STRUCT)

	# Set the plane to be accessed.
	ndim = PL_NAXES(pl)
	do i = 1, 2
	    msize[i] = PL_AXLEN(pl,i)

	do i = 1, PL_MAXDIM
	    if (i > ndim) {
		PLR_PLANE(pl,i) = 1
		v1[i] = 1
		v2[i] = 1
	    } else if (i > 2) {
		PLR_PLANE(pl,i) = plane[i]
		v1[i] = plane[i]
		v2[i] = plane[i]
	    }

	# Get the maximum table size in pixels.
	if (buflimit <= 0)
	    maxpix = DEF_BUFLIMIT
	else
	    maxpix = buflimit

	# Determine the blocking factors required to keep the lookup table
	# within the given size limit.

	block[1] = 1;  block[2] = 1
	while ((msize[1] / block[1]) * (msize[2] / block[2]) > maxpix)
	    do i = 1, 2
		block[i] = min (msize[i], block[i]*2)

	# Compute the lookup table size.
	do i = 1, 2
	    tsize[i]  = (msize[i] + block[i]-1) / block[i]

	# Allocate the table space.
	call malloc (bufp, tsize[1] * tsize[2], TY_INT)

	# Compute the lookup table.  Since the lookup table can be large,
	# e.g., a quarter million elements for a 512sq table, we don't want
	# to directly compute the value of each bufp[i,j].  Instead, we examine
	# a region of the table, starting with the entire table, and if the
	# corresponding region of the mask is not filled with the same mask
	# value, we divide the region into 4 quadrants and examine each in
	# turn, and so on until the nonconstant regions are the size of one
	# table bin (pixel), which we conclude maps to a COMPLEX (nonconstant)
	# region of the mask.  By this technique, only table bins which map
	# to complex mask regions need be evaluated, and entire large regions
	# of the mask are quickly dealt with.

	# Push the entire mask area on the stack as the first region.
	el = stack
	do i = 1, 2 {
	    V1(el,i) = 1
	    V2(el,i) = tsize[i]
	}

	repeat {
	    # Get the mask coordinates of the next region on the stack.
	    do i = 1, 2 {
		v1[i] = (V1(el,i) - 1) * block[i] + 1
		v2[i] = min (msize[i], V2(el,i) * block[i])
	    }

	    # Examine the region to see if the associated region of the mask
	    # consists entirely of a single mask value.

	    if (pl_sectnotconst (pl, v1, v2, ndim, mval)) {
		if (V1(el,1) == V2(el,1) && V1(el,2) == V2(el,2)) {
		    # This single table pixel maps to a complex mask region.
		    Memi[bufp+(V1(el,2)-1)*tsize[1]+V1(el,1)-1] = COMPLEX

		} else {
		    # Divide the nonzero mask region into four quadrants
		    # and recursively examine each in turn.

		    # Compute the coordinates of the central pixel in vm.
		    do i = 1, 2
			vm[i] = (V1(el,i) + V2(el,i) + 1) / 2

		    # Save the currently stacked region in v1/v2.
		    v1[1] = V1(el,1);  v1[2] = V1(el,2)
		    v2[1] = V2(el,1);  v2[2] = V2(el,2)

		    if ((el-stack)/LEN_REGDES+4 >= LEN_STACK)
			call syserrs (SYS_PLSTKOVFL, "plr_open")

		    # Push the four quadrants of this region on the stack.
		    # If the region we are subdividing is only one pixel
		    # wide in either axis then only two of the regions will
		    # be valid.  The invalid regions will have zero pixels
		    # in one axis or the other, i.e. (v2[i] < v1[i]).  If
		    # a region is invalid discard it by not advancing the
		    # stack pointer.

		    V1(el,1) = v1[1];    V1(el,2) = vm[2]
		    V2(el,1) = vm[1]-1;  V2(el,2) = v2[2]
		    if (V1(el,1) <= V2(el,1) && V1(el,2) <= V2(el,2))
			el = el + LEN_REGDES

		    V1(el,1) = vm[1];    V1(el,2) = vm[2]
		    V2(el,1) = v2[1];    V2(el,2) = v2[2]
		    if (V1(el,1) <= V2(el,1) && V1(el,2) <= V2(el,2))
			el = el + LEN_REGDES
		    
		    V1(el,1) = v1[1];    V1(el,2) = v1[2]
		    V2(el,1) = vm[1]-1;  V2(el,2) = vm[2]-1
		    if (V1(el,1) <= V2(el,1) && V1(el,2) <= V2(el,2))
			el = el + LEN_REGDES

		    V1(el,1) = vm[1];    V1(el,2) = v1[2]
		    V2(el,1) = v2[1];    V2(el,2) = vm[2]-1
		    if (V1(el,1) <= V2(el,1) && V1(el,2) <= V2(el,2))
			el = el + LEN_REGDES
		}
	    } else {
		# Set entire region to a constant mask value.
		npix = V2(el,1) - V1(el,1) + 1
		do j = V1(el,2), V2(el,2) {
		    rp = bufp + (j-1) * tsize[1] + V1(el,1) - 1
		    if (npix == 1) {
			Memi[rp] = mval
		    } else if (npix < 8) {
			do i = 1, npix
			    Memi[rp+i-1] = mval
		    } else {
			if (mval == 0)
			    call aclri (Memi[rp], npix)
			else
			    call amovki (mval, Memi[rp], npix)
		    }
		}
	    }

	    # Pop stack.
	    el = el - LEN_REGDES

	} until (el < stack)

	# Initialize the PLRIO descriptor.
	PLR_PL(plr) = pl
	PLR_NCOLS(plr) = tsize[1]
	PLR_NLINES(plr) = tsize[2]
	PLR_XBLOCK(plr) = block[1]
	PLR_YBLOCK(plr) = block[2]
	PLR_BUFP(plr) = bufp
	PLR_X1(plr) = 1
	PLR_Y1(plr) = 1
	PLR_X2(plr) = msize[1]
	PLR_Y2(plr) = msize[2]

	call sfree (sp)
	return (plr)
end


# PLR_GETPIX -- Return the value of the given mask pixel, identified by the
# 2-dim coordinates of the pixel relative to the plane of the N-dim mask
# specified at open time.

int procedure plr_getpix (plr, i, j)

pointer	plr			#I PLR descriptor
int	i, j			#I plane-relative coordinates of pixel

pointer	pl, ll_src
int	ii, jj, mval, np
pointer	pl_access()
int	pl_l2pi()
errchk	pl_access

begin
	# Clip to the specified region of the mask.
	if (i < PLR_X1(plr) || i > PLR_X2(plr))
	    return (ERR)
	if (j < PLR_Y1(plr) || j > PLR_Y2(plr))
	    return (ERR)

	# Map mask pixel coordinates to lookup table bin.
	ii = (i - 1) / PLR_XBLOCK(plr)
	jj = (j - 1) / PLR_YBLOCK(plr)

	# Get the lookup table value of the given bin.
	mval = Memi[PLR_BUFP(plr)+jj*PLR_NCOLS(plr)+ii]

	# Access the original mask to get value if complex region.
	if (mval == COMPLEX) {
	    pl = PLR_PL(plr)
	    PLR_PLANE(plr,2) = j
	    ll_src = pl_access (pl, PLR_PLANE(plr,1))
	    np = pl_l2pi (Mems[ll_src], i, mval, 1)
	}

	return (mval)
end


# PLR_GETLUT -- Obtain the buffer pointer and scaling information of the
# internal lookup table, so that direct table references may be made to
# minimize overhead in particularly demanding applications.  This is not
# recommended unless absolutely necessary, as PLR_GETPIX is easier and
# safer to use and nearly as efficient.  The strategy for using the table
# is to use the blocking factors and XSIZE to map a 2dim mask coordinate
# into a table offset, and access the table to get the table value.
# If this is negative PLR_GETPIX should be called to compute the mask
# value, else the table value is the mask value.

procedure plr_getlut (plr, bufp, xsize,ysize, xblock,yblock)

pointer	plr			#I PLR descriptor
pointer	bufp			#O lookup table buffer pointer (int *)
int	xsize,ysize		#O table size
int	xblock,yblock		#O blocking factors

begin
	bufp = PLR_BUFP(plr)
	xsize = PLR_NCOLS(plr)
	ysize = PLR_NLINES(plr)
	xblock = PLR_XBLOCK(plr)
	yblock = PLR_YBLOCK(plr)
end


# PLR_SETRECT -- Set the clipping region for PLR_GETPIX.

procedure plr_setrect (plr, x1,y1, x2,y2)

pointer	plr			#I PLR descriptor
int	x1,y1			#I lower left corner of region
int	x2,y2			#I upper right corner of region

pointer	pl
define	oob_ 91
errchk	syserrs

begin
	pl = PLR_PL(plr)

	if (x1 < 1 || x1 > PL_AXLEN(pl,1))
	    goto oob_
	if (x2 < 1 || x2 > PL_AXLEN(pl,1))
	    goto oob_
	if (y1 < 1 || y1 > PL_AXLEN(pl,2))
	    goto oob_
	if (y2 < 1 || y2 > PL_AXLEN(pl,2))
oob_	    call syserrs (SYS_PLREFOOB, "plr_setrect")

	PLR_X1(plr) = x1;  PLR_Y1(plr) = y1
	PLR_X2(plr) = x2;  PLR_Y2(plr) = y2
end


# PLR_CLOSE -- Free a PLRIO descriptor.

procedure plr_close (plr)

pointer	plr			#I PLR descriptor

begin
	call mfree (PLR_BUFP(plr), TY_INT)
	call mfree (plr, TY_STRUCT)
end
