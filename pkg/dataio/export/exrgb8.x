include <imhdr.h>
include "export.h"


# Size definitions
define	A_BITS		8		# Number of bits of color
define	B_BITS		5		# Number of bits/pixel to use
define	C_BITS		3		# Number of cells/color to use
define	A_LEN		256		# 2 ** A_BITS
define	B_LEN		32		# 2 ** B_BITS
define	C_LEN		8		# 2 ** C_BITS
define  AB_SHIFT	8		# 2 ** (A_BITS - B_BITS)
define	BC_SHIFT	4		# 2 ** (B_BITS - C_BITS)
define	AC_SHIFT	32		# 2 ** (A_BITS - C_BITS)

# Color metric definitions
define	R2FACT		20		# .300 * .300 * 256 = 23
define	G2FACT		39		# .586 * .586 * 256 = 88
define	B2FACT		8		# .114 * .114 * 256 =  3

define	RED		1
define	GREEN		2
define	BLUE		3

# Colorbox structure
define	CBOX_LEN	9
define	CBOX_NEXT	Memi[$1]	# pointer to next colorbox structure
define	CBOX_PREV	Memi[$1+1]	# pointer to previous colorbox structure
define	CBOX_RMIN	Memi[$1+2]
define	CBOX_RMAX	Memi[$1+3]
define	CBOX_GMIN	Memi[$1+4]
define	CBOX_GMAX	Memi[$1+5]
define	CBOX_BMIN	Memi[$1+6]
define	CBOX_BMAX	Memi[$1+7]
define	CBOX_TOTAL	Memi[$1+8]

# Color cell structure
define	CCELL_LEN	(A_LEN*2+1)
define	CCELL_NUM_ENTS	Memi[$1]
define	CCELL_ENTRIES	Memi[$1+2*($2)+$3+1]

# Output number of colors
define	NCOLORS		256


# EX_MKCMAP -- Generate an 8-bit colormap from three input image expressions
# using Heckbert's Median Cut algorithm.  The implementation of this algorithm
# was modeled, with permission, on that in the program XV written by John 
# Bradley.

procedure ex_mkcmap (ex)

pointer	ex				#i task struct pointer

pointer	oim				# Output image
real	z1[3], dz[3]			# Display ranges

int	i, ncolors
pointer	sp, cmap, box_list, histogram, ColorCells
pointer freeboxes, usedboxes, ptr, im

pointer	immap(), cm_largest_box()
errchk	open, immap

begin
	# Since we're creating a colormap we force the output pixel size
	# to be 8-bits.
	call ex_do_outtype (ex, "b1")

	# Create a temporary image of the processed expressions.  We'll
	# evaluate the expressions only once an save the results, later
	# we'll path up the operand and expressions structs to it copies 
	# this out to the requested format.

	if (EX_TIMPTR(ex) == NULL)
	    call calloc (EX_TIMPTR(ex), SZ_FNAME, TY_CHAR)
	else
	    call aclrc (TIMNAME(ex), SZ_FNAME)
	call mktemp ("tmp$ex", TIMNAME(ex), SZ_FNAME)
	oim = immap (TIMNAME(ex), NEW_IMAGE, 0)
	IM_PIXTYPE(oim) = TY_SHORT
	IM_LEN(oim,1) = EX_OCOLS(ex)
	IM_LEN(oim,2) = EX_OROWS(ex)
	IM_NDIM(oim) = 2

	# Set input image intensity scaling.
	z1[1] = 0.0
	dz[1] = 1.0
	z1[2] = 0.0
	dz[2] = 1.0
	z1[3] = 0.0
	dz[3] = 1.0

	# Allocate color map.
	ncolors = NCOLORS
	call smark (sp)
	call salloc (cmap, 3 * ncolors, TY_SHORT)

	# Allocate and initialize color boxes.
	call salloc (box_list, ncolors * CBOX_LEN, TY_STRUCT) 

	freeboxes = box_list
	usedboxes = NULL
	ptr = freeboxes
	CBOX_PREV(ptr) = NULL
	CBOX_NEXT(ptr) = ptr + CBOX_LEN
	for (i=2; i<ncolors; i=i+1) {
	    ptr = ptr + CBOX_LEN
	    CBOX_PREV(ptr) = ptr - CBOX_LEN
	    CBOX_NEXT(ptr) = ptr + CBOX_LEN
	}
	ptr = ptr + CBOX_LEN
	CBOX_PREV(ptr) = ptr - CBOX_LEN
	CBOX_NEXT(ptr) = NULL

	ptr = freeboxes
	freeboxes = CBOX_NEXT(ptr)
	if (freeboxes != NULL)
	    CBOX_PREV(freeboxes) = NULL

	CBOX_NEXT(ptr) = usedboxes
	usedboxes = ptr
	if (CBOX_NEXT(ptr) != NULL)
	    CBOX_PREV(CBOX_NEXT(ptr)) = ptr
	
	# Allocate and get histogram.
	if (EX_VERBOSE(ex) == YES) {
	    call printf ("Computing colormap....\n")
	    call flush (STDOUT)
	}
	call salloc (histogram, B_LEN*B_LEN*B_LEN, TY_INT)
	call aclri (Memi[histogram], B_LEN*B_LEN*B_LEN)
	call cm_get_histogram(ex, z1, dz, ptr, Memi[histogram])
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_CMAP)

	# Subdivide boxes until no more free boxes remain
	while (freeboxes != NULL) {
	    ptr = cm_largest_box (usedboxes)
	    if (ptr != NULL)
		call cm_splitbox (ptr, usedboxes, freeboxes, Memi[histogram])
	    else
		break
	}

	# Set color map and write it out.
	ptr = usedboxes
	for (i=0; i<ncolors && ptr!=NULL; i=i+1) {
	    call cm_assign_color (ptr, Mems[cmap+3*i])
	    ptr = CBOX_NEXT(ptr)
	}
	ncolors = i

	# Copy the colormap to the main task array.
	call cm_save_cmap (ex, Mems[cmap], ncolors)

	# Scan histogram and map all values to closest color.
	# First create cell list as described in Heckbert[2] and then
	# create mapping from truncated pixel space to color table entries

	call salloc (ColorCells, C_LEN*C_LEN*C_LEN, TY_POINTER)
	call aclri (Memi[ColorCells], C_LEN*C_LEN*C_LEN)
	call cm_map_colortable (Memi[histogram], Mems[cmap], ncolors,
	    Memi[ColorCells])

	# Scan image and match input values to table entries.
	# Apply Floyd-Steinberg dithering.

	if (EX_VERBOSE(ex) == YES) {
	    call printf ("Computing color indices....\n")
	    call flush (STDOUT)
	}
	call cm_quant_fsdither (ex, z1, dz, Memi[histogram],
	    Memi[ColorCells], Mems[cmap], ncolors, oim)

        # Unmap the current image pointer(s).
        do i = 1, EX_NIMAGES(ex) {
            im = IO_IMPTR(IMOP(ex,i))
            if (im != NULL)
                call imunmap (im)
        }

	# Free the current operand and outbands pointers and fake up a new 
	# one that processes the temporary image.
        for (i=1; i < EX_NEXPR(ex); i=i+1)
            call ex_free_outbands (OBANDS(ex,i))
        for (i=1; i < EX_NIMOPS(ex); i=i+1)
            call ex_free_operand (IMOP(ex,i))
	call ex_do_outbands (ex, "b1")
	O_HEIGHT(ex,1) = EX_OROWS(ex)
	O_WIDTH(ex,1) = EX_OCOLS(ex)

	# Set the temp image as the only valid image and fudge the operands.
	EX_NIMAGES(ex) = 1
	EX_NEXPR(ex) = 1
	EX_NLINES(ex) = EX_OROWS(ex)
	IO_IMPTR(IMOP(ex,1)) = oim
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_BAND)
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), BAND_STORAGE)

	for (i=0; i < C_LEN*C_LEN*C_LEN; i=i+1) {
	    if (Memi[ColorCells+i] != NULL)
		call mfree (Memi[ColorCells+i], TY_STRUCT)
	}

	call sfree (sp)
end


# CM_SAVE_CMAP -- Save color map for to main structure.

procedure cm_save_cmap (ex, map, ncolors)

pointer	ex				#i task struct pointer
short	map[3,ncolors]			#i Color map
int	ncolors				#i Number of colors

int	i
pointer	cmap

begin
        # Allocate the colormap pointer and read the colormap.
        iferr (call calloc (EX_CMAP(ex), 3*CMAP_SIZE, TY_CHAR))
            call error (0, "Error allocating colormap pointer.")
	cmap = EX_CMAP(ex)

	for (i=1; i<=min(ncolors,256); i=i+1) {
	    CMAP(cmap,EX_RED,i)   = (map[1,i] + 0.5)
	    CMAP(cmap,EX_GREEN,i) = (map[2,i] + 0.5)
	    CMAP(cmap,EX_BLUE,i)  = (map[3,i] + 0.5)
	}
	for (; i<=256; i=i+1) {
	    CMAP(cmap,EX_RED,i)   = 0
	    CMAP(cmap,EX_GREEN,i) = 0
	    CMAP(cmap,EX_BLUE,i)  = 0
	}
end


# CM_GETLINE -- Get a line of intensity mapped input data.

procedure cm_getline (ex, z1, dz, line, data)

pointer	ex				#I task struct pointer
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
int	line				#I Line to be obtained
pointer	data				#O Intensity mapped data

int	i, j, nc, lnum
pointer	iptr, optr, bptr, op

pointer	ex_evaluate(), ex_chtype()

begin
        # See if we're flipping the image.
        if (bitset (EX_OUTFLAGS(ex), OF_FLIPY))
            lnum = EX_NLINES(ex) - line + 1
        else
            lnum = line

	# Get the pixels.
	call ex_getpix (ex, lnum)

	nc = EX_OCOLS(ex)
	call malloc (iptr, nc, TY_SHORT)
	do i = 1, 3 {
	    op = ex_evaluate (ex, O_EXPR(ex,i))
	    bptr = ex_chtype (ex, op, EX_OUTTYPE(ex))
	    call achtbs (Memc[bptr], Mems[iptr], nc)
	    call evvfree (op)

	    optr = data + i - 1
	    do j = 1, nc {
	        Memi[optr] = max (0, min (255, int (Mems[iptr+j-1])))
		optr = optr + 3
	    }

	    call mfree (bptr, TY_CHAR)
	}
	call mfree (iptr, TY_SHORT)
end


# CM_GET_HISTOGRAM -- Compute color histogram

procedure cm_get_histogram (ex, z1, dz, box, histogram)

pointer	ex				#I task struct pointer
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
pointer	box				#O Initial box
int	histogram[B_LEN,B_LEN,B_LEN]	#O Histogram

int	i, j, nc, nl, r, g, b, rmin, gmin, bmin, rmax, gmax, bmax
pointer	sp, data, ptr

begin
	nc = EX_OCOLS(ex)
	nl = EX_OROWS(ex)

	call smark (sp)
	call salloc (data, 3 * nc, TY_INT)

	rmin = A_LEN; rmax = -1 
	gmin = A_LEN; gmax = -1 
	bmin = A_LEN; bmax = -1 

	# calculate histogram
	do j = 1, nl {
	    call cm_getline (ex, z1, dz, j, data)
	    ptr = data
	    do i = 1, nc {
		r = Memi[ptr] / AB_SHIFT + 1
		g = Memi[ptr+1] / AB_SHIFT + 1
		b = Memi[ptr+2] / AB_SHIFT + 1
		ptr = ptr + 3

		histogram[r,g,b] = histogram[r,g,b] + 1

		rmin = min (rmin, r)
		rmax = max (rmax, r)
		gmin = min (gmin, g)
		gmax = max (gmax, g)
		bmin = min (bmin, b)
		bmax = max (bmax, b)
	    }
	}

	CBOX_RMIN(box) = rmin
	CBOX_GMIN(box) = gmin
	CBOX_BMIN(box) = bmin
	CBOX_RMAX(box) = rmax
	CBOX_GMAX(box) = gmax
	CBOX_BMAX(box) = bmax
	CBOX_TOTAL(box) = nc * nl

	call sfree (sp)
end



# CM_LARGEST_BOX -- Return pointer to largest box

pointer procedure cm_largest_box (usedboxes)

pointer	usedboxes		#I Pointer to used boxes

pointer	tmp, ptr
int	size

begin
	size = -1
	ptr = NULL

	for (tmp=usedboxes; tmp!=NULL; tmp=CBOX_NEXT(tmp)) {
	    if ((CBOX_RMAX(tmp) > CBOX_RMIN(tmp) ||
	         CBOX_GMAX(tmp) > CBOX_GMIN(tmp) ||
	         CBOX_BMAX(tmp) > CBOX_BMIN(tmp)) &&
		 CBOX_TOTAL(tmp) > size) {
		ptr = tmp
		size = CBOX_TOTAL(tmp)
	    }
	}
	return(ptr)
end


# CM_SPLITBOX -- Split a box along largest dimension

procedure cm_splitbox (box, usedboxes, freeboxes, histogram)

pointer	box					#U Box to split
pointer	usedboxes				#U Used boxes
pointer	freeboxes				#U Free boxes
int	histogram[B_LEN, B_LEN, B_LEN]		#I Histogram

int	first, last, i, j, rdel, gdel, bdel, sum1, sum2
pointer	sp, hist, new
int	ir, ig, ib
int	rmin, rmax, gmin, gmax, bmin, bmax
int	which

begin
	call smark (sp)
	call salloc (hist, B_LEN, TY_INT)

	# see which axis is the largest, do a histogram along that
	# axis.  Split at median point.  Contract both new boxes to
	# fit points and return

	first = 1; last = 1
	rmin = CBOX_RMIN(box); rmax = CBOX_RMAX(box)
	gmin = CBOX_GMIN(box); gmax = CBOX_GMAX(box)
	bmin = CBOX_BMIN(box); bmax = CBOX_BMAX(box)

	rdel = rmax - rmin
	gdel = gmax - gmin
	bdel = bmax - bmin

	if (rdel>=gdel && rdel>=bdel)
	    which = RED
	else if (gdel>=bdel)
	    which = GREEN
	else
	    which = BLUE

	# get histogram along longest axis
	switch (which) {
	case RED:
	    for (ir=rmin; ir<=rmax; ir=ir+1) {
		sum1 = 0
		for (ig=gmin; ig<=gmax; ig=ig+1) {
		    for (ib=bmin; ib<=bmax; ib=ib+1) {
			sum1 = sum1 + histogram[ir,ig,ib]
		    }
		}
		Memi[hist+ir-1] = sum1
	    }
	    first = rmin; last = rmax

	case GREEN:
	    for (ig=gmin; ig<=gmax; ig=ig+1) {
		sum1 = 0
		for (ir=rmin; ir<=rmax; ir=ir+1) {
		    for (ib=bmin; ib<=bmax; ib=ib+1) {
			sum1 = sum1 + histogram[ir,ig,ib]
		    }
		}
		Memi[hist+ig-1] = sum1
	    }
	    first = gmin; last = gmax

	case BLUE:
	    for (ib=bmin; ib<=bmax; ib=ib+1) {
		sum1 = 0
		for (ir=rmin; ir<=rmax; ir=ir+1) {
		    for (ig=gmin; ig<=gmax; ig=ig+1) {
			sum1 = sum1 + histogram[ir,ig,ib]
		    }
		}
		Memi[hist+ib-1] = sum1
	    }
	    first = bmin;  last = bmax
	}


	# find median point
	sum1 = 0
	sum2 = CBOX_TOTAL(box) / 2
	for (i=first; i<=last; i=i+1) {
	    sum1 = sum1 + Memi[hist+i-1]
	    if (sum1 >= sum2)
		break
	}
	if (i == first)
	    i = i + 1


	# Create new box, re-allocate points
	
	new = freeboxes
	freeboxes = CBOX_NEXT(new)
	if (freeboxes != NULL)
	    CBOX_PREV(freeboxes) = NULL
	if (usedboxes != NULL)
	    CBOX_PREV(usedboxes) = new
	CBOX_NEXT(new) = usedboxes
	usedboxes = new

	sum1 = 0
	sum2 = 0
	for (j = first; j < i; j=j+1)
	    sum1 = sum1 + Memi[hist+j-1]
	for (; j <= last; j=j+1)
	    sum2 = sum2 + Memi[hist+j-1]
	CBOX_TOTAL(new) = sum1
	CBOX_TOTAL(box) = sum2

	CBOX_RMIN(new) = rmin;  CBOX_RMAX(new) = rmax
	CBOX_GMIN(new) = gmin;  CBOX_GMAX(new) = gmax
	CBOX_BMIN(new) = bmin;  CBOX_BMAX(new) = bmax

	switch (which) {
	case RED:
	    CBOX_RMAX(new) = i-1;  CBOX_RMIN(box) = i
	case GREEN:
	    CBOX_GMAX(new) = i-1;  CBOX_GMIN(box) = i
	case BLUE:
	    CBOX_BMAX(new) = i-1;  CBOX_BMIN(box) = i
	}

	call cm_shrinkbox (new, histogram)
	call cm_shrinkbox (box, histogram)
	call sfree (sp)
end


# CM_SHRINKBOX -- Shrink box

procedure cm_shrinkbox (box, histogram)

pointer	box					#U Box
int	histogram[B_LEN,B_LEN,B_LEN]		#I Histogram

int	ir, ig, ib
int	rmin, rmax, gmin, gmax, bmin, bmax

define	have_rmin	11
define	have_rmax	12
define	have_gmin	13
define	have_gmax	14
define	have_bmin	15
define	have_bmax	16

begin

	rmin = CBOX_RMIN(box);  rmax = CBOX_RMAX(box)
	gmin = CBOX_GMIN(box);  gmax = CBOX_GMAX(box)
	bmin = CBOX_BMIN(box);  bmax = CBOX_BMAX(box)

	if (rmax > rmin) {
	    for (ir=rmin; ir<=rmax; ir=ir+1) {
		for (ig=gmin; ig<=gmax; ig=ig+1) {
		    for (ib=bmin; ib<=bmax; ib=ib+1) {
			if (histogram[ir,ig,ib] != 0) {
			    rmin = ir
			    CBOX_RMIN(box) = rmin
			    goto have_rmin
			}
		    }
		}
	    }

have_rmin
	    if (rmax > rmin) {
		for (ir=rmax; ir>=rmin; ir=ir-1) {
		    for (ig=gmin; ig<=gmax; ig=ig+1) {
			for (ib=bmin; ib<=bmax; ib=ib+1) {
			    if (histogram[ir,ig,ib] != 0) {
			        rmax = ir
			        CBOX_RMAX(box) = rmax
			        goto have_rmax
			    }
			}
		    }
		}
	    }
	}


have_rmax
	if (gmax > gmin) {
	    for (ig=gmin; ig<=gmax; ig=ig+1) {
		for (ir=rmin; ir<=rmax; ir=ir+1) {
		    for (ib=bmin; ib<=bmax; ib=ib+1) {
			if (histogram[ir,ig,ib] != 0) {
			    gmin = ig
			    CBOX_GMIN(box) = gmin
			    goto have_gmin
			}
		    }
		}
	    }

have_gmin
	    if (gmax > gmin) {
		for (ig=gmax; ig>=gmin; ig=ig-1) {
		    for (ir=rmin; ir<=rmax; ir=ir+1) {
			for (ib=bmin; ib<=bmax; ib=ib+1) {
			    if (histogram[ir,ig,ib] != 0) {
				gmax = ig
				CBOX_GMAX(box) = gmax
			 	goto have_gmax
			    }
			}
		    }
		}
	    }
	}

have_gmax
	if (bmax > bmin) {
	    for (ib=bmin; ib<=bmax; ib=ib+1) {
		for (ir=rmin; ir<=rmax; ir=ir+1) {
		    for (ig=gmin; ig<=gmax; ig=ig+1) {
			if (histogram[ir,ig,ib] != 0) {
			    bmin = ib
			    CBOX_BMIN(box) = bmin
			    goto have_bmin
			}
		    }
		}
	    }

have_bmin
	    if (bmax > bmin) {
		for (ib=bmax; ib>=bmin; ib=ib-1) {
		    for (ir=rmin; ir<=rmax; ir=ir+1) {
			for (ig=gmin; ig<=gmax; ig=ig+1) {
			    if (histogram[ir,ig,ib] != 0) {
				bmax = ib
				CBOX_BMAX(box) = bmax
				goto have_bmax
			    }
			}
		    }
		}
	    }
	}

have_bmax
	return
end



# CM_ASSIGN_COLOR -- Assign colors

procedure cm_assign_color (box, cmap)

pointer	box					#I Box
short	cmap[3]					#O Color map entry

begin
	# +1 ensures that color represents the middle of the box

	cmap[1] = ((CBOX_RMIN(box) + CBOX_RMAX(box) - 2) * AB_SHIFT) / 2
	cmap[2] = ((CBOX_GMIN(box) + CBOX_GMAX(box) - 2) * AB_SHIFT) / 2
	cmap[3] = ((CBOX_BMIN(box) + CBOX_BMAX(box) - 2) * AB_SHIFT) / 2
end



# CM_MAP_COLORTABLE -- Map the color table

procedure cm_map_colortable (histogram, cmap, ncolor, ColorCells)

int	histogram[B_LEN,B_LEN,B_LEN]		#U Histogram
short	cmap[3,ncolor]				#I Color map
int	ncolor					#I Number of colors
pointer	ColorCells[C_LEN,C_LEN,C_LEN]		#O Color cells

int	i, j, ir, ig, ib, rcell, bcell, gcell
long	dist, d2, tmp
pointer	cell, cm_create_colorcell()

begin
	for (ir=0; ir<B_LEN; ir=ir+1) {
	    rcell = 1 + ir / BC_SHIFT
	    for (ig=0; ig<B_LEN; ig=ig+1) {
		gcell = 1 + ig / BC_SHIFT
		for (ib=0; ib<B_LEN; ib=ib+1) {
		    bcell = 1 + ib / BC_SHIFT
		    if (histogram[1+ir,1+ig,1+ib]==0)
			histogram[1+ir,1+ig,1+ib] = -1
		    else {
			cell = ColorCells[rcell, gcell, bcell]
			    
			if (cell == NULL)
			    cell = cm_create_colorcell (ColorCells,
				ir*AB_SHIFT, ig*AB_SHIFT, ib*AB_SHIFT,
				cmap, ncolor)

			dist = 2000000000
			for (i=0; i<CCELL_NUM_ENTS(cell) &&
			    dist>CCELL_ENTRIES(cell,i,1); i=i+1) {
			    j = CCELL_ENTRIES(cell,i,0)
			    d2 = cmap[1,1+j] - (ir * BC_SHIFT)
			    d2 = (d2 * d2 * R2FACT)
			    tmp = cmap[2,1+j] - (ig * BC_SHIFT)
			    d2 = d2 + (tmp*tmp * G2FACT)
			    tmp = cmap[3,1+j] - (ib * BC_SHIFT)
			    d2 = d2 + (tmp*tmp * B2FACT)
			    if (d2 < dist) {
				dist = d2
				histogram[1+ir,1+ig,1+ib] = j
			    }
			}
		    }
		}
	    }
	}
end



# CM_CREATE_COLORCELL -- Create a color cell structure

pointer procedure cm_create_colorcell (ColorCells, ra, ga, ba, cmap, ncolor)

pointer	ColorCells[C_LEN,C_LEN,C_LEN]		#U Color cells
int	ra, ga, ba				#I Color to create cell for
short	cmap[3,ncolor]				#I Color map
int	ncolor					#I Number of colors

int	i, n, next_n, ir,ig,ib, r1,g1,b1
long	dist, mindist, tmp
pointer	ptr

begin
	ir = ra / AC_SHIFT
	ig = ga / AC_SHIFT
	ib = ba / AC_SHIFT

	r1 = ir * AC_SHIFT
	g1 = ig * AC_SHIFT
	b1 = ib * AC_SHIFT

	call malloc (ptr, CCELL_LEN, TY_STRUCT)
	ColorCells[1+ir,1+ig,1+ib] = ptr
	CCELL_NUM_ENTS(ptr) = 0

	# step 1: find all colors inside this cell, while we're at
	#    it, find distance of centermost point to furthest corner

	mindist = 2000000000

	for (i=1; i<=ncolor; i=i+1) {
	    if (cmap[1,i]/AC_SHIFT == ir &&
		cmap[2,i]/AC_SHIFT == ig &&
		cmap[3,i]/AC_SHIFT == ib) {
		CCELL_ENTRIES(ptr,CCELL_NUM_ENTS(ptr),0) = i - 1
		CCELL_ENTRIES(ptr,CCELL_NUM_ENTS(ptr),1) = 0
		CCELL_NUM_ENTS(ptr) = CCELL_NUM_ENTS(ptr) + 1

		tmp = cmap[1,i] - r1
		if (tmp < (A_LEN/C_LEN/2))
		    tmp = A_LEN/C_LEN-1 - tmp
		dist = (tmp*tmp * R2FACT)

		tmp = cmap[2,i] - g1
		if (tmp < (A_LEN/C_LEN/2))
		    tmp = A_LEN/C_LEN-1 - tmp
		dist = dist + (tmp*tmp * G2FACT)

		tmp = cmap[3,i] - b1
		if (tmp < (A_LEN/C_LEN/2))
		    tmp = A_LEN/C_LEN-1 - tmp
		dist = dist + (tmp*tmp * B2FACT)

		mindist = min (mindist, dist)
	    }
	}


	# step 3: find all points within that distance to box

	for (i=1; i<=ncolor; i=i+1) {
	    if (cmap[1,i]/AC_SHIFT != ir ||
		cmap[2,i]/AC_SHIFT != ig ||
		cmap[3,i]/AC_SHIFT != ib) {
		dist = 0
		tmp = r1 - cmap[1,i]
		if (tmp>0) {
		    dist = dist + (tmp*tmp * R2FACT)
		} else {
		    tmp = cmap[1,i] - (r1 + A_LEN/C_LEN-1)
		    if (tmp > 0)
			dist = dist + (tmp*tmp * R2FACT)
		}

		tmp = g1 - cmap[2,i]
		if (tmp>0) {
		    dist = dist + (tmp*tmp * G2FACT)
		} else {
		    tmp = cmap[2,i] - (g1 + A_LEN/C_LEN-1)
		    if (tmp > 0)
			dist = dist + (tmp*tmp * G2FACT)
		}

		tmp = b1 - cmap[3,i]
		if (tmp>0) {
		    dist = dist + (tmp*tmp * B2FACT)
		} else {
		    tmp = cmap[3,i] - (b1 + A_LEN/C_LEN-1)
		    if (tmp > 0)
			dist = dist + (tmp*tmp * B2FACT)
		}

		if (dist < mindist) {
		    CCELL_ENTRIES(ptr,CCELL_NUM_ENTS(ptr),0) = i - 1
		    CCELL_ENTRIES(ptr,CCELL_NUM_ENTS(ptr),1) = dist
		    CCELL_NUM_ENTS(ptr) = CCELL_NUM_ENTS(ptr) + 1
		}
	    }
	}


	# sort color cells by distance, use cheap exchange sort
	n = CCELL_NUM_ENTS(ptr) - 1
	while (n > 0) {
	    next_n = 0
	    for (i=0; i<n; i=i+1) {
		if (CCELL_ENTRIES(ptr,i,1) > CCELL_ENTRIES(ptr,i+1,1)) {
		    tmp = CCELL_ENTRIES(ptr,i,0)
		    CCELL_ENTRIES(ptr,i,0) = CCELL_ENTRIES(ptr,i+1,0)
		    CCELL_ENTRIES(ptr,i+1,0) = tmp
		    tmp = CCELL_ENTRIES(ptr,i,1)
		    CCELL_ENTRIES(ptr,i,1) = CCELL_ENTRIES(ptr,i+1,1)
		    CCELL_ENTRIES(ptr,i+1,1) = tmp
		    next_n = i
		}
	    }
	    n = next_n
	}

	return (ptr)
end



# CM_QUANT_FSDITHER -- Quantized Floyd-Steinberg Dither

procedure cm_quant_fsdither (ex, z1, dz, histogram,
	ColorCells, cmap, ncolor, oim)

pointer	ex				#I task struct pointer
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
int	histogram[B_LEN,B_LEN,B_LEN]	#U Histogram
pointer	ColorCells[C_LEN,C_LEN,C_LEN]	#U Color cell data
short	cmap[3,ncolor]			#I Color map 
int	ncolor				#I Number of colors
pointer	oim				#O Output IMIO pointer

pointer	thisptr, nextptr, optr, impl2s()
pointer	sp, thisline, nextline, tmpptr
int     ir, ig, ib, r1, g1, b1, rcell, bcell, gcell
int     i, j, nc, nl, oval

int	ci, cj
long	dist, d2, tmp
pointer	cell

pointer	cm_create_colorcell()

begin
	nc = EX_OCOLS(ex)
	nl = EX_OROWS(ex)

	call smark (sp)
	call salloc (thisline, nc * 3, TY_INT)
	call salloc (nextline, nc * 3, TY_INT)

	# get first line of picture
	call cm_getline (ex, z1, dz, 1, nextline)

	for (i=1; i<=nl; i=i+1) {
	    # swap thisline and nextline
	    tmpptr = thisline
	    thisline = nextline
	    nextline = tmpptr

	    # read in next line
	    if (i < nl)
		#call cm_getline (ex, z1, dz, i, nextline, nc)
		call cm_getline (ex, z1, dz, i, nextline)

	    # dither this line and put it into the output picture
	    thisptr = thisline
	    nextptr = nextline
	    optr = impl2s (oim, i)

	    for (j=1; j<=nc; j=j+1) {
		r1 = Memi[thisptr]
		g1 = Memi[thisptr+1]
		b1 = Memi[thisptr+2]
		thisptr = thisptr + 3

		r1 = max (0, min (A_LEN-1, r1))
		g1 = max (0, min (A_LEN-1, g1))
		b1 = max (0, min (A_LEN-1, b1))

		ir = r1 / AB_SHIFT
		ig = g1 / AB_SHIFT
		ib = b1 / AB_SHIFT

		oval = histogram[1+ir,1+ig,1+ib]
		if (oval == -1) {
		    rcell = 1 + ir / BC_SHIFT
		    gcell = 1 + ig / BC_SHIFT
		    bcell = 1 + ib / BC_SHIFT
		    cell = ColorCells[rcell, gcell, bcell]
		    if (cell == NULL)
			cell = cm_create_colorcell (ColorCells, r1, g1, b1,
			    cmap, ncolor)

		    dist = 2000000000
		    for (ci=0; ci<CCELL_NUM_ENTS(cell) &&
			dist>CCELL_ENTRIES(cell,ci,1); ci=ci+1) {
			cj = CCELL_ENTRIES(cell,ci,0)
			d2 = (cmap[1,1+cj]/AB_SHIFT) - ir
			d2 = (d2*d2 * R2FACT)
			tmp = (cmap[2,1+cj]/AB_SHIFT) - ig
			d2 = d2 + (tmp*tmp * G2FACT)
			tmp = (cmap[3,1+cj]/AB_SHIFT) - ib
			d2 = d2 + (tmp*tmp * B2FACT)
			if (d2<dist) {
			    dist = d2
			    oval = cj
			}
		    }
		    histogram[1+ir,1+ig,1+ib] = oval
		}

		Mems[optr] = 1 + oval
		optr = optr + 1

		r1 = r1 - cmap[1,1+oval]
		g1 = g1 - cmap[2,1+oval]
		b1 = b1 - cmap[3,1+oval]
	
		# don't use tables, because r1,g1,b1 could go negative
		if (j < nc) {
		    tmpptr = thisptr
		    if (r1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (r1*7-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (r1*7+8)/16
		    tmpptr = tmpptr + 1
		    if (g1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (g1*7-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (g1*7+8)/16
		    tmpptr = tmpptr + 1
		    if (b1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (b1*7-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (b1*7+8)/16
		}
	
		if (i < nl) {
		    if (j > 1) {
			tmpptr = nextptr - 3
			if (r1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (r1*3-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (r1*3+8)/16
			tmpptr = tmpptr + 1
			if (g1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (g1*3-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (g1*3+8)/16
			tmpptr = tmpptr + 1
			if (b1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (b1*3-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (b1*3+8)/16
		    }

		    tmpptr = nextptr
		    if (r1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (r1*5-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (r1*5+8)/16
		    tmpptr = tmpptr + 1
		    if (g1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (g1*5-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (g1*5+8)/16
		    tmpptr = tmpptr + 1
		    if (b1 < 0)
			Memi[tmpptr] = Memi[tmpptr] + (b1*5-8)/16
		    else
			Memi[tmpptr] = Memi[tmpptr] + (b1*5+8)/16

		    if (j < nc) {
			tmpptr = nextptr + 3
			if (r1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (r1-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (r1+8)/16
			tmpptr = tmpptr + 1
			if (g1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (g1-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (g1+8)/16
			tmpptr = tmpptr + 1
			if (b1 < 0)
			    Memi[tmpptr] = Memi[tmpptr] + (b1-8)/16
			else
			    Memi[tmpptr] = Memi[tmpptr] + (b1+8)/16
		    }
		    nextptr = nextptr + 3
		}
	    }
	}

        # Flush the pixels to the output image, otherwise we end up with an
        # odd line which may or may not be actual pixels.
        call imflush (oim)

	call sfree (sp)
end
