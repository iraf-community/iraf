include <imhdr.h>


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

# Output color map types and number of colors
define	NCOLORS		199
define	MAPTYPES	"|saoimage|imtool|ximtool|"
define	SAOIMAGE	1
define	IMTOOL		2
define	XIMTOOL		3


# T_RGBTO8 -- Convert RGB IRAF images to 8 bit IRAF image and color map using
# Heckbert's Median Cut algorithm.  The implementation of this algorithm
# was modeled, with permission, on that in the program XV written by
# John Bradley.

procedure t_rgbto8 ()

pointer	im[3]			# Red, green, blue images
pointer	oim			# Output image
int	maptype			# Color map type
int	nmap			# Number of colors in map
real	z1[3], dz[3]		# Display range
bool	logmap			# Logartihmic intensity mapping?

int	i, ncolors, fd
pointer	sp, rgb, root, mapname, cmap, box_list, histogram, ColorCells
pointer freeboxes, usedboxes, ptr

bool	clgetb()
int	clgeti(), clgwrd(), access(), open(), strlen()
real	clgetr()
pointer	immap(), largest_box()
errchk	open, immap

begin
	call smark (sp)
	call salloc (rgb, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (mapname, SZ_FNAME, TY_CHAR)

	# Open input images.
	call clgstr ("red", Memc[rgb], SZ_FNAME)
	im[1] = immap (Memc[rgb], READ_ONLY, 0)
	call clgstr ("green", Memc[rgb], SZ_FNAME)
	im[2] = immap (Memc[rgb], READ_ONLY, 0)
	call clgstr ("blue", Memc[rgb], SZ_FNAME)
	im[3] = immap (Memc[rgb], READ_ONLY, 0)

	# Check all input images are 2D and the same size.
	i = IM_NDIM(im[1])
	if (i != 2 || i != IM_NDIM(im[2]) || i != IM_NDIM(im[3]))
	    call error (1, "All images must be two dimensional")
	i = IM_LEN(im[1],1)
	if (i != IM_LEN(im[2],1) || i != IM_LEN(im[3],1))
	    call error (1, "All images must be the same size")
	i = IM_LEN(im[1],2)
	if (i != IM_LEN(im[2],2) || i != IM_LEN(im[3],2))
	    call error (1, "All images must be the same size")

	# Open output color map and image.  Do this now rather than later
	# to get an immediate error since the following can take some time.

	call clgstr ("rgb", Memc[rgb], SZ_FNAME)
	maptype = clgwrd ("maptype", Memc[mapname], SZ_FNAME, MAPTYPES)
	call imgimage (Memc[rgb], Memc[root], SZ_FNAME)
	i = strlen (Memc[root]) - 1
	switch (Memc[root+i]) {
	case 'h':
	    if (i > 3 && Memc[root+i-3] == '.')
		Memc[root+i-3] = EOS
	case 'l':
	    if (i > 2 && Memc[root+i-2] == '.')
		Memc[root+i-2] = EOS
	}
	switch (maptype) {
	case SAOIMAGE:
	    nmap = NCOLORS
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.sao")
		call pargstr (Memc[root])
	    if (access (Memc[mapname], 0, 0) == YES)
		fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	case IMTOOL:
	    nmap = NCOLORS
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.imt")
		call pargstr (Memc[root])
	    if (access (Memc[mapname], 0, 0) == YES)
		fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	case XIMTOOL:
	    nmap = clgeti ("ncolors")
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.xim")
		call pargstr (Memc[root])
	    if (access (Memc[mapname], 0, 0) == YES)
		fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	}

	oim = immap (Memc[rgb], NEW_COPY, im[1])
	IM_PIXTYPE(oim) = TY_SHORT

	# Set input image intensity scaling.
	z1[1] = clgetr ("rz1")
	dz[1] = clgetr ("rz2")
	z1[2] = clgetr ("gz1")
	dz[2] = clgetr ("gz2")
	z1[3] = clgetr ("bz1")
	dz[3] = clgetr ("bz2")
	logmap = clgetb ("logmap")

	if (logmap) {
	    dz[1] = 9. / (dz[1] - z1[1])
	    dz[2] = 9. / (dz[2] - z1[2])
	    dz[3] = 9. / (dz[3] - z1[3])
	} else {
	    dz[1] = 255. / (dz[1] - z1[1])
	    dz[2] = 255. / (dz[2] - z1[2])
	    dz[3] = 255. / (dz[3] - z1[3])
	}

	# Allocate color map.
	call salloc (cmap, 3 * nmap, TY_SHORT)

	# Allocate and initialize color boxes.
	call salloc (box_list, nmap * CBOX_LEN, TY_STRUCT) 

	freeboxes = box_list
	usedboxes = NULL
	ptr = freeboxes
	CBOX_PREV(ptr) = NULL
	CBOX_NEXT(ptr) = ptr + CBOX_LEN
	for (i=2; i<nmap; i=i+1) {
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
	call salloc (histogram, B_LEN*B_LEN*B_LEN, TY_INT)
	call aclri (Memi[histogram], B_LEN*B_LEN*B_LEN)
	call get_histogram(im, z1, dz, logmap, ptr, Memi[histogram])

	# Subdivide boxes until no more free boxes remain
	while (freeboxes != NULL) {
	    ptr = largest_box (usedboxes)
	    if (ptr != NULL)
		call splitbox (ptr, usedboxes, freeboxes, Memi[histogram])
	    else
		break
	}

	# Set color map and write it out.
	ptr = usedboxes
	for (i=0; i<nmap && ptr!=NULL; i=i+1) {
	    call assign_color (ptr, Mems[cmap+3*i])
	    ptr = CBOX_NEXT(ptr)
	}
	ncolors = i

	switch (maptype) {
	case SAOIMAGE:
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.sao")
		call pargstr (Memc[root])
	    fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	    call sao_write (fd, Mems[cmap], nmap, ncolors)
	    call close (fd)
	case IMTOOL:
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.imt")
		call pargstr (Memc[root])
	    fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	    call imt_write (fd, Mems[cmap], nmap, ncolors)
	    call close (fd)
	case XIMTOOL:
	    call sprintf (Memc[mapname], SZ_FNAME, "%s.xim")
		call pargstr (Memc[root])
	    fd = open (Memc[mapname], NEW_FILE, TEXT_FILE)
	    call xim_write (fd, Mems[cmap], nmap, ncolors)
	    call close (fd)
	}

	# Scan histogram and map all values to closest color.
	# First create cell list as described in Heckbert[2] and then
	# create mapping from truncated pixel space to color table entries

	call salloc (ColorCells, C_LEN*C_LEN*C_LEN, TY_POINTER)
	call aclri (Memi[ColorCells], C_LEN*C_LEN*C_LEN)
	call map_colortable (Memi[histogram], Mems[cmap], ncolors,
	    Memi[ColorCells])

	# Scan image and match input values to table entries.
	# Apply Floyd-Steinberg dithering.

	call quant_fsdither (im, z1, dz, logmap, Memi[histogram],
	    Memi[ColorCells], Mems[cmap], ncolors, oim)

	# Finish up.
	call imunmap (oim)
	call imunmap (im[1])
	call imunmap (im[2])
	call imunmap (im[3])

	for (i=0; i < C_LEN*C_LEN*C_LEN; i=i+1) {
	    if (Memi[ColorCells+i] != NULL)
		call mfree (Memi[ColorCells+i], TY_STRUCT)
	}

	call sfree (sp)
end


# SAO_WRITE -- Write color map for SAOIMAGE.

procedure sao_write (fd, cmap, nmap, ncolors)

int	fd				# Output file descriptor
short	cmap[3,nmap]			# Color map
int	nmap				# Size of color map
int	ncolors				# Number of colors assigned

int	i

begin
	call fprintf (fd, "PSEUDOCOLOR\n")
	call fprintf (fd, "RED:\n")
	call fprintf (fd, "(0.,0.)\n")
	for (i=1; i<=199; i=i+1) {
	    call fprintf (fd, "(%g,%g)\n")
		call pargr (real(i)/199.)
		call pargr ((int(cmap[1,min(ncolors,i)])*256+1) / 65535.)
	}
	call fprintf (fd, "\nGREEN:\n")
	call fprintf (fd, "(0.,0.)\n")
	for (i=1; i<=199; i=i+1) {
	    call fprintf (fd, "(%g,%g)\n")
		call pargr (real(i)/199)
		call pargr ((int(cmap[2,min(ncolors,i)])*256+1) / 65535.)
	}
	call fprintf (fd, "\nBLUE:\n")
	call fprintf (fd, "(0.,0.)\n")
	for (i=1; i<=199; i=i+1) {
	    call fprintf (fd, "(%g,%g)\n")
		call pargr (real(i)/199)
		call pargr ((int(cmap[3,min(ncolors,i)])*256+1) / 65535.)
	}
	call fprintf (fd, "\n")
end

procedure imt_write (fd, cmap, nmap, ncolors)

int	fd				# Output file descriptor
short	cmap[3,nmap]			# Color map
int	nmap				# Size of color map
int	ncolors				# Number of colors assigned

int	i, j

begin
	for (i=1; i<=256; i=i+1) {
	    j = (i - 128) * 199. / 255. + 101.
	    j = max (1, min (ncolors, j))
	    call fprintf (fd, "%g %g %g\n")
		call pargr ((cmap[1,j] + 0.5) / 255.)
		call pargr ((cmap[2,j] + 0.5) / 255.)
		call pargr ((cmap[3,j] + 0.5) / 255.)
	}
end


# XIM_WRITE -- Write color map for XIMTOOL.

procedure xim_write (fd, cmap, nmap, ncolors)

int	fd				# Output file descriptor
short	cmap[3,nmap]			# Color map
int	nmap				# Size of color map
int	ncolors				# Number of colors assigned

int	i

begin
	for (i=1; i<=min(ncolors,200); i=i+1) {
	    call fprintf (fd, "%g %g %g\n")
		call pargr ((cmap[1,i] + 0.5) / 255.)
		call pargr ((cmap[2,i] + 0.5) / 255.)
		call pargr ((cmap[3,i] + 0.5) / 255.)
	}
	for (; i<=nmap; i=i+1)
	    call fprintf (fd, "0 0 0\n")
end


# XV_GETLINE -- Get a line of intensity mapped input data.

procedure xv_getline (im, z1, dz, logmap, line, data)

pointer	im[3]				#I IMIO pointers
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
bool	logmap				#I Intensity mapping log map?
int	line				#I Line to be obtained
pointer	data				#O Intensity mapped data

int	i, j, nc
real	a, b, c
pointer	iptr, optr, imgl2s()

begin
	nc = IM_LEN(im[1],1)

	do i = 1, 3 {
	    iptr = imgl2s (im[i], line)
	    optr = data + i - 1
	    a = z1[i]
	    b = dz[i]
	    if (logmap) {
		do j = 1, nc {
		    c = max (1., min (10., 1. + (Mems[iptr] - a) * b))
		    Memi[optr] = max (0, min (255, nint (log10 (c) * 255)))
		    iptr = iptr + 1
		    optr = optr + 3
		}
	    } else {
		do j = 1, nc {
		    Memi[optr] = max (0, min (255, nint ((Mems[iptr] - a) * b)))
		    iptr = iptr + 1
		    optr = optr + 3
		}
	    }
	}
end


# GET_HISTOGRAM -- Compute color histogram

procedure get_histogram (im, z1, dz, logmap, box, histogram)

pointer	im[3]				#I IMIO pointers
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
bool	logmap				#I Intensity mapping log map?
pointer	box				#O Initial box
int	histogram[B_LEN,B_LEN,B_LEN]	#O Histogram

int	i, j, nc, nl, r, g, b, rmin, gmin, bmin, rmax, gmax, bmax
pointer	sp, data, ptr

begin
	nc = IM_LEN(im[1],1)
	nl = IM_LEN(im[1],2)

	call smark (sp)
	call salloc (data, 3 * nc, TY_INT)

	rmin = A_LEN; rmax = -1 
	gmin = A_LEN; gmax = -1 
	bmin = A_LEN; bmax = -1 

	# calculate histogram
	do j = 1, nl {
	    call xv_getline (im, z1, dz, logmap, j, data)
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



# LARGEST_BOX -- Return pointer to largest box

pointer procedure largest_box (usedboxes)

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


# SPLITBOX -- Split a box along largest dimension

procedure splitbox (box, usedboxes, freeboxes, histogram)

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

	call shrinkbox (new, histogram)
	call shrinkbox (box, histogram)
	call sfree (sp)
end


# SHRINKBOX -- Shrink box

procedure shrinkbox (box, histogram)

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



# ASSIGN_COLOR -- Assign colors

procedure assign_color (box, cmap)

pointer	box					#I Box
short	cmap[3]					#O Color map entry

begin
	# +1 ensures that color represents the middle of the box

	cmap[1] = ((CBOX_RMIN(box) + CBOX_RMAX(box) - 2) * AB_SHIFT) / 2
	cmap[2] = ((CBOX_GMIN(box) + CBOX_GMAX(box) - 2) * AB_SHIFT) / 2
	cmap[3] = ((CBOX_BMIN(box) + CBOX_BMAX(box) - 2) * AB_SHIFT) / 2
end



# MAP_COLORTABLE -- Map the color table

procedure map_colortable (histogram, cmap, ncolor, ColorCells)

int	histogram[B_LEN,B_LEN,B_LEN]		#U Histogram
short	cmap[3,ncolor]				#I Color map
int	ncolor					#I Number of colors
pointer	ColorCells[C_LEN,C_LEN,C_LEN]		#O Color cells

int	i, j, ir, ig, ib, rcell, bcell, gcell
long	dist, d2, tmp
pointer	cell, create_colorcell()

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
			    cell = create_colorcell (ColorCells,
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



# CREATE_COLORCELL -- Create a color cell structure

pointer procedure create_colorcell (ColorCells, ra, ga, ba, cmap, ncolor)

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



# QUANT_FSDITHER -- Quantized Floyd-Steinberg Dither

procedure quant_fsdither (im, z1, dz, logmap, histogram,
	ColorCells, cmap, ncolor, oim)

pointer	im[3]				#I IMIO pointers
real	z1[3]				#I Intensity mapping origins
real	dz[3]				#I Intensity mapping ranges
bool	logmap				#I Intensity mapping log map?
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

pointer	create_colorcell()

begin
	nc = IM_LEN(im[1], 1)
	nl = IM_LEN(im[1], 2)

	call smark (sp)
	call salloc (thisline, nc * 3, TY_INT)
	call salloc (nextline, nc * 3, TY_INT)

	# get first line of picture
	call xv_getline (im, z1, dz, logmap, 1, nextline)

	for (i=1; i<=nl; i=i+1) {
	    # swap thisline and nextline
	    tmpptr = thisline
	    thisline = nextline
	    nextline = tmpptr

	    # read in next line
	    if (i < nl)
		call xv_getline (im, z1, dz, logmap, i, nextline)

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
			cell = create_colorcell (ColorCells, r1, g1, b1,
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

	call sfree (sp)
end
