include	<imhdr.h>


# T_RGBDITHER -- Make special RGB 8 bit dither image

procedure t_rgbdither ()

pointer	im[3]			# Red, green, blue images
pointer	rgb			# Output image
real	rz1, rz2		# Red display range
real	gz1, gz2		# Green display range
real	bz1, bz2		# Blue display range
int	blk			# Block average factor
bool	logmap			# Logartihmic intensity mapping?

int	i, j, k, l, nc, nl, ncblk, nlblk, dither[3,3]
real	rdz, rz, rs, gdz, gz, gs, bdz, bz, bs, v
pointer	buf[3], rgbbuf, ptr1, ptr2, ptr3
pointer	sp, fname
bool	clgetb()
real	clgetr()
int	clgeti()
pointer	immap(), imgl2r(), impl2s()

data	dither/1,2,3,2,3,1,3,1,2/

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Open input RGB images
	call clgstr ("red", Memc[fname], SZ_FNAME)
	im[1] = immap (Memc[fname], READ_ONLY, 0)
	call clgstr ("green", Memc[fname], SZ_FNAME)
	im[2] = immap (Memc[fname], READ_ONLY, 0)
	call clgstr ("blue", Memc[fname], SZ_FNAME)
	im[3] = immap (Memc[fname], READ_ONLY, 0)

	# Get other parameters
	rz1 = clgetr ("rz1")
	rz2 = clgetr ("rz2")
	gz1 = clgetr ("gz1")
	gz2 = clgetr ("gz2")
	bz1 = clgetr ("bz1")
	bz2 = clgetr ("bz2")
	blk = clgeti ("blkavg")
	logmap = clgetb ("logmap")
	call clgstr ("pattern", Memc[fname], SZ_FNAME)

	# Parse the dither pattern
	ptr1 = fname 
	do j = 1, 3 {
	    do i = 1, 3 {
		if (Memc[ptr1] == 'r')
		    dither[i,j] = 1
		else if (Memc[ptr1] == 'g')
		    dither[i,j] = 2
		else if (Memc[ptr1] == 'b')
		    dither[i,j] = 3
		else
		    call error (1, "Error reading dither pattern")
		ptr1 = ptr1 + 1
	    }
	}

	# Check dimensions
	i = IM_NDIM(im[1])
	nc = IM_LEN(im[1],1)
	nl = IM_LEN(im[1],2)
	ncblk = nc / blk
	nlblk = nl / blk
	if (i != 2 || i != IM_NDIM(im[2]) || i != IM_NDIM(im[3]))
	    call error (1, "All images must be two dimensional")
	if (nc != IM_LEN(im[2],1) || nc != IM_LEN(im[3],1))
	    call error (1, "All images must be the same size")
	if (nl != IM_LEN(im[2],2) || nl != IM_LEN(im[3],2))
	    call error (1, "All images must be the same size")

	# Open and initialize the output image
	call clgstr ("rgb", Memc[fname], SZ_FNAME)
	rgb = immap (Memc[fname], NEW_COPY, im[1])
	IM_PIXTYPE(rgb) = TY_SHORT
	IM_LEN(rgb,1) = 3 * ncblk
	IM_LEN(rgb,2) = 3 * nlblk

	# Set the z range
	if (logmap) {
	    rdz = 9. / (rz2 - rz1)
	    gdz = 9. / (gz2 - gz1)
	    bdz = 9. / (bz2 - bz1)
	} else {
	    rdz = 1. / (rz2 - rz1)
	    gdz = 1. / (gz2 - gz1)
	    bdz = 1. / (bz2 - bz1)
	}
	rz = 0.
	gz = 85.
	bz = 170.
	rs = 84.
	gs = 84.
	bs = 84.

	# Setup and do the block averaging
	if (blk > 1) {
	    call salloc (buf[1], ncblk, TY_REAL)
	    call salloc (buf[2], ncblk, TY_REAL)
	    call salloc (buf[3], ncblk, TY_REAL)
	}

	do j = 0, nlblk-1 {
	    if (blk > 1) {
		do k = 1, 3 {
		    call aclrr (Memr[buf[k]], ncblk)
		    do l = 1, blk {
			ptr1 = imgl2r (im[k], j*blk+l)
			do i = 0, ncblk*blk-1 {
			    ptr2 = buf[k] + i / blk
			    Memr[ptr2] = Memr[ptr2] + Memr[ptr1+i]
			}
		    }
		    call adivkr (Memr[buf[k]], real (blk*blk),
			Memr[buf[k]], ncblk)
		}
			
	    } else {
		buf[1] = imgl2r (im[1], j+1)
		buf[2] = imgl2r (im[2], j+1)
		buf[3] = imgl2r (im[3], j+1)
	    }

	    # Map the input values to the output levels
	    ptr1 = buf[1]
	    ptr2 = buf[2]
	    ptr3 = buf[3]
	    if (logmap) {
		do i = 1, ncblk {
		    v = max (1., min (10., 1. + (Memr[ptr1] - rz1) * rdz))
		    Memr[ptr1] = nint (rz + rs * log10 (v))
		    v = max (1., min (10., 1. + (Memr[ptr2] - gz1) * gdz))
		    Memr[ptr2] = nint (gz + gs * log10 (v))
		    v = max (1., min (10., 1. + (Memr[ptr3] - bz1) * bdz))
		    Memr[ptr3] = nint (bz + bs * log10 (v))
		    ptr1 = ptr1 + 1
		    ptr2 = ptr2 + 1
		    ptr3 = ptr3 + 1
		}
	    } else {
		do i = 1, ncblk {
		    v = max (0., min (1., (Memr[ptr1] - rz1) * rdz))
		    Memr[ptr1] = nint (rz + rs * v)
		    v = max (0., min (1., (Memr[ptr2] - gz1) * gdz))
		    Memr[ptr2] = nint (gz + gs * v)
		    v = max (0., min (1., (Memr[ptr3] - bz1) * bdz))
		    Memr[ptr3] = nint (bz + bs * v)
		    ptr1 = ptr1 + 1
		    ptr2 = ptr2 + 1
		    ptr3 = ptr3 + 1
		}
	    }

	    # Build and output the dither pattern
	    do k = 1, 3 {
		ptr1 = buf[dither[1,k]]
		ptr2 = buf[dither[2,k]]
		ptr3 = buf[dither[3,k]]
		rgbbuf = impl2s (rgb, 3*j+k)
		do i = 1, ncblk {
		    Mems[rgbbuf] = Memr[ptr1]
		    Mems[rgbbuf+1] = Memr[ptr2]
		    Mems[rgbbuf+2] = Memr[ptr3]
		    ptr1 = ptr1 + 1
		    ptr2 = ptr2 + 1
		    ptr3 = ptr3 + 1
		    rgbbuf = rgbbuf + 3
		}
	    }
	}

	# Make a record in the output image header
	call sprintf (Memc[fname], SZ_FNAME, "%g %g %g %g %g %g %d %b")
	    call pargr (rz1)
	    call pargr (rz2)
	    call pargr (gz1)
	    call pargr (gz2)
	    call pargr (bz1)
	    call pargr (bz2)
	    call pargi (blk)
	    call pargb (logmap)
	call imastr (rgb, "MKRGB8", Memc[fname])
		
	# Finish up
	call imunmap (rgb)
	call imunmap (im[3])
	call imunmap (im[2])
	call imunmap (im[1])

	call sfree (sp)
end
