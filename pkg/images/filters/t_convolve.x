# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <mach.h>
include <ctype.h>

define	SZ_KERNEL	SZ_LINE


# T_CONVOLVE -- Convolve a list of IRAF images with an arbitrary kernel.

procedure t_convolve()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image
int	boundary				# Type of boundary extension
real	constant				# Constant boundary extension
int	delim					# record delimiter for files

char	str[SZ_LINE], imtemp[SZ_FNAME]
int	list1, list2, kxdim, kydim, radsym
pointer	sp, im1, im2, kername, kernel

bool	clgetb()
char	clgetc()
int	imtopen(), imtgetim(), imtlen(), clgwrd(), btoi()
pointer	immap()
real	clgetr()
errchk	cnv_convolve

begin
	call smark (sp)
	call salloc (kername, SZ_LINE, TY_CHAR)

	# Get input and output image parameters
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)
	call clgstr ("kernel", Memc[kername], SZ_LINE)

	# Get boundary extension parameters
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Check list lengths
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Get the kernel
	delim = int (clgetc ("row_delimiter"))
	iferr (call cnv_kernel (Memc[kername], SZ_LINE, delim,
	    kernel, kxdim, kydim))
	    call erract (EA_FATAL)
	if (mod (kxdim, 2) != 0)
	    radsym = btoi (clgetb ("radsym"))
	else
	    radsym = NO

	call sfree (sp)

	# Convolve the images with the kernel
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    # Make temporary image name
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    # Open images
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Convolve an image with the kernel
	    iferr {

		switch (IM_NDIM(im1)) {
		case 1:
		    kydim = 1
		case 2:
		    ;
		default:
		    call error (0, "T_CONVOLVE: Image dimension > 2.")
		}

		# Convolve image
		call cnv_convolve (im1, im2, Memr[kernel], kxdim, kydim,
		    boundary, constant, radsym)

	    } then {
		call eprintf ("Error convolving image: %s\n")
		    call pargstr (image1)
		call erract (EA_WARN)
		call imunmap (im1)
		call imunmap (im2)
		call imdelete (image2)
	    } else {
	        call imunmap (im1)
	        call imunmap (im2)
	        call xt_delimtemp (image2, imtemp)
	    }
	}

	# Close image lists
	call imtclose (list1)
	call imtclose (list2)

	# Free kernel
	call mfree (kernel, TY_REAL)
end


# CNV_KERNEL -- Make the kernel. If kername begins with a digit, a period or
# a minus sign CNV_KERNEL opens the kername string as a file and passes
# the file descriptor to the decoding routines. Otherwise CNV_KERNEL tries
# to open a text file on disk.

procedure cnv_kernel (kername, maxch, delim, kernel, nx, ny)

char	kername[maxch]		# kernal
int	maxch			# maximum length of kername
int	delim		# delimiter for kernel rows
pointer	kernel			# Gaussian kernel
int	nx, ny			# dimensions of the kernel

int	fd
int	stropen(), open()

begin
	if (IS_DIGIT(kername[1]) || kername[1] == '.' || kername[1] == '-' ||
	    kername[1] == '+') {
	    fd = stropen (kername, maxch, READ_ONLY)
	    call cnv_decode_kernel (fd, kernel, nx, ny, delim)
	    call strclose (fd)
	} else {
	    fd = open (kername, READ_ONLY, TEXT_FILE)
	    call cnv_decode_kernel (fd, kernel, nx, ny, delim)
	    call cnv_rowflip (Memr[kernel], nx, ny)
	    call close (fd)
	}
end


# CNV_ROWFLIP -- Column flip a 2D matrix in place

procedure cnv_rowflip (a, nx, ny)

real	a[nx,ny]	# matrix to be flipped
int	nx, ny		# dimensions of a

int	i, j, nhalf, ntotal
real	temp

begin
	nhalf = ny / 2
	ntotal = ny + 1

	do i = 1, nx {
	    do j = 1, nhalf {
		temp = a[i,j]
		a[i,j] = a[i,ntotal-j]
		a[i,ntotal-j] = temp
	    }
	}
end


# CNV_DECODE_KERNEL -- Procedure to decode the kernel

procedure cnv_decode_kernel (fd, kernel, nx, ny, delim)

int	fd		# file descriptor
pointer	kernel		# pointer to kernel
int	nx, ny		# kernel dimensions
int	delim		# kernel row delimiter

pointer	sp, line
int	sz_kernel, kp, lp, minnx, maxnx, nchars
int	getline(), ctor()

begin
	# Line buffer
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Initialize row and column counters
	nx = 0
	ny = 0
	minnx = MAX_INT
	maxnx = -MAX_INT

	kp = 0

	# Decode kernel file
	nchars = getline (fd, Memc[line])
	while (nchars != EOF) {

	    # Decode the kernel line
	    for (lp = 1; lp <= nchars; ) {

	        # Check to see that kernel is big enough
	        if (kp == 0) {
		    sz_kernel = SZ_KERNEL
		    call malloc (kernel, sz_kernel, TY_REAL)
	        } else if (kp > sz_kernel) {
		    sz_kernel = sz_kernel + SZ_KERNEL
		    call realloc (kernel, sz_kernel, TY_REAL)
	        }

		# Decode the kernel elements
		if (Memc[line+lp-1] == delim) {
		    minnx = min (minnx, nx)
		    maxnx = max (maxnx, nx)
		    nx = 0
		    ny = ny + 1
		    lp = lp + 1

		} else if (Memc[line+lp-1] == '\n' ||
		    IS_WHITE(Memc[line+lp-1]) || Memc[line+lp-1] == ',') {

		    lp = lp + 1

		} else {
		    # Decode kernel element
		    if (ctor (Memc[line], lp, Memr[kernel+kp]) == 0) {
			call sfree (sp)
			call error (0, "CNV_DECODE_KERNEL: Invalid kernel.")
		    }

		    kp = kp + 1
		    nx = nx + 1
		}
	    }

	    nchars = getline (fd, Memc[line])
	}

	# Last delimiter not necessary
	if (nx != 0)
	    ny = ny + 1

	# Free temporary space
	call sfree (sp)

	# Test that the kernel is correct size
	if (minnx != maxnx)
	    call error (0, "CNV_KERNEL: Kernel rows are different lengths.")
	else if ((kp != minnx * ny) || (kp != maxnx * ny))
	    call error (0, "CNV_KERNEL: Incorrect kernel row number.")
	else {
	    call realloc (kernel, kp, TY_REAL)
	    nx = minnx
	}
end
