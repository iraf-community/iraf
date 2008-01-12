# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<syserr.h>
include	<imhdr.h>
include	<imio.h>

define	FIRST		1
define	LAST		MAX_LONG

.help imisec
.nf ___________________________________________________________________________
IMISEC -- Translate a section specification string (passed as a suffix
to the imagefile filename) into a set of logical to physical transformation
vectors.

The image section notation is used to access portions of an image, to reduce
the dimensionality of an image, to reverse the coordinates of any of the axes
of an image, and so on.  Since this facility is built into IMIO, and is
completely transparent to programs using IMIO, it significantly increases
the power and flexibility of all programs which assess images, without
complicating the applications code.

Examples ("map (image_name, ...)":

		   image_name			meaning

		image[*,-*]		(flip columns end for end)
		image[*,*,5]		(band 5 of image cube)
		image[*,5,*]		(x,y --> x,z)
		image[x1:x2,y1:y2]	(2-D subraster)
		image[x1:x2:n,*]	(subsample by N in x)

If the number of dimensions specified in the section is less than the number
of physical dimensions in the image then the higher dimensions default to 1.
If the number of dimensions given is greater than the number of phyiscal
dimensions then the nonphysical excess dimensions must be set to 1.
.endhelp ______________________________________________________________________


procedure imisec (imdes, section)

pointer	imdes
char	section[ARB]
int	ip, i, dim, nsubscripts, nphysdim, nlogdim
long	x1[IM_MAXDIM], x2[IM_MAXDIM], step[IM_MAXDIM], clktime()

begin
	# Set up null mapping (default).  Check for null section string,
	# or null section, and return if found.

	nphysdim = IM_NDIM(imdes)

	call aclrl (IM_VOFF(imdes,1), nphysdim)
	call amovkl (long(1), IM_VSTEP(imdes,1), nphysdim)

	do dim = 1, nphysdim
	    IM_VMAP(imdes,dim) = dim

	if (section[1] == EOS)
	    return
	else if (section[1] != '[')
	    call imerr (IM_NAME(imdes), SYS_IMSYNSEC)

	ip = 2
	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	if (section[ip] == ']')
	    return


	# Decode the section string, yielding the vectors X1, X2, and STEP,
	# of length NSUBSCRIPTS.

	for (i=1;  i <= IM_MAXDIM && section[ip] != ']';  i=i+1)
	    call im_decode_subscript (section, ip, x1[i], x2[i], step[i])
	nsubscripts = i - 1


	# Set the transformation vectors.  If too few dimensions were given
	# set the higher dimensions to 1.  If too many dimensions were given
	# the higher dimensions must have been set to 1 in the section.

	for (dim = nsubscripts + 1;  dim <= nphysdim;  dim = dim + 1) {
	    x1[dim] = 1
	    x2[dim] = 1
	    step[dim] = 1
	}
	for (dim = nphysdim + 1;  dim <= nsubscripts;  dim = dim + 1)
	    if (x1[dim] != 1 || x2[dim] != 1)
		call imerr (IM_NAME(imdes), SYS_IMDIMSEC)

	nlogdim = 0
	for (dim=1;  dim <= nphysdim;  dim=dim+1) {
	    # Set up transformation for a single physical dimension.
	    call im_ctranset (imdes, dim, x1[dim], x2[dim], step[dim])

	    # Map logical dimension onto physical dimension.
	    if (x1[dim] != x2[dim]) {
		nlogdim = nlogdim + 1
		IM_VMAP(imdes,nlogdim) = dim
		IM_LEN(imdes,nlogdim) = IM_LEN(imdes,dim)
	    }
	}

	# Convert a zero-dimensional image into a one dimensional image
	# of length one pixel (section addresses a single pixel).

	if (nlogdim == 0) {
	    nlogdim = 1
	    IM_VMAP(imdes,1) = 1
	    IM_LEN(imdes,1) = 1
	}

	IM_NDIM(imdes) = nlogdim
	IM_MTIME(imdes) = clktime (long(0))
end
    

# IM_CTRANSET -- Set the logical to physical section coordinate transformation
# coefficients VOFF and VSTEP for the axis DIM.  Adjust the length of the
# logical axis IM_LEN if needed.

procedure im_ctranset (imdes, dim, x1_arg, x2_arg, step)

pointer	imdes
int	dim
long	x1_arg, x2_arg, step, x1, x2, length_axis

begin
	x1 = x1_arg
	if (x1_arg == LAST)
	    x1 = IM_LEN(imdes,dim)
	x2 = x2_arg
	if (x2_arg == LAST)
	    x2 = IM_LEN(imdes,dim)

	# Compute the number of pixels in this axis of the section, allowing
	# for non-unity step sizes.  Set the axis length seen by the calling
	# program to this value.

	length_axis = (x2 - x1) / step + 1
	if (length_axis <= 0)
	    call imerr (IM_NAME(imdes), SYS_IMSTEPSEC)
	else
	    IM_LEN(imdes,dim) = length_axis

	IM_VOFF(imdes,dim) = x1 - step
	IM_VSTEP(imdes,dim) = step
end


# IM_DECODE_SUBSCRIPT -- Decode a single subscript expression to produce the
# range of values for that subscript (X1:X2), and the sampling step size, STEP.
# Note that X1 may be less than, greater than, or equal to X2, and STEP may
# be a positive or negative nonzero integer.  Various shorthand notations are
# permitted, as is embedded whitespace.

procedure im_decode_subscript (section, ip, x1, x2, step)

char	section[ARB]
int	ip
long	x1, x2, step, temp
int	ctol()
define	synerr_ 99

begin
	x1 = FIRST
	x2 = LAST
	step = 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get X1, X2.
	if (ctol (section, ip, temp) > 0) {			# [x1
	    x1 = temp
	    if (section[ip] == ':') {	
		ip = ip + 1
		if (ctol (section, ip, x2) == 0)		# [x1:x2
		    goto synerr_
	    } else
		x2 = x1

	} else if (section[ip] == '-') {
	    x1 = LAST						# [-*
	    x2 = FIRST
	    ip = ip + 1
	    if (section[ip] == '*')
		ip = ip + 1

	} else if (section[ip] == '*')				# [*
	    ip = ip + 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get sample step size, if give.
	if (section[ip] == ':') {				# ..:step
	    ip = ip + 1
	    if (ctol (section, ip, step) == 0)
		goto synerr_
	    else if (step == 0)
		goto synerr_
	}

	# Allow notation such as "-*:5", (or even "-:5") where the step
	# is obviously supposed to be negative.

	if (x1 > x2 && step > 0)
	    step = -step
	
	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	if (section[ip] == ',') {
	    ip = ip + 1
	    return
	} else if (section[ip] == ']')
	    return

synerr_
	# Syntax error in image section specification.
 	call imerr (section, SYS_IMSYNSEC)
end
