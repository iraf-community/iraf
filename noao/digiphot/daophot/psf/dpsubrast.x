include <imhdr.h>

# DP_SUBRAST -- Return a pointer to a subraster of an IRAF image.
# This procedure assumes that box_size is even.

pointer procedure dp_subrast (im, xcen, ycen, box_size, x1, x2, y1, y2)

pointer	im			# input image descriptor
int	xcen, ycen		# center of subraster
int	box_size		# size of box to extract
int	x1, y1, x2, y2		# boundaries of subraster

int	j, ncols
pointer	buf, ptr, imbuf
pointer imgs2r()

begin
	# Calculate start position of extraction box.
	x1 = xcen - box_size / 2  + 1
	y1 = ycen - box_size / 2  + 1
	x2 = x1 + box_size - 1
	y2 = y1 + box_size - 1
	if (x1 < 1 || x2 > IM_LEN(im,1) || y1 < 1 || y2 > IM_LEN(im,2))
	    return (NULL)
	call malloc (buf, (x2 - x1 + 1) * (y2 - y1 + 1), TY_REAL)

	ptr = buf
	ncols = x2 - x1 + 1
	do j = y1, y2 {
	    imbuf = imgs2r (im, x1, x2, j, j)
	    call amovr (Memr[imbuf], Memr[ptr], ncols) 
	    ptr = ptr + ncols
	}

	return (buf)
end
