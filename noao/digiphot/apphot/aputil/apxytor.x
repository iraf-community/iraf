# AP_XYTOR -- Procedure to change the single integer coord of the sky pixels
# to a radial distance value. The integer coordinate is equal to
# coord = (i - xc + 1) + blklen * (j - yc).

procedure ap_xytor (coords, r, nskypix, xc, yc, blklen)

int	coords[ARB]		# coordinate array
real	r[ARB]			# radial coordinates
int	nskypix			# number of sky pixels
real	xc, yc			# center of sky subraster
int	blklen			# x dimension of sky subraster

int	i, x, y

begin
	do i = 1, nskypix {
	    x = real (mod (coords[i], blklen))
	    if (x == 0)
		x = blklen
	    y = (coords[i] - x) / blklen + 1 
	    r[i] = sqrt ((x - xc) ** 2 + (y - yc) ** 2)
	}
end
