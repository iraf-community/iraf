# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CNV_ABOXR -- Vector boxcar smooth.

procedure cnv_aboxr (in, out, npix, knpix)

real	in[npix+knpix-1]
real	out[npix]
int	npix, knpix

int	i
real	sum

begin
	sum = 0.0
	do i = 1, knpix - 1
	    sum = sum + in[i]

	do i = 1, npix {
	    sum = sum + in[i+knpix-1] 
	    out[i] = sum
	    sum = sum - in[i]
	}
end
