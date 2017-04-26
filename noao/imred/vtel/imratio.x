# IMRATIO -- Divide two images and return the result in a third image.

procedure imratio (numerator, denominator, ratio, xdim, ydim)

real	numerator[xdim, ydim]		# input numerator
real	denominator[xdim, ydim]		# input denominator
real	ratio[xdim, ydim]		# output ratio image
int	xdim, ydim			# dimensions of the image

int	i
real	ezero()
extern	ezero()

begin
	do i = 1, ydim {
	    call arltr (denominator[1,i], xdim, 1E-10, 0.0)
	    call advzr (numerator[1,i], denominator[1,i], ratio[1,i], xdim,
		ezero)
	}
end


real procedure ezero (input)

real	input

begin
	return (0.0)
end
