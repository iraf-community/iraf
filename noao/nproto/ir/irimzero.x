
# IR_IMZERO -- Fill the output image with a constant value.

procedure ir_imzero (im, ncols, nlines, value)

pointer	im		# pointer to the output image
int	ncols		# number of columns
int	nlines		# number of lines
real	value		# default blank value

int	i
pointer	obuf
pointer	impl2r()

begin
	do i = 1, nlines {
	    obuf = impl2r (im, i)
	    if (obuf == EOF)
		call error (0, "Error writing output image.")
	    call amovkr (value, Memr[obuf], ncols)
	}
end
