# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# URAND -- Uniform random number generator.  Write a list of random numbers
# to the standard output.  The number of columns per line of output and the
# number of significant digits in each number are parameterized.

procedure t_urand()

int	nlines			# number of lines of output
int	ncols			# number of random numbers per line
int	ndigits			# number of digits of precision
long	seed			# seed for the random number generator
real	scale_factor		# scale output numbers by this factor

int	n, i
int	clgeti()
long	clgetl(), clktime()
real	clgetr(), urand()

begin
	# Get parameters from the CL.
	nlines		= clgeti ("nlines")
	ncols		= clgeti ("ncols")
	ndigits		= clgeti ("ndigits")
	seed		= clgetl ("seed")
	scale_factor	= clgetr ("scale_factor")

	# Compute the random numbers and print on the standard output as
	# a list, "ncols" numbers per output line.  The output format
	# is dependent on the ndigits of precision, set by the user.

	if (IS_INDEFL(seed))
	    seed = clktime (long(0))

	for (n=1;  n <= nlines;  n=n+1) {
	    do i = 1, ncols {
		call printf ("%*.*g ")
		    call pargi (ndigits + 2)		# field width
		    call pargi (ndigits)		# precision
		    call pargr (urand (seed) * scale_factor)
	    }
	    call printf ("\n")
	}
end
