include	<error.h>

# HD_POWERR -- Construct the basis functions for a power series function.
# Invoked from curfit as a user function.  Real version.

procedure hd_powerr (x, order, k1, k2, basis)

real	x		# array of data points
int	order		# order of polynomial, order = 1, constant
real	k1, k2		# normalizing constants - unused
real	basis[ARB]	# basis functions

int	i

begin
	do i = 1, order
	    iferr (basis[i] = x ** (i-1))
		call erract (EA_FATAL) 
end


# HD_POWERD -- Double version of above.

procedure hd_powerd (x, order, k1, k2, basis)

double	x		# array of data points
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants - unused
double	basis[ARB]	# basis functions

int	i

begin
	do i = 1, order
            iferr (basis[i] = x ** (i-1))
	        call erract (EA_FATAL)
end
