define	LEN_IR	97
define	IC	150889
define	M	714025
define	IA	1366
define	RM	1.400511e-6

# DAORAN -- The random number generator RAN2 from Numerical Recipes.

real procedure daoran (idum)

int	idum			# seed for the random number generator

int	j, iff, iy, ir[LEN_IR]
real	rnum2
data	iff /0/

begin
	repeat {

	    # Initialize the random number generator.
	    if ((idum < 0) || (iff == 0)) {
		iff = 1
		idum = mod (abs (IC - idum), M)
		do j = 1, LEN_IR {
		    idum = mod (IA * idum + IC, M)
		    ir[j] = idum
		}
		idum = mod (IA * idum + IC, M)
		iy = idum
	    }

	    # Get the random number
	    j = 1 + (LEN_IR * iy) / M
	    j = max (1, min (LEN_IR, j))
	    iy = ir[j]
	    rnum2 = iy * RM
	    idum = mod (IA * idum + IC, M)
	    ir[j] = idum

	} until (rnum2 > 0.0)

	return (rnum2)
end
