define	MBIG 	1000000000
define 	MSEED	161803398
define	MZ 	0.0
define	FAC	1.0 / MBIG

# RAN3 -- Returns a uniform random deviate between 0.0 and 1.0. Set 
# 'idum' to any negative value to initialize or reinitialize the sequence.
# From Numerical Recipes (originally attributed to Donald Knuth, 1981;
# Seminumerical Algorithms, 2nd edition, volume 2 of 'The Art of Computer
# Programming' - Section 3.2-3.3.

real procedure ran3 (idum)

int	idum

int	ma[55]
int	mj, mk, i, k, ii
int	iff, inext, inextp
data	iff /0/

begin
	if(idum < 0 || iff == 0) {
	    iff = 1
	    mj = MSEED - iabs(idum)
	    mj = mod(mj, MBIG)
	    ma[55] = mj
	    mk = 1

	    do i = 1, 54 {
		ii = mod(21 * i , 55)
		ma[ii] = mk
		mk = mj - mk
		if (mk < MZ)
		    mk = mk + MBIG
		mj = ma[ii]
	    }

	    do k = 1, 4 {
		do i = 1, 55 {
		    ma[i] = ma[i] - ma[1+mod(i+30, 55)]
		    if (ma[i] < MZ)
			ma[i] = ma[i] + MBIG
		}
	    }

	    inext = 0
	    inextp = 31
	    idum = 1
	}

	inext = inext + 1
	if (inext == 56)
	    inext = 1
	inextp = inextp + 1
	if (inextp == 56)
	    inextp = 1
	mj = ma[inext] - ma[inextp]
	if (mj < MZ)
	    mj = mj + MBIG
	ma[inext]= mj
	return (mj * FAC)

end
