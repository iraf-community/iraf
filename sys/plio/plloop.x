# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PLLOOP -- Increment the vector V from VS to VE (nested do loops cannot
# be used because of the variable number of dimensions).  Return LOOP_DONE
# when V exceeds VE.

int procedure plloop (v, vs, ve, ndim)

long	v[ndim]			#U vector giving current position in image
long	vs[ndim]		#I start vector
long	ve[ndim]		#I end vector
int	ndim			#I vector length

int	i

begin
	do i = 2, ndim {
	    v[i] = v[i] + 1
	    if (v[i] > ve[i]) {
		if (i < ndim)
		    v[i] = vs[i]
		else
		    break
	    } else
		return (LOOP_AGAIN)
	}

	return (LOOP_DONE)
end
