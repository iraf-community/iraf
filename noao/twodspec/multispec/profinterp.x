include	"ms.h"

.help profile_interpolation Jul84 MULTISPEC
     The input to this procedure are the intensity profiles and the derivatives
of the profiles with position for each spectrum at two sample lines y(2) and
y(3).  The profiles are gridded on unit position intervals starting at two
different points x(2) and x(3).  Let us denote the i_th point in these profiles
(for some given spectrum) by

	I(x(j)+i,y(j)), dI/dx(x(j)+i,y(j))

where j takes the values 2 and 3 in the remaining discussion.
Note that the profiles contain dI/dx0, the derivative with respect to the
profile center.  This is related to the derivative with respect to x by

	dI/dx = -dI/dx0

     We want interpolated profiles at line y(1) gridded with a starting point
x(1).  Denote this profile by

	   I(x(1)+i,y(1))

     The algorithm is to first interpolate to the point x(1)+i from each of
the two neighboring points at each endpoint.  This yields the quantities:

.nf
(1)	a(j) = I(x(j)+ileft,y(j))  + dI/dx(x(j)+ileft,y(j))  * dxa(j)
	b(j) = I(x(j)+iright,y(j)) + dI/dx(x(j)+iright,y(j)) * dxb(j)
.fi

where

.nf
(2)	dxa(j) = x(1) - x(j)		x(1) > x(j)
	dxb(j) = x(1) - (x(j) + 1)	x(1) > x(j)
	dxa(2) = x(1) - (x(j) - 1)	x(1) < x(j)
	dxb(2) = x(1) - x(j)		x(1) < x(j)
.fi

The final value is then obtained by the bi-linear interpolation formula:

.nf
(3)	I(x(1)+i,y(1)) = a(2) * wta(2) + b(2) * wtb(2) +
			 a(3) * wta(3) + b(3) * wtb(3)
.fi

where

.nf
(4)	f(2) = 1 - (y(1) - y(2)) / (y(3) - y(2))
	f(3) = 1 - (y(3) - y(1)) / (y(3) - y(2)) = 1 - f(2)
	wta(j) = -dxb(j) * f(j)
	wtb(j) = dxa(j) * f(j)
.fi

     If x(1) > x(j) then b(j) does not exist at the rightmost profile point.
In this case in equation 1 replace the term

.nf
(5)	a(j) * wta(j) + b(j) * wtb(j)
.fi

with

.nf
(6)	a(j) * f(j)
.fi

for the rightmost endpoint.
Similarly, if x(1) < x(j) then a(j) does not exist for the leftmost profile
point.  Then replace the term (5) with

.nf
(7)	b(j) * f(j).
.fi

     Procedure profile_interpolation implements this interpolation scheme.
The only difference is that instead of equation 3 the profiles are built up
by accumulation of the terms.
.endhelp

# PROFILE_INTERPOLATION -- Interpolate between two profiles.
#
# The equation references are to those in the help text.

procedure profile_interpolation (fraction, len_profile, nspectra, nparams,
    profiles, ranges)

real	fraction			# The interpolation point
int	len_profile			# The length of the profiles
int	nspectra			# The number of spectra
int	nparams				# The number of model parameters
real	profiles[len_profile, nspectra, nparams, 3] # The profiles
real	ranges[nspectra, LEN_RANGES, 3]	# The ranges array

int	i, j, spectrum
real	dx, f[3], dxa[3], dxb[3], wta[3], wtb[3], a, b

begin
	# Clear the final profiles because we accumulate the terms in
	# equations 3 and 5.
	call aclrr (profiles[1, 1, I0_INDEX, 1], len_profile * nspectra)

	# Equation 4.
	f[2] = 1 - fraction
	f[3] = fraction

	# Do each endpoint and each spectrum.
	do j = 2, 3 {
	    do spectrum = 1, nspectra {
	        dx = ranges[spectrum, DX_START, 1] -
		    ranges[spectrum, DX_START, j]

	        if (dx < 0.) {
		    # x(1) < x(j) and ileft = i - 1, iright = i.

		    # Equation 2.
		    dxa[j] = 1 + dx
		    dxb[j] = dx

		    # Equation 4.
		    wta[j] = -dxb[j] * f[j]
		    wtb[j] = dxa[j] * f[j]

		    # Accumulate the terms from the left neighbor.  Eq. 1 & 3
	            do i = 2, len_profile {
		        a = profiles[i - 1, spectrum, I0_INDEX, j] -
		            profiles[i - 1, spectrum, X0_INDEX, j] * dxa[j]
		        profiles[i, spectrum, I0_INDEX, 1] =
			    profiles[i, spectrum, I0_INDEX, 1] + a * wta[j]
		    }

		    # Accumulate the terms from the right neighbor.  Eq. 1 & 3
	            do i = 2, len_profile {
		        b = profiles[i, spectrum, I0_INDEX, j] -
		            profiles[i, spectrum, X0_INDEX, j] * dxb[j]
		        profiles[i, spectrum, I0_INDEX, 1] =
			    profiles[i, spectrum, I0_INDEX, 1] + b * wtb[j]
		    }

		    # There is no left neighbor for the left profile endpoint.
		    # Eq. 1 & 7
		    b = profiles[1, spectrum, I0_INDEX, j] -
		        profiles[1, spectrum, X0_INDEX, j] * dxb[j]
		    profiles[1, spectrum, I0_INDEX, 1] =
			profiles[1, spectrum, I0_INDEX, 1] + b * f[j]
	        }

	        else {
		    # x(1) > x(j) and ileft = i, iright = i + 1.
		    # Equation 2.
		    dxa[j] = dx
		    dxb[j] = dx - 1

		    # Equation 4.
		    wta[j] = -dxb[j] * f[j]
		    wtb[j] = dxa[j] * f[j]

		    # Accumulate the terms from the left neighbor. Eq. 1 & 3.
	            do i = 1, len_profile - 1 {
		        a = profiles[i, spectrum, I0_INDEX, j] -
		            profiles[i, spectrum, X0_INDEX, j] * dxa[j]
		        profiles[i, spectrum, I0_INDEX, 1] =
			    profiles[i, spectrum, I0_INDEX, 1] + a * wta[j]
		    }

		    # Accumulate the terms from the right neighbor. Eq. 1 & 3.
	            do i = 1, len_profile - 1 {
		        b = profiles[i + 1, spectrum, I0_INDEX, j] -
		            profiles[i + 1, spectrum, X0_INDEX, j] * dxb[j]
		        profiles[i, spectrum, I0_INDEX, 1] =
			    profiles[i, spectrum, I0_INDEX, 1] + b * wtb[j]
		    }

		    # There is no right neighbor for the right profile endpoint.
		    # Eq. 1 & 6
		    a = profiles[len_profile, spectrum, I0_INDEX, j] -
		        profiles[len_profile, spectrum, X0_INDEX, j] * dxa[j]
		    profiles[len_profile, spectrum, I0_INDEX, 1] =
			profiles[len_profile, spectrum, I0_INDEX, 1] + a * f[j]
	        }
	    }
	}
	call amaxkr (profiles[1, 1, I0_INDEX, 1], 0.,
	    profiles[1, 1, I0_INDEX, 1], len_profile * nspectra)
end
