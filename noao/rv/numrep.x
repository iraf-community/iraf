include <math.h>
include <mach.h>

# NUMREP.X - A collection of routines recoded from Numerical Recipes by 
# Press, Flannery, Teukolsky, and Vetterling.  Used by permission of the 
# authors.  Copyright(c) 1986 Numerical Recipes Software.


# FOUR1 -  Replaces DATA by it's discrete transform, if ISIGN is input
# as 1; or replaces DATA by NN times it's inverse discrete Fourier transform
# if ISIGN is input as -1.  Data is a complex array of length NN or, equiv-
# alently, a real array of length 2*NN.  NN *must* be an integer power of
# two.

procedure four1 (data, nn, isign)

real	data[ARB]			#U Data array (returned as FFT)
int	nn				#I No. of points in data array
int	isign				#I Direction of transform

double 	wr, wi, wpr, wpi		# Local variables
double	wtemp, theta
real 	tempr, tempi
int	i, j, istep
int	n, mmax, m

begin
	n = 2 * nn
	j = 1
	for (i=1; i<n; i = i  +  2) {
	  if (j > i) {				# Swap 'em
	    tempr = data[j]
	    tempi = data[j+1]
	    data[j] = data[i]
	    data[j+1] = data[i+1]
	    data[i] = tempr
	    data[i+1] = tempi
	  }
	  m = n / 2
	  while (m >= 2 && j > m) {
	    j = j - m
	    m = m / 2
	  }
	  j = j + m
	}
	mmax = 2
	while (n > mmax) {
	  istep = 2 * mmax
	  theta = TWOPI / double (isign*mmax)
	  wtemp = dsin (0.5*theta)
	  wpr = -2.d0 * wtemp * wtemp
	  wpi = dsin (theta)
	  wr = 1.d0
	  wi = 0.d0
	  for (m=1; m < mmax; m = m  +  2) {
	    for (i=m; i<=n; i = i  +  istep) {
		j = i + mmax
		tempr = real (wr) * data[j] - real (wi) * data[j+1]
		tempi = real (wr) * data[j + 1] + real (wi) * data[j]
		data[j] = data[i] - tempr
		data[j+1] = data[i+1] - tempi
		data[i] = data[i] + tempr
		data[i+1] = data[i+1] + tempi
	    }
	    wtemp = wr
	    wr = wr * wpr - wi * wpi + wr
	    wi = wi * wpr + wtemp * wpi + wi
	  } 
	  mmax = istep
	}
end


# REALFT - Calculates the Fourier Transform of a set of 2N real valued
# data points.  Replaces this data (which is stored in the array DATA) by
# the positive frequency half of it's complex Fourier Transform.  The real
# valued first and last components of the complex transform are returned
# as elements DATA(1) and DATA(2) respectively.  N must be an integer power
# of 2.  This routine also calculates the inverse transform of a complex
# array if it is the transform of real data.  (Result in this case must be
# multiplied by 1/N). A forward transform is perform for isign == 1, other-
# wise the inverse transform is computed.

procedure realft (data, N, isign)

real 	data[ARB]			#U Input data array & output FFT
int	N				#I No. of points
int 	isign				#I Direction of transfer

double	wr, wi, wpr, wpi, wtemp, theta	# Local variables
real	c1, c2, h1r, h1i, h2r, h2i
real	wrs, wis
int	i, i1, i2, i3, i4 
int	N2P3

begin
					# Initialize
	theta = PI/double(N)
	c1 = 0.5
	
	if (isign == 1) {
	  c2 = -0.5
	  call four1 (data,n,1)		# Forward transform is here
	} else {
	  c2 = 0.5
	  theta = -theta
	} 

	wtemp = sin (0.5 * theta)
	wpr = -2.0d0 * wtemp * wtemp
	wpi = dsin (theta)
	wr = 1.0D0  +  wpr
	wi = wpi
	n2p3 = 2*n + 3

	for (i=2; i<=n/2; i = i  +  1) {
	  i1 = 2 * i - 1
	  i2 = i1 + 1
	  i3 = n2p3 - i2
	  i4 = i3 + 1
	  wrs = sngl (wr)
	  wis = sngl (wi)
	  			# The 2 transforms are separated out of Z
	  h1r = c1 * (data[i1] + data[i3])	
	  h1i = c1 * (data[i2] - data[i4])
	  h2r = -c2 * (data[i2] + data[i4])
	  h2i = c2 * (data[i1] - data[i3])
				# Here they are recombined to form the true
				# transform of the original real data.
	  data[i1] = h1r + wr*h2r - wi*h2i
	  data[i2] = h1i + wr*h2i + wi*h2r
	  data[i3] = h1r - wr*h2r + wi*h2i
	  data[i4] = -h1i + wr*h2i + wi*h2r

	  wtemp = wr		# The reccurrence
	  wr = wr * wpr - wi * wpi + wr
	  wi = wi * wpr + wtemp * wpi + wi
	}

	if (isign == 1) {
	  h1r = data[1]
	  data[1] = h1r + data[2]
	  data[2] = h1r - data[2]
	} else {
	  h1r = data[1]
	  data[1] = c1 * (h1r + data[2])
	  data[2] = c1 * (h1r - data[2])
	  call four1 (data,n,-1)
	}

end


# TWOFFT - Given two real input arrays DATA1 and DATA2, each of length
# N, this routine calls cc_four1() and returns two complex output arrays,
# FFT1 and FFT2, each of complex length N (i.e. real length 2*N), which
# contain the discrete Fourier transforms of the respective DATAs.  As
# always, N must be an integer power of 2.

procedure twofft (data1, data2, fft1, fft2, N)

real	data1[ARB], data2[ARB]			#I Input data arrays
real	fft1[ARB], fft2[ARB]			#O Output FFT arrays
int	N					#I No. of points

int	nn3, nn2, jj, j
real	rep, rem, aip, aim

begin
	nn2 = 2  +  N  +  N
	nn3 = nn2  +  1

	jj = 2
	for (j=1; j <= N; j = j  +  1) {
	    fft1[jj-1] = data1[j]	# Pack 'em into one complex array
	    fft1[jj] = data2[j]
	    jj = jj  +  2
	}

	call four1 (fft1, N, 1)		# Transform the complex array
	fft2[1] = fft1[2]
	fft2[2] = 0.0
	fft1[2] = 0.0
	for (j=3; j <= N + 1; j = j  +  2) {
	    rep = 0.5 * (fft1[j]  +  fft1[nn2-j])
	    rem = 0.5 * (fft1[j] - fft1[nn2-j])
	    aip = 0.5 * (fft1[j + 1]  +  fft1[nn3-j])
	    aim = 0.5 * (fft1[j + 1] - fft1[nn3-j])
	    fft1[j] = rep
	    fft1[j+1] = aim
	    fft1[nn2-j] = rep
	    fft1[nn3-j] = -aim
	    fft2[j] = aip
	    fft2[j+1] = -rem
	    fft2[nn2-j] = aip
	    fft2[nn3-j] = rem
	}

end
