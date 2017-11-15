include <math.h>
include <mach.h>

# NUMREP.X - A collection of routines API compatible to Numerical Recipes

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

real	data[ARB]			#U Input data array & output FFT
int	N				#I No. of points
int	isign				#I Direction of transfer

pointer sp, wsave
real last
int j
begin
    call smark(sp)
    call salloc(wsave, 4*N+15, TY_REAL)
    call rffti(2*N, Memr[wsave])

    if (isign == 1) {
        call rfftf(2*N, data, Memr[wsave])
        last = data[2*N]
        do j=2*N-1,3,-2 {
            data[j+1] = -data[j]
            data[j] = data[j-1]
        }
        data[2] = last
    } else {
        data[1] = data[1]/2.0
        last = data[2]/2.0
        do j=2,2*N-2,2 {
            data[j] = data[j+1]/2.0
            data[j+1] = -data[j+2]/2.0
        }
        data[2*N] = last
        call rfftb(2*N, data, Memr[wsave])
    }
    
    call sfree(sp)
end


# TWOFFT - Given two real input arrays DATA1 and DATA2, each of length
# N, this routine returns two complex output arrays, FFT1 and FFT2,
# each of complex length N (i.e. real length 2*N), which contain the
# discrete Fourier transforms of the respective DATAs.

procedure twofft (data1, data2, fft1, fft2, N)

real	data1[ARB], data2[ARB]			#I Input data arrays
real	fft1[ARB], fft2[ARB]			#O Output FFT arrays
int	N					#I No. of points

int j
pointer sp, wsave
begin
    call smark(sp)
    call salloc(wsave, 4*N+15, TY_REAL)
    call cffti(N, Memr[wsave])
    do j=1, N {
        fft1[2*j-1] = data1[j]
        fft1[2*j] = 0.0
        fft2[2*j-1] = data2[j]
        fft2[2*j] = 0.0
    }
    call cfftf(N, fft1, Memr[wsave])
    call cfftf(N, fft2, Memr[wsave])
    do j=1, N {
        fft1[2*j] = -fft1[2*j]
        fft2[2*j] = -fft2[2*j]
    }
    call sfree(sp)
end
