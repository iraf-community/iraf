define	TINY	(1.0e-15)

# INVERS
#
# Although it seems counter-intuitive, the tests that I have run
# so far suggest that the 180 x 180 matrices that NSTAR needs can
# be inverted with sufficient accuracy if the elements are REAL*4
# rather than REAL*8.
#
# Arguments
# 
#     a (input/output) is a square matrix of dimension N.  The inverse 
#       of the input matrix A is returned in A.
#
#   nmax (input) is the size assigned to the matrix A in the calling 
#       routine.  It is needed for the dimension statement below.
#
# iflag (output) is an error flag.  iflag  =  1 if the matrix could not
#       be inverted; iflag  =  0 if it could.
#
# This is an SPP translation of the original fortran version with the
# addition of a check for tiny numbers which could cause an FPE.

procedure invers2 (a, nmax, n, iflag)

real	a[nmax,nmax]
int	nmax
int	n
int	iflag

int	i, j, k

begin
      # Check for tiny numbers.
      do i = 1, n
        do j = 1, n
	  if (abs (a[i,j]) < TINY)
	      a[i,j] = 0e0

      # Original code.
      iflag = 0
      i = 1
   30 if (a[i,i] .eq. 0.0e0) goto 91
      a[i,i] = 1.0e0 / a[i,i]
      j = 1
   31 if (j .eq. i) goto  34
      a[j,i] = -a[j,i] * a[i,i]
      k = 1
   32 if (k .eq. i) goto  33
      a[j,k] = a[j,k] + a[j,i] * a[i,k]
   33 if (k .eq. n) goto  34
      k = k + 1
      goto  32
   34 if (j .eq. n) goto  35
      j = j + 1
      goto  31
   35 k = 1
   36 if (k .eq. i) goto  37
      a[i,k] = a[i,k] * a[i,i]
   37 if (k .eq. n) goto  38
      k = k + 1
      goto  36
   38 if (i .eq. n) return
      i = i+1
      goto  30

# Error:  zero on the diagonal.

 91 iflag = 1
      return

end
