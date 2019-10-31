#-----------------------------------------------------------------------
# subroutine:  fft842
# fast fourier transform for n=2**m
# complex input
#-----------------------------------------------------------------------
# this program replaces the vector z=x+iy by its  finite
# discrete, complex fourier transform if in=0.  the inverse transform
# is calculated for in=1.
#
# the subroutine is called as subroutine fft842 (in,n,x,y,ier).
# the integer n, the n real and complex location array x and
# the n real location array y must be supplied to the subroutine.

procedure fft842(in, n, x, y, ier)
int in, n, ier
real x[ARB], y[ARB]

int j
pointer sp, wsave, fft

begin
    call smark(sp)
    call salloc(fft, 2*n, TY_REAL)
    call salloc(wsave, 2*n+15, TY_REAL)
    call cffti(n, Memr[wsave])
    ier = 0
    if (in == 1) {
      Memr[fft + 2] = 0.0
      do j=1, n-1 {
        Memr[fft + 2*j-1] = x[j]/n
        Memr[fft + 2*j] = -y[j+1]/n
      }
      Memr[fft + 2*n-1] = x[n]/n
      Memr[fft + 2*n] = 0
      call cfftb(N, Memr[fft], Memr[wsave])
      do j=1, n {
        x[j] = Memr[fft + 2*j-1]
  	y[j] = Memr[fft + 2*j]
      }
    } else {
      do j=1, n {
          Memr[fft + 2*j-1] = x[j]
          Memr[fft + 2*j] = y[j]
      }
      call cfftf(N, Memr[fft], Memr[wsave])
      y[1] = 0
      do j=1, n-1 {
        x[j] = Memr[fft + 2*j-1]
        y[j+1] = -Memr[fft + 2*j]
      }
      x[n] = Memr[fft + 2*n-1]
    }
    call sfree(sp)
end
