include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_CLEAN -- Procedure to clean a data array.

procedure ap_clean (ap, pix, nx, ny, xc, yc)

pointer	ap		# pointer to the apphot structure
real	pix[nx,ny]	# data array
int	nx, ny		# size of subarray
real	xc, yc		# center of subarray

int	apstati()
real	apstatr()

begin
	switch (apstati (ap, NOISEFUNCTION)) {
	case AP_NCONSTANT:
	    call ap_cclean (pix, nx, ny, xc, yc, apstatr (ap, SCALE) *
	        apstatr (ap, RCLIP), apstatr (ap, SIGMACLEAN),
		apstatr (ap, SKYSIGMA), apstatr (ap, MAXSHIFT))
	case AP_NPOISSON:
	    call ap_pclean (pix, nx, ny, xc, yc, apstatr (ap, SCALE) *
		apstatr (ap, RCLEAN), apstatr (ap, SCALE) * apstatr (ap,
		RCLIP), apstatr (ap, SIGMACLEAN), apstatr (ap, SKYSIGMA),
		apstatr (ap, EPADU), apstatr (ap, MAXSHIFT))
	default:
	    return
	}
end


# AP_CCLEAN -- Procedure to clean the subraster assuming the noise is
# due to a constant sky sigma.

procedure ap_cclean (pix, nx, ny, cxc, cyc, rclip, kclean, skysigma,
    maxshift)

real	pix[nx, ny]		# array of pixels
int	nx, ny			# dimensions of the subarray
real	cxc, cyc		# initial center
real	rclip			# cleaning and clipping radius
real	kclean			# k-sigma clipping factor
real	skysigma		# maxshift
real	maxshift		# maximum shift

int	i, ii, ixc, j, jj, jyc, ijunk, jjunk
real	mindat, maxdat, rclip2, r2, ksigma 

begin
	# Return if indef valued sigma or sigma <= 0.
	if (IS_INDEFR(skysigma) || (skysigma <= 0.0))
	    return

	# Find the maximum pixel in the subarray and treat this point as
	# the center of symmetry if it is less than maxshift from the
	# initial center.

	call ap_2dalimr (pix, nx, ny, mindat, maxdat, ijunk, jjunk, ixc, jyc)
	if (abs (cxc - ixc) > maxshift || abs (cyc - jyc) > maxshift) {
	    ixc = int (cxc)
	    jyc = int (cyc)
	}

	# Clip.
	rclip2 = rclip ** 2
	ksigma = kclean * skysigma
	do j = 1, ny {
	    jj = 2 * jyc - j
	    if (jj < 1 || jj > ny)
		next
	    do i = 1, ixc {
		ii = 2 * ixc - i
		if (ii < 1 || ii > nx)
		    next
		r2 = (i - ixc) ** 2 + (j - jyc) ** 2
		if (r2 > rclip2) {
		    if (pix[ii,jj] > pix[i,j] + ksigma)
			pix[ii,jj] = pix[i,j]
		    else if (pix[i,j] > pix[ii,jj] + ksigma)
			pix[i,j] = pix[ii,jj]
		}
	    }
	}
end


# AP_PCLEAN -- Procedure to clean the subraster assuming that the noise is
# due to a constant sky value plus poisson noise.

procedure ap_pclean (pix, nx, ny, cxc, cyc, rclean, rclip, kclean, skysigma,
    padu, maxshift)

real	pix[nx, ny]		# array of pixels
int	nx, ny			# dimensions of the subarray
real	cxc, cyc		# initial center
real	rclean, rclip		# cleaning and clipping radius
real	kclean			# k-sigma clipping factor
real	skysigma		# maxshift
real	padu			# photons per adu
real	maxshift		# maximum shift

int	i, ii, ixc, j, jj, jyc, ijunk, jjunk
real	mindat, maxdat, rclean2, rclip2, r2, ksigma, ksigma2 

begin
	# Return if indef-valued sigma.
	if (IS_INDEFR(skysigma))
	    return

	# Find the maximum pixel in the subarray and treat this point as
	# the center of symmetry if it is less than maxshift from the
	# initial center.

	call ap_2dalimr (pix, nx, ny, mindat, maxdat, ijunk, jjunk, ixc, jyc)
	if (abs (cxc - ixc) > maxshift || abs (cyc - jyc) > maxshift) {
	    ixc = int (cxc)
	    jyc = int (cyc)
	}

	# Clip.
	rclean2 = rclean ** 2
	rclip2 = rclip ** 2
	ksigma = kclean * skysigma
	ksigma2 = ksigma ** 2
	do j = 1, ny {
	    jj = 2 * jyc - j
	    if (jj < 1 || jj > ny)
		next
	    do i = 1, ixc {
		ii = 2 * ixc - i
		if (ii < 1 || ii > nx)
		    next
		r2 = (i - ixc) ** 2 + (j - jyc) ** 2
		if (r2 > rclean2 && r2 <= rclip2) {
		    if (pix[ii,jj] > (pix[i,j] + sqrt (pix[i,j] / padu +
		        ksigma2)))
			pix[ii,jj] = pix[i,j]
		    else if (pix[i,j] > (pix[ii,jj] + sqrt (pix[ii,jj] / padu +
		        ksigma2)))
			pix[i,j] = pix[ii,jj]
		}
		if (r2 > rclip2) {
		    if (pix[ii,jj] > pix[i,j] + ksigma)
			pix[ii,jj] = pix[i,j]
		    else if (pix[i,j] > pix[ii,jj] + ksigma)
			pix[i,j] = pix[ii,jj]
		}
	    }
	}
end
