include	<mach.h>

# LSTSQ -- Do a least squares fit to the data contained in the zz array.
# Algorithm is from Jack Harvey. (Yes, it's a black box...)

procedure lstsq (zz, mz, fno)

real	zz[mz, mz]
int	mz
real	fno

int	n, m, m1, i, j, k, l, l1
real	fn, pp

begin
	n = mz - 2
	m = n + 1
	m1 = m + 1
	fn = n

	do i = 1, m {
	    l = i + 1
	    do k = 1, i-1 {
		zz[i,l] = zz[i,l] - zz[k,l]**2
	    }

	    if (i == m)
		break
	    if (zz[i,l] >= 0.0)
	        zz[i,l] = zz[i,l]**.5
	    else {
		call eprintf ("square root of negitive number in lstsq\n")
	        zz[i,l] = 0.0
	    }
	    l1 = l + 1

	    do j = l1, m1 {
		do k = 1, i-1 {
		    zz[i,j] = zz[i,j] - zz[k,l] * zz[k,j]
		}
		if (zz[i,l] >= EPSILONR)
		    zz[i,j] = zz[i,j] / zz[i,l]
		else
		    call eprintf ("divide by zero in lstsq\n")
	    }

	    if (zz[i,l] >= EPSILONR)
	        zz[i,i] = 1. / zz[i,l]
	    else
		call eprintf ("divide by zero in lstsq\n")
	    do j = 1, i-1 {
		pp = 0.
		l1 = i - 1
		do k = j, l1 {
		    pp = pp + zz[k,l] * zz[k,j]
		}
	        zz[i,j] = -zz[i,i] * pp
	    }
	}

	if ((fno - fn) >= EPSILONR)
	    if ((zz[m,m1] / (fno - fn)) >= 0.0)
	        zz[m1,m1] = .6745 * (zz[m,m1] / (fno - fn))**.5
	    else {
		call eprintf ("square root of negitive number in lstsq\n")
	        zz[m1,m1] = 0.0
	    }
	else
	    call eprintf ("divide by zero in lstsq\n")

	do i = 1, n {
	    zz[m,i] = 0.
	    pp = 0.
	    do j = i, n {
		zz[m,i] = zz[m,i] + zz[j,i] * zz[j,m1]
		pp = pp + zz[j,i] * zz[j,i]
	    }
	    if (pp >= 0.0)
	        zz[m1,i] = zz[m1,m1] * pp**.5
	    else {
		call eprintf ("square root of negitive number in lstsq\n")
	        zz[m1,i] = 0.0
	    }
	}
end
