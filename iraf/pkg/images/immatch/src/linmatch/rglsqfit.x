include <mach.h>
include "lsqfit.h"

# LL_RLSQF1 -- Given an initial fit reject points outside of the low and
# high cut rejections parameters.

procedure ll_rlsqf1 (x, y, xerr, yerr, weight, npts, maxiter, answers, nreject,
	locut, hicut)

real	x[ARB]				#I the input vector
real	y[ARB]				#I the reference vector
real	xerr[ARB]			#I the input vector errors squared
real	yerr[ARB]			#I the reference vector errors squared
real	weight[ARB]			#I the input weight array
int	npts				#I the number of points
int	maxiter				#I the number of iterations
real	answers[ARB]			#I/O the answers array
int	nreject				#I the max number of rejection cycles
real	locut				#I the low side rejection parameter
real	hicut				#I the high side rejection parameter

int	i, niter, nrej
real	loval, hival, resid

begin
	if ((IS_INDEFR(locut) && IS_INDEFR(hicut)) || npts <= 2)
	    return
	if (RMS[answers] <= 0.0 || IS_INDEFR(CHI[answers]))
	    return

	niter = 0
	repeat {
	    if (IS_INDEFR(locut))
		loval = -MAX_REAL
	    else
		loval = -locut * RMS[answers]
	    if (IS_INDEFR(hicut))
		hival = MAX_REAL
	    else
		hival = hicut * RMS[answers]
	    nrej = 0
	    do i = 1, npts {
		if (weight[i] <= 0.0)
		    next
		resid = y[i] - (SLOPE[answers] * x[i] + YINCPT[answers])
		if (resid >= loval && resid <= hival)
		    next
		weight[i] = 0.0
		nrej = nrej + 1
	    }
	    if (nrej <= 0)
		break
	    call ll_lsqf1 (x, y, xerr, yerr, weight, npts, maxiter, answers)
	    if (IS_INDEFR(CHI[answers]))
	        break
	    if (RMS[answers] <= 0.0)
		break
	    niter = niter + 1
	} until (niter >= nreject)
end


# LL_LSQF1 -- Compute the slope and intercept of the equation y = a * x + b
# using error arrays in both x and y.

procedure ll_lsqf1 (x, y, xerr, yerr, weight, npts, niter, answers)

real	x[ARB]				#I the input vector
real	y[ARB]				#I the reference vector
real	xerr[ARB]			#I the input vector errors squared
real	yerr[ARB]			#I the reference vector errors squared
real	weight[ARB]			#I the input weight array
int	npts				#I the number of points
int	niter				#I the number of iterations
real	answers[ARB]			#I/O the answers array

int	i, j
pointer	bufr, bufx, bufw
real	slope, yintrcpt, me1, msq, wt, dm, db

begin
	# Peform the initial fit.
	call ll_0lsqf1 (x, y, weight, npts, answers)
	if (IS_INDEFR(CHI[answers]))
	    return

	# Allocate working space.
	call malloc (bufr, npts, TY_REAL)
	call malloc (bufx, npts, TY_REAL)
	call malloc (bufw, npts, TY_REAL)

	# Initialize the iterations.
	slope = SLOPE[answers]
	yintrcpt = YINCPT[answers]
	me1 = CHI[answers]

	# Iterate on the fit.
	do i = 1, niter {
	    msq = slope * slope
	    do j = 1, npts {
		if (weight[j] <= 0.0) {
		    Memr[bufr+j-1] = 0.0
		    Memr[bufw+j-1] = 0.0
		    Memr[bufx+j-1] = 0.0
		} else {
		    wt = yerr[j] + msq * xerr[j]
		    if (wt <= 0.0)
			wt = 1.0
		    else
			wt = 1.0 / wt
		    Memr[bufr+j-1] = y[j] - (slope * x[j] + yintrcpt)
		    Memr[bufw+j-1] = weight[j] * wt
		    Memr[bufx+j-1] = x[j] + Memr[bufr+j-1] * slope * xerr[j] *
		        wt
		}
	    }
	    call ll_0lsqf1 (Memr[bufx], Memr[bufr], Memr[bufw], npts, answers)
	    if (IS_INDEFR(CHI[answers]))
	        break
	    if (abs ((me1 - CHI[answers]) / CHI[answers]) < 1.0e-5)
		break
	    dm = SLOPE[answers]
	    db = YINCPT[answers]
	    me1 = CHI[answers]
	    slope = slope + dm
	    yintrcpt = yintrcpt + db
	}

	# Compute the final answers.
	SLOPE[answers] = slope
	YINCPT[answers] = yintrcpt

	call mfree (bufr, TY_REAL)
	call mfree (bufx, TY_REAL)
	call mfree (bufw, TY_REAL)
end


# LL_0LSQF1: Compute the slope and intercept of the equation y = a * x + b
# using errors in y only.

procedure ll_0lsqf1 (x, y, w, npts, answers)

real	x[ARB]				#I the input vector
real	y[ARB]				#I the reference vector
real	w[ARB]				#I the weight vector
int	npts				#I the number of points
real	answers[ARB]			#I the answers

int	i, ngood
double	sumyy, sumxx, sumxy, sumx, sumy, sumw
double	a, b, det
real	wressq, ressq
bool	fp_equald()
double 	ll_dsum1(), ll_dsum2(), ll_dsum3()

begin
	# Compute the determinant.
	sumyy = ll_dsum3 (y, y, w, npts)
	sumxx = ll_dsum3 (x, x, w, npts)
	sumxy = ll_dsum3 (x, y, w, npts)
	sumy = ll_dsum2 (y, w, npts)
	sumx = ll_dsum2 (x, w, npts)
	sumw = ll_dsum1 (w, npts)
	det = sumw * sumxx - sumx * sumx

	if (fp_equald (0.0d0, det)) {
	    SLOPE[answers] = INDEFR
	    YINCPT[answers] = INDEFR
	    ESLOPE[answers] = INDEFR
	    EYINCPT[answers] = INDEFR
	    CHI[answers] = INDEFR
	    RMS[answers] = INDEFR
	} else {
	    a = (sumw * sumxy - sumx * sumy) / det
	    b = (sumxx * sumy - sumx * sumxy) / det
	    ngood = 0.0
	    ressq = 0.0
	    do i = 1, npts {
	        if (w[i] > 0.0) {
		    ngood = ngood + 1
		    ressq = ressq + (y[i] - (a * x[i] + b)) ** 2
		}
	    }
	    SLOPE[answers] = a
	    YINCPT[answers] = b
	    wressq = sumyy + a * (a * sumxx + 2. * (b * sumx - sumxy)) +
	        b * (b * sumw - 2.0 * sumy)
	    if (ngood <= 2) {
		CHI[answers] = 0.0
		ESLOPE[answers] = 0.0
		EYINCPT[answers] = 0.0
		RMS[answers] = 0.0
	    } else if (wressq >= 0.0) {
		CHI[answers] = sqrt (wressq / (ngood - 2))
		ESLOPE[answers] = CHI[answers] * sqrt (real (sumw / abs(det)))
		EYINCPT[answers] = CHI[answers] * sqrt (real (sumxx / abs(det)))
		RMS[answers] = sqrt (ressq / (ngood - 2))
	    } else {
		CHI[answers] = 0.0
		ESLOPE[answers] = 0.0
		EYINCPT[answers] = 0.0
		RMS[answers] = 0.0
	    }
	}
end


## GET_LSQF2: iterate LSq Fit to z=ax+by+c for errors in x, y and z.
## NB: xerr, yerr, zerr are errors SQUARED.
##
#
#procedure get_lsqf2 (x, y, z, xerr, yerr, zerr, weight, npts, niter, stats)
#
#real	x[npts], y[npts], z[npts]		# data vectors
#real	xerr[npts], yerr[npts], zerr[npts]	# error ** 2 vectors
#real	weight[npts]				# additional weight factors
#int	npts					# vector lengths
#int	niter					# no. of iterations
#real	stats[NFITPAR]				# returned fit params
#
#int	i, j
#real	a, b, c, me1
#pointer	bufr, bufx, bufy, bufw
#real	asq, bsq, res, wt, da, db, dc
#
#begin
#	call malloc (bufr, npts, TY_REAL)
#	call malloc (bufx, npts, TY_REAL)
#	call malloc (bufy, npts, TY_REAL)
#	call malloc (bufw, npts, TY_REAL)
#
## initial fit; NB needs expansion
#	call get_0lsqf2 (x, y, z, weight, npts, stats)
#	a = SLOPE1[stats]
#	b = SLOPE2[stats]
#	c = OFFSET[stats]
#	me1 = CHI[stats]
##	call printf ("iteration: %2d  a=%7.4f b=%7.4f off=%6.2f (%7.3f) \n")
##		call pargi (0)
##		call pargr (a)
##		call pargr (b)
##		call pargr (c)
##		call pargr (me1)
#
## iterate
#	do i = 1, niter {
#		asq = a * a
#		bsq = b * b
#		do j = 1, npts {
#			res = z[j] - (a * x[j] + b * y[j] + c)
#			wt = 1. / (zerr[j] + asq * xerr[j] + bsq * yerr[j])
#			Memr[bufr+j-1] = res
#			Memr[bufw+j-1] = weight[j] * wt
#			Memr[bufx+j-1] = x[j] + res * a * xerr[j] * wt
#			Memr[bufy+j-1] = y[j] + res * b * yerr[j] * wt
#		}
#		call get_0lsqf2 (Memr[bufx], Memr[bufy], Memr[bufr], Memr[bufw], npts, stats)
#		da = SLOPE1[stats]
#		db = SLOPE2[stats]
#		dc = OFFSET[stats]
#		me1 = CHI[stats]
#		a = a + da
#		b = b + db
#		c = c + dc
##		call printf ("iteration: %2d  a=%7.4f b=%7.4f off=%6.2f (%7.3f) \n")
##			call pargi (i)
##			call pargr (a)
##			call pargr (b)
##			call pargr (c)
##			call pargr (me1)
#	}
#
#	SLOPE1[stats] = a
#	SLOPE2[stats] = b
#	OFFSET[stats] = c
#
#	call mfree (bufr, TY_REAL)
#	call mfree (bufx, TY_REAL)
#	call mfree (bufy, TY_REAL)
#	call mfree (bufw, TY_REAL)
#end
#
##
## GET_0LSQF2 -- calculate the zeroth order LLSq Fit for 2 independent variables,
## assumming errors in z only
##
#
#	procedure get_0lsqf2 (x, y, z, w, npt, stats)
#
#real	x[npt], y[npt]				# input coords
#real	z[npt]					# ref. coord.
#real	w[npt]					# weights
#int	npt					# number of points
#real	stats[NFITPAR]				# fit info struct
#
#real	ga[4, 3]
#
#double	dsum1(), dsum2(), dsum3()
#
#begin
#	ga[1,1] = dsum3 (x, x, w, npt)
#	ga[2,1] = dsum3 (x, y, w, npt)
#	ga[2,2] = dsum3 (y, y, w, npt)
#	ga[3,1] = dsum2 (x, w, npt)
#	ga[3,2] = dsum2 (y, w, npt)
#	ga[4,1] = dsum3 (x, z, w, npt)
#	ga[4,2] = dsum3 (y, z, w, npt)
#	ga[4,3] = dsum2 (z, w, npt)
#	ga[3,3] = dsum1 (w, npt)
#
#	ga[1,2] = ga[2,1]
#	ga[1,3] = ga[3,1]
#	ga[2,3] = ga[3,2]
#
#	call g_elim(ga, 3)
#
#	SLOPE1[stats] = ga[4,1]
#	SLOPE2[stats] = ga[4,2]
#	OFFSET[stats]  = ga[4,3]
##need to define errors, me1
#	EOFFSET[stats] = INDEF
#	ESLOPE1[stats] = INDEF
#	ESLOPE2[stats] = INDEF
#	CHI[stats] = INDEF
#end
#


# LL_LLSQF0 -- Compute the offset b in the equation y - x = b using error
# arrays in both x and y.

#procedure ll_lsqf0 (x, y, xerr, yerr, w, npts, answers)

#real	x[ARB]				#I the input vector
#real	y[ARB]				#I the reference vector
#real	xerr[ARB]			#I the input vector errors squared
#real	yerr[ARB]			#I the reference vector errors squared
#real	w[ARB]				#I the input weight vector
#int	npts				#I the number of points
#real	answers[ARB]			#I the answer vector

#double	sumxx, sumx, sumw
#pointer	bufr, bufw
#double	ll_dsum1(), ll_dsum2(), ll_dsum3()

#begin
#	# Allocate working space.
#	call malloc (bufr, npts, TY_REAL)
#	call malloc (bufw, npts, TY_REAL)
#
#	call asubr (y, x, Memr[bufr], npts)
#	call aaddr (yerr, xerr, Memr[bufw], npts)
#	call adivr (w, Memr[bufw], Memr[bufw], npts)
#
#	sumxx = ll_dsum3 (Memr[bufr], Memr[bufr], Memr[bufw], npts)
#	sumx = ll_dsum2 (Memr[bufr], Memr[bufw], npts)
#	sumw = ll_dsum1 (Memr[bufw], npts)
#
#	if (sumw <= 0.0d0) {
#	    OFFSET[answers] = INDEFR
#	    EOFFSET[answers] = INDEFR
#	    CHI[answers] = INDEFR
#	} else {
#	    OFFSET[answers] = sumx / sumw
#	    if (npts > 1) {
#	        CHI[answers] = sqrt (real ((sumxx - sumx * sumx / sumw) /
#	            (npts - 1)))
#	        EOFFSET[answers] = CHI[answers] / sqrt (real (sumw))
#	    } else {
#	        CHI[answers] = 0.0
#	        EOFFSET[answers] = 0.0
#	    }
#	}
#
#	# Free working space.
#	call mfree (bufr, TY_REAL)
#	call mfree (bufw, TY_REAL)
#end


# LL_DSUM1 -- Compute a double precision vector sum.

double	procedure ll_dsum1 (a, n)

real	a[ARB]			#I the input vector
int	n			#I the number of points

double	sum
int	i

begin
	sum = 0.0d0
	do i = 1, n
	    sum = sum + a[i]

	return (sum)
end


# LL_DSUM2 -- Compute a double precision vector product.

double	procedure ll_dsum2 (a, b, n)

real	a[n]		#I the input vector 
real	b[n]		#I the weight vector
int	n		#I the number of points

double	sum
int	i

begin
	sum = 0.0d0
	do i = 1, n {
	    if (b[i] > 0.0)
	        sum = sum + a[i] * b[i]
	}

	return (sum)
end


# LL_DSUM3 -- Compute a double precision weighted dot product.


double	procedure ll_dsum3 (a, b, c, n)

real	a[n]			#I first input vector
real	b[n]			#I second input vector
real	c[n]			#I input weight vector
int	n			#I the number of points

double	sum
int	i

begin
	sum = 0.0d0
	do i = 1, n
	    if (c[i] > 0.0)
	        sum = sum + a[i] * b[i] * c[i]

	return (sum)
end
