include <mach.h>
include <plset.h>
include "xyxymatch.h"

# RG_MATCH -- Compute the intersection of two lists using a pattern matching
# algorithm. This algorithm is based on one developed by Edward Groth
# 1986 A.J. 91, 1244. The algorithm matches pairs of coordinates from
# two lists based on the triangles that can be formed from triplets of
# points in each list. The algorithm is insensitive to coordinate translation,
# rotation, magnification, or inversion and can tolerate distortions and
# random errors.

int procedure rg_match (xref, yref, nref, xin, yin, nin, reftri, reftrirat,
	nreftri, nrmaxtri, nrefstars, intri, intrirat, nintri, ninmaxtri,
	nliststars, tolerance, ptolerance, ratio, nreject)

real	xref[ARB]		#I the reference x coordinates
real	yref[ARB]		#I the reference y coordinates
int	nref			#I the number of reference coordinates
real	xin[ARB]		#I the input x coordinates
real	yin[ARB]		#I the input y coordinates
int	nin			#I the number of input coordinates
int	reftri[nrmaxtri,ARB]	#U list of reference triangles
real	reftrirat[nrmaxtri,ARB]	#U list of reference triangle parameters
int	nreftri			#U number of reference triangles
int	nrmaxtri		#I maximum number of reference triangles
int	nrefstars		#I the number of reference stars
int	intri[ninmaxtri,ARB]	#U list of input triangles
real	intrirat[ninmaxtri,ARB]	#U list of input triangle parameters
int	nintri			#U number of input triangles
int	ninmaxtri		#I maximum number of input triangles
int	nliststars		#I the number of input stars
real	tolerance		#I the reference triangles matching tolerance
real	ptolerance		#I the input triangles matching tolerance
real	ratio			#I the maximum ratio of triangle sides
int	nreject			#I maximum number of rejection iterations

int	i, nmerge, nkeep, nmatch, ncheck
pointer	sp, rindex, lindex
int	rg_tmerge(), rg_treject(), rg_tvote(), rg_triangle

begin
	# Match the triangles in the input list to those in the reference list.
	if (nreftri < nintri)
	    nmerge = rg_tmerge (reftri, reftrirat, nreftri, nrmaxtri, intri,
	        intrirat, nintri, ninmaxtri)
	else
	    nmerge = rg_tmerge (intri, intrirat, nintri, ninmaxtri, reftri,
	        reftrirat, nreftri, nrmaxtri)
	if (nmerge <= 0)
	    return (0)

	# Perform the rejection cycle.
	nkeep = rg_treject (reftri, reftrirat, nreftri, nrmaxtri,
	    intri, intrirat, nintri, ninmaxtri, nmerge, nreject)
	if (nkeep <= 0)
	    return (0)

	# Match the coordinates.
	nmatch = rg_tvote (reftri, nrmaxtri, nrefstars, intri, ninmaxtri,
	    nliststars, nkeep)
	if (nmatch <= 0)
	    return (0)
	else if (nmatch <= 3 && nkeep < nmerge)
	    return (0)

	# If all the coordinates were not matched then make another pass
	# through the triangles matching algorithm. If the number of
	# matches decreases as a result of this then all the matches were
	# not true matches and declare the list unmatched.
	if (nmatch < min (nref, nin) && nmatch > 2) {

	    # Find the indices of the matched points.
	    call smark (sp)
	    call salloc (rindex, nmatch, TY_INT)
	    call salloc (lindex, nmatch, TY_INT)
	    do i = 1, nmatch {
		Memi[rindex+i-1] = reftri[i,RG_MATCH]
		Memi[lindex+i-1] = intri[i,RG_MATCH]
	    }

	    # Recompute the triangles.
	    nreftri = rg_triangle (xref, yref, Memi[rindex], nmatch, reftri,
		reftrirat, nrmaxtri, nrefstars, tolerance, ratio)
	    nintri = rg_triangle (xin, yin, Memi[lindex], nmatch, intri,
		intrirat, ninmaxtri, nliststars, ptolerance, ratio)

	    # Rematch the triangles.
	    if (nreftri < nintri)
	        nmerge = rg_tmerge (reftri, reftrirat, nreftri, nrmaxtri, intri,
	            intrirat, nintri, ninmaxtri)
	    else
	        nmerge = rg_tmerge (intri, intrirat, nintri, ninmaxtri, reftri,
	            reftrirat, nreftri, nrmaxtri)

	    # Reperform the rejection cycle.
	    if (nmerge > 0)
	        nkeep = rg_treject (reftri, reftrirat, nreftri, nrmaxtri,
	            intri, intrirat, nintri, ninmaxtri, nmerge, nreject)

	    # Reperform the vote.
	    if (nkeep > 0) {
	        ncheck = rg_tvote (reftri, nrmaxtri, nrefstars, intri,
	            ninmaxtri, nliststars, nkeep)
		if (ncheck <= 3 && nkeep < nmerge)
		    ncheck = 0
	    } else
		ncheck = 0

	    if (ncheck < nmatch)
		nmatch = 0
	    else
		nmatch = ncheck

	    call sfree (sp)
	}

	return (nmatch)
end
 

# RG_TRIANGLE -- Construct all the the possible triangles from
# an input coordinate list. The triangles are constructed in such a way
# that the shortest side of the triangle lies between vertices 1 and 2 and the
# longest side between vertices 1 and 3. The parameters of each triangle
# including the log of the perimeter, the ratio of the longest to shortest
# side, the cosine of the angle at vertex 1, the tolerances in the ratio
# and cosine and the sense of the triangle (clockwise or anti-clockwise)
# are also computed. Triangles with a ratio greater than maxratio are
# rejected as are triangles with vertices closer together than tolerance.

int procedure rg_triangle (xref, yref, refindex, nrefstars, reftri, tripar,
	nmaxtri, maxnpts, tolerance, maxratio)

real	xref[ARB]		#I x reference coordinates
real	yref[ARB]		#I y reference coordinates
int	refindex[ARB]		#I the reference list sort index
int	nrefstars		#I number of reference stars
int	reftri[nmaxtri,ARB]	#O reference triangles
real	tripar[nmaxtri,ARB]	#O triangle parameters
int	nmaxtri			#I maximum number of triangles
int	maxnpts			#I the maximum number of points
real	tolerance		#I matching tolerance
real	maxratio		#I maximum ratio of triangle sides

int	i, j, k, nsample, npts, ntri
real	rij, rjk, rki, dx1, dy1, dx2, dy2, dx3, dy3, r1, r2sq, r2, r3sq, r3
real	ratio, cosc, cosc2, sinc2, tol2, tol

begin
	# Create the tolerance.
	tol2 = tolerance ** 2
	nsample = max (1, nrefstars / maxnpts) 
	npts = min (nrefstars, nsample * maxnpts)

	# Construct the triangles.
	ntri = 1
	do i = 1, npts - 2 * nsample, nsample {
	    do j = i + nsample, npts - nsample, nsample {
		do k = j + nsample, npts, nsample {

		    # Compute the lengths of the three sides of the triangle,
		    # eliminating triangles with sides that are less than
		    # tolerance.
		    rij = (xref[refindex[i]] - xref[refindex[j]]) ** 2 +
		        (yref[refindex[i]] - yref[refindex[j]]) ** 2
		    if (rij <= tol2)
		        next
		    rjk = (xref[refindex[j]] - xref[refindex[k]]) ** 2 +
		        (yref[refindex[j]] - yref[refindex[k]]) ** 2
		    if (rjk <= tol2)
			next
		    rki = (xref[refindex[k]] - xref[refindex[i]]) ** 2 +
		        (yref[refindex[k]] - yref[refindex[i]]) ** 2
		    if (rki <= tol2)
			next

		    # Order the vertices with the shortest side of the triangle
		    # between vertices 1 and 2 and the intermediate side between
		    # vertices 2 and 3.
		    reftri[ntri,RG_INDEX] = ntri
		    if (rij <= rjk) {
	                if (rki <= rij) {
			    reftri[ntri,RG_X1] = refindex[k]
			    reftri[ntri,RG_X2] = refindex[i]
			    reftri[ntri,RG_X3] = refindex[j]
	    		} else if (rki >= rjk) {
			    reftri[ntri,RG_X1] = refindex[i]
			    reftri[ntri,RG_X2] = refindex[j]
			    reftri[ntri,RG_X3] = refindex[k]
	    		} else {
			    reftri[ntri,RG_X1] = refindex[j]
			    reftri[ntri,RG_X2] = refindex[i]
			    reftri[ntri,RG_X3] = refindex[k]
	    	        }
		    } else {
	    	        if (rki <= rjk) {
			    reftri[ntri,RG_X1] = refindex[i]
			    reftri[ntri,RG_X2] = refindex[k]
			    reftri[ntri,RG_X3] = refindex[j]
	    	        } else if (rki >= rij) {
			    reftri[ntri,RG_X1] = refindex[k]
			    reftri[ntri,RG_X2] = refindex[j]
			    reftri[ntri,RG_X3] = refindex[i]
	    	        } else {
			    reftri[ntri,RG_X1] = refindex[j]
			    reftri[ntri,RG_X2] = refindex[k]
			    reftri[ntri,RG_X3] = refindex[i]
	    	        }
		    }

		    # Compute the lengths of the sides.
		    dx1 = xref[reftri[ntri,RG_X3]] - xref[reftri[ntri,RG_X2]]
		    dy1 = yref[reftri[ntri,RG_X3]] - yref[reftri[ntri,RG_X2]]
		    dx2 = xref[reftri[ntri,RG_X2]] - xref[reftri[ntri,RG_X1]]
		    dy2 = yref[reftri[ntri,RG_X2]] - yref[reftri[ntri,RG_X1]]
		    dx3 = xref[reftri[ntri,RG_X3]] - xref[reftri[ntri,RG_X1]]
		    dy3 = yref[reftri[ntri,RG_X3]] - yref[reftri[ntri,RG_X1]]

		    # Compute the ratio of the longest side of the triangle
		    # to the shortest side.
		    r1 = sqrt (dx1 ** 2 + dy1 ** 2)
		    r2sq = dx2 ** 2 + dy2 ** 2
		    r2 = sqrt (r2sq)
		    r3sq = dx3 ** 2 + dy3 ** 2
		    r3 = sqrt (r3sq)
		    if (r2 <= 0.)
	    	        next
		    ratio = r3 / r2
		    if (ratio > maxratio)
	    		next

		    # Compute the cos, cos ** 2 and sin ** 2 of the angle at 
		    # vertex 1.
		    cosc = (dx3 * dx2 + dy3 * dy2) / (r3 * r2)
		    cosc2 = max (0.0, min (1.0, cosc * cosc))
		    sinc2 = max (0.0, min (1.0, 1.0 - cosc2))

		    # Determine whether the triangles vertices are arranged
		    # clockwise of anticlockwise.
		    if ((dx2 * dy1 - dy2 * dx1) > 0.0)
			reftri[ntri,RG_CC] = YES
		    else
			reftri[ntri,RG_CC] = NO

		    # Compute the tolerances.
		    tol = (1.0 / r3sq - cosc / (r3 * r2) + 1.0 / r2sq)
		    tripar[ntri,RG_TOLR] = 2.0 * ratio ** 2 * tol2 * tol 
		    tripar[ntri,RG_TOLC] = 2.0 * sinc2 * tol2 * tol + 3.0 *
		        cosc2 * tol2 ** 2 * tol * tol

		    # Compute the perimeter.
		    tripar[ntri,RG_LOGP] = log (r1 + r2 + r3)
		    tripar[ntri,RG_RATIO] = ratio
		    tripar[ntri,RG_COS1] = cosc

		    ntri = ntri + 1
		}
	    }
	}

	ntri = ntri - 1

	# Sort the triangles in increasing order of ratio.
	call rg_qsortr (tripar[1,RG_RATIO], reftri[1,RG_INDEX],
	    reftri[1,RG_INDEX], ntri)

	return (ntri)
end


# RG_TMERGE -- Compute the intersection of two sorted files of triangles
# using the tolerance parameter.

int procedure rg_tmerge (reftri, rtripar, nrtri, nmrtri, listri, ltripar,
	nltri, nmltri)

int	reftri[nmrtri,ARB]	#U list of reference triangles
real	rtripar[nmrtri,ARB]	#I reference triangle parameters
int	nrtri			#I number of reference triangles
int	nmrtri			#I maximum number of reference triangles
int	listri[nmltri,ARB]	#U list of reference triangles
real	ltripar[nmltri,ARB]	#I reference triangle parameters
int	nltri			#I number of reference triangles
int	nmltri			#I maximum number of reference triangles

int	rp, blp, lp, ninter, rindex, lindex, mindex
real	rmaxtol, lmaxtol, maxtol, dr, dr2, mdr2, dcos2, mdcos2, dtolr, dtolc 

begin
	# Find the maximum tolerance for each list.
	call alimr (rtripar[1,RG_TOLR], nrtri, maxtol, rmaxtol)
	call alimr (ltripar[1,RG_TOLR], nltri, maxtol, lmaxtol)
	maxtol = sqrt (rmaxtol + lmaxtol)

	# Define the beginning of the search range for each triangle.
	blp = 1

	# Loop over all the triangles in the reference list.
	ninter = 0
	for (rp = 1; rp <= nrtri; rp = rp + 1) {

	    # Get the index for the reference triangle.
	    rindex = reftri[rp,RG_INDEX]

	    # Move to the first triangle in the input list that satisfies the
	    # ratio tolerance requirement.
	    for  (; blp <= nltri; blp = blp + 1) {
		lindex = listri[blp,RG_INDEX]
		dr = rtripar[rindex,RG_RATIO] - ltripar[lindex,RG_RATIO]
		if (dr <= maxtol)
		    break
	    }

	    # If the beginning of the search range becomes greater than
	    # the length of the list then there is no match.
	    if (blp > nltri)
		break

	    # If the first triangle in the list is past the tolerance
	    # limit skip to the next reference triangle
	    if (dr < -maxtol)
		next

	    # Search through the appropriate range of triangles for the 
	    # closest fit.

	    # Initialize the tolerances.
	    mindex = 0
	    mdr2 = 0.5 * MAX_REAL
	    mdcos2 = 0.5 * MAX_REAL

	    for (lp = blp; lp <= nltri; lp = lp + 1) {

		# Quit the loop if the next triangle is out of match range.
		lindex = listri[lp,RG_INDEX]
		dr = rtripar[rindex,RG_RATIO] - ltripar[lindex,RG_RATIO]
		if (dr < -maxtol)
		    break

		# Compute the tolerances for the two triangles.
		dr2 = dr * dr
		dcos2 = (rtripar[rindex,RG_COS1] - ltripar[lindex,RG_COS1]) ** 2
		dtolr = rtripar[rindex,RG_TOLR] + ltripar[lindex,RG_TOLR]
		dtolc = rtripar[rindex,RG_TOLC] + ltripar[lindex,RG_TOLC] 

		# Find the best of all possible matches.
		if (dr2 <= dtolr && dcos2 <= dtolc) {
		    if ((dr2 + dcos2) < (mdr2 + mdcos2)) {
			mindex = lindex
			mdr2 = dr2
			mdcos2 = dcos2
		    }
		}

	    }

	    # Add the match to the list.
	    if (mindex > 0) {
		ninter = ninter + 1
		reftri[ninter,RG_MATCH] = rindex
		listri[ninter,RG_MATCH] = mindex
	    }
	}

	return (ninter)
end


# RG_TREJECT -- Remove false matches from the list of matched triangles.

int procedure rg_treject (reftri, rtripar, nrtri, nmrtri, listri, ltripar,
	nltri, nmltri, nmatch, maxiter)

int	reftri[nmrtri,ARB]	#U list of reference triangles
real	rtripar[nmrtri,ARB]	#I reference triangle parameters
int	nrtri			#I number of reference triangles
int	nmrtri			#I maximum number of reference triangles
int	listri[nmltri,ARB]	#U list of reference triangles
real	ltripar[nmltri,ARB]	#I reference triangle parameters
int	nltri			#I number of reference triangles
int	nmltri			#I maximum number of reference triangles
int	nmatch			#I initial number of matches
int	maxiter			#I maximum number of rejection iterations

double	dif, mode, sum, sumsq
int	i, nrej, nplus, nminus, ntrue, nfalse, npts, ncount, niter, rindex
int	lindex
pointer	sp, adif
real	sigma, factor, locut, hicut
double	rg_moded()

begin
	call smark (sp)
	call salloc (adif, nmatch, TY_DOUBLE)

	# Accumulate the number of same sense and number of opposite sense
	# matches as well as the log perimeter statistics.
	sum = 0.0d0
	sumsq = 0.0d0
	nplus = 0
	do i = 1, nmatch {
	    rindex = reftri[i,RG_MATCH]
	    lindex = listri[i,RG_MATCH]
	    dif = (rtripar[rindex,RG_LOGP] - ltripar[lindex,RG_LOGP])
	    Memd[adif+i-1] = dif
	    sum = sum + dif
	    sumsq = sumsq + dif * dif
	    if (reftri[rindex,RG_CC] == listri[lindex,RG_CC])
		nplus = nplus + 1
	}
	nminus = nmatch - nplus

	# Compute the mean, mode, and sigma of the logP distribution,
	ntrue = abs (nplus - nminus)
	nfalse = nplus + nminus - ntrue
	#mean = sum / nmatch
	if (nmatch <= 1)
	    sigma = 0.0
	else
	    sigma = (sumsq - (sum / nmatch) * sum) / (nmatch - 1)
	if (sigma <= 0.0) {
	    call sfree (sp)
	    return (nmatch)
	} else
	    sigma = sqrt (sigma)
	call asrtd (Memd[adif], Memd[adif], nmatch)
        #if (mod (nmatch,2) == 1)
            #median = Memd[adif+nmatch/2]
        #else
	    #median = (Memd[adif+nmatch/2] + Memd[adif+(nmatch-1)/2]) / 2.0d0
	mode = rg_moded (Memd[adif], nmatch, 10, 1.0d0, 0.1d0 * sigma,
	    0.01d0 * sigma)
	if (nfalse > ntrue)
	    factor = 1.0
	else if ((0.1 * ntrue) > nfalse)
	    factor = 3.0
	else
	    factor = 2.0

	# Begin the rejection cycle.
	npts = nmatch
	niter = 0
	repeat {

	    ncount = 0
	    nrej = 0
	    locut = mode - factor * sigma
	    hicut = mode + factor * sigma

	    # Reject matched triangles which are too far from the mean logP.
	    do i = 1, npts {
		rindex = reftri[i,RG_MATCH]
		lindex = listri[i,RG_MATCH]
		dif = rtripar[rindex,RG_LOGP] - ltripar[lindex,RG_LOGP]
		if (dif < locut || dif > hicut) {
		    sum = sum - dif
		    sumsq = sumsq - dif * dif
	    	    if (reftri[rindex,RG_CC] == listri[lindex,RG_CC])
			nplus = nplus - 1
		    else
			nminus = nminus - 1
		    nrej = nrej + 1
		} else {
		    Memd[adif+ncount] = dif
		    ncount = ncount + 1
		    reftri[ncount,RG_MATCH] = rindex
		    listri[ncount,RG_MATCH] = lindex
		}
	    }

	    # No more points were rejected.
	    npts = ncount
	    if (nrej <= 0)
		break

	    # All the points were rejected.
	    if (npts <= 0)
		break

	    # The rejection iteration limit was reached.
	    niter = niter + 1
	    if (niter >= maxiter)
		break

	    # Compute the new mean and sigma of the logP distribution.
	    #mean = sum / npts
	    if (npts <= 1)
	        sigma = 0.0
	    else
	        sigma = (sumsq - (sum / npts) * sum) / (npts - 1)
	    if (sigma <= 0.0)
		break
	    sigma = sqrt (sigma)
	    call asrtd (Memd[adif], Memd[adif], npts)
            #if (mod (npts,2) == 1)
                #median = Memd[adif+npts/2]
            #else
		#median = (Memd[adif+npts/2] + Memd[adif+(npts-1)/2]) / 2.0d0
	    mode = rg_moded (Memd[adif], npts, 10, 1.0d0, 0.10d0 * sigma,
	        0.01d0 * sigma)

	    # Recompute the ksigma rejection criterion based on the number of
	    # same and opposite sense matches.
	    ntrue = abs (nplus - nminus)
	    nfalse = nplus + nminus - ntrue
	    if (nfalse > ntrue)
	        factor = 1.0
	    else if ((0.1 * ntrue) > nfalse)
	        factor = 3.0
	    else
	        factor = 2.0
	}

	# One last iteration to get rid of opposite sense of matches.
	if (npts <= 0)
	    npts = 0
	else if (nplus > nminus) {
	    ncount = 0
	    do i = 1, npts {
	        rindex = reftri[i,RG_MATCH]
	        lindex = listri[i,RG_MATCH]
	    	if (reftri[rindex,RG_CC] == listri[lindex,RG_CC]) {
		    ncount = ncount + 1
		    reftri[ncount,RG_MATCH] = rindex
		    listri[ncount,RG_MATCH] = lindex
		}
	    }
	    npts = ncount
	} else {
	    ncount = 0
	    do i = 1, npts {
	        rindex = reftri[i,RG_MATCH]
	        lindex = listri[i,RG_MATCH]
	    	if (reftri[rindex,RG_CC] != listri[lindex,RG_CC]) {
		    ncount = ncount + 1
		    reftri[ncount,RG_MATCH] = rindex
		    listri[ncount,RG_MATCH] = lindex
		}
	    }
	    npts = ncount
	}

	call sfree (sp)
	return (npts)
end


# RG_TVOTE -- Count the number a times a particular pair of
# coordinates is matched in the set of matched triangles. If a particular
# pair of points occurs in many triangles it is much more likely to be
# a true match than if it occurs in very few. Since this vote array
# may be quite sparsely occupied, use the PLIO package to store and
# maintain the list.

int procedure rg_tvote (reftri, nmrtri, nrefstars, listri, nmltri, nliststars,
	nmatch)

int	reftri[nmrtri,ARB]		#U reference triangles
int	nmrtri				#I maximum number of reference triangles
int	nrefstars			#I number of reference stars
int	listri[nmltri,ARB]		#U input list triangles
int	nmltri				#I maximum number of list triangles
int	nliststars			#I number of list stars
int	nmatch				#I number of match triangles

int	i, j, rp, lp, vp, pixval, tminvote, tmaxvote, minvote, maxvote, hmaxvote
int	ninter, axes[2], laxes[2], pvp
pointer	sp, vote, vindex, pl, lmatch, rmatch
bool	pl_linenotempty()
pointer	pl_create()

begin
	# Open the pixel list.
	axes[1] = nliststars
	axes[2] = nrefstars
	pl = pl_create (2, axes, 16)

	# Acumulate the votes.
	do i = 1, nmatch {
	    rp = reftri[i,RG_MATCH]
	    lp = listri[i,RG_MATCH]
	    laxes[1] = listri[lp,RG_X1]
	    laxes[2] = reftri[rp,RG_X1]
	    if (! pl_linenotempty (pl, laxes))
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET + PIX_VALUE(1))
	    else {
		call pl_glpi (pl, laxes, pixval, 16, 1, PIX_SRC)
		pixval = pixval + 1
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET +
		    PIX_VALUE(pixval))
	    }
	    laxes[1] = listri[lp,RG_X2]
	    laxes[2] = reftri[rp,RG_X2]
	    if (! pl_linenotempty (pl, laxes))
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET + PIX_VALUE(1))
	    else {
		call pl_glpi (pl, laxes, pixval, 16, 1, PIX_SRC)
		pixval = pixval + 1
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET +
		    PIX_VALUE(pixval))
	    }
	    laxes[1] = listri[lp,RG_X3]
	    laxes[2] = reftri[rp,RG_X3]
	    if (! pl_linenotempty (pl, laxes))
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET + PIX_VALUE(1))
	    else {
		call pl_glpi (pl, laxes, pixval, 16, 1, PIX_SRC)
		pixval = pixval + 1
	        call pl_point (pl, laxes[1], laxes[2], PIX_SET +
		    PIX_VALUE(pixval))
	    }
	}

	# Allocate temporary working space.
	call smark (sp)
	call salloc (vote, axes[1], TY_INT)
	call salloc (vindex, axes[1], TY_INT)
	call salloc (lmatch, axes[1], TY_INT)
	call salloc (rmatch, axes[2], TY_INT)
	call amovki (NO, Memi[lmatch], axes[1])
	call amovki (NO, Memi[rmatch], axes[2])

	# Find the maximum value in the mask.
	minvote = MAX_INT
	maxvote = -MAX_INT
	do i = 1, axes[2] {
	    laxes[1] = 1
	    laxes[2] = i
	    if (! pl_linenotempty (pl, laxes))
		next
	    call pl_glpi (pl, laxes, Memi[vote], 16, axes[1], PIX_SRC)
	    call alimi (Memi[vote], axes[1], tminvote, tmaxvote)
	    minvote = min (minvote, tminvote)
	    maxvote = max (maxvote, tmaxvote)
	}
	if (maxvote < 0) {
	    maxvote = 0
	    hmaxvote = 0
	} else
	    hmaxvote = maxvote / 2

	# Vote on the matched pairs.
	ninter = 0
	if (maxvote > 0) {
	    do j = 1, axes[2] {

	        # Sort the vote array.
	        do i = 1, axes[1]
		    Memi[vindex+i-1] = i
	        laxes[1] = 1
	        laxes[2] = j
	        call pl_glpi (pl, laxes, Memi[vote], 16, axes[1], PIX_SRC)
	        call rg_qsorti (Memi[vote], Memi[vindex], Memi[vindex],
		    axes[1])

	        # Reject points which have no votes, which have only a
		# single vote if the maximum number of votest is > 1,
		# less or equal to half the  maximum number of votes,
		# the same number of votes as the next largest index,
		# or which have already been matched.

		vp = Memi[vindex+axes[1]-1]
		pvp = Memi[vindex+axes[1]-2]
	        if (Memi[vote+vp-1] <= 0)
		    next
		if (Memi[vote+vp-1] == Memi[vote+pvp-1])
		    next
		if (Memi[vote+vp-1] <= hmaxvote)
		    next
		if (Memi[lmatch+vp-1] == YES || Memi[rmatch+j-1] == YES)
		    next
	        if (Memi[vote+vp-1] == 1 && (maxvote > 1 || nmatch > 1))
		    next

		ninter = ninter + 1
		reftri[ninter, RG_MATCH] = j
		listri[ninter,RG_MATCH] = vp
		Memi[rmatch+j-1] = YES
		Memi[lmatch+vp-1] = YES
	    }
	} else if (maxvote > 0) {
	}

	call sfree (sp)
	call pl_close (pl)

	return (ninter)
end


# RG_MLINCOEFF -- Compute the coefficients of a new linear transformation
# using the first one to three matched stars as input.

int procedure rg_mlincoeff (xref, yref, xlist, ylist, reftri, nmrtri, listri,
	nmltri, nmatch, coeff, ncoeff)

real	xref[ARB]		#I the x reference coordinates
real	yref[ARB]		#I the y reference coordinates
real	xlist[ARB]		#I the x list coordinates
real	ylist[ARB]		#I the y list coordinates
int	reftri[nmrtri,ARB]	#I list of reference triangles
int	nmrtri			#I maximum number of reference triangles
int	listri[nmltri,ARB]	#I list of reference triangles
int	nmltri			#I maximum number of list triangles
int	nmatch			#I number of matches
real	coeff[ARB]		#O the new computed coefficients
int	ncoeff			#I the number of coefficients

int	i, rindex, lindex, stat
pointer	sp, xr, yr, xin, yin
int	rg_lincoeff()

begin
	if (nmatch <= 0)
	    return (ERR)

	call smark (sp)
	call salloc (xr, nmatch, TY_REAL)
	call salloc (yr, nmatch, TY_REAL)
	call salloc (xin, nmatch, TY_REAL)
	call salloc (yin, nmatch, TY_REAL)

	# Load the points to be fit.
	do i = 1, nmatch {
	    rindex = reftri[i,RG_MATCH]
	    lindex = listri[i,RG_MATCH]
	    Memr[xr+i-1] = xref[rindex] 
	    Memr[yr+i-1] = yref[rindex] 
	    Memr[xin+i-1] = xlist[lindex]
	    Memr[yin+i-1] = ylist[lindex]
	}

	# Compute the new coefficients.
	stat = rg_lincoeff (Memr[xr], Memr[yr], Memr[xin], Memr[yin],
	    nmatch, coeff, ncoeff)

	call sfree (sp)

	return (stat)
end


# RG_MWRITE -- Write out the intersection of the two matched pixel lists to the
# output file.

procedure rg_mwrite (ofd, xref, yref, rlineno, xlist, ylist, ilineno,
	reftri, nmrtri, listri, nmltri, nmatch, xformat, yformat)

int	ofd			#I the output file descriptor
real	xref[ARB]		#I the x reference coordinates
real	yref[ARB]		#I the y reference coordinates
int	rlineno[ARB]		#I the reference coordinate line numbers
real	xlist[ARB]		#I the x list coordinates
real	ylist[ARB]		#I the y list coordinates
int	ilineno[ARB]		#I the input list line numbers
int	reftri[nmrtri,ARB]	#I list of reference triangles
int	nmrtri			#I maximum number of reference triangles
int	listri[nmltri,ARB]	#I list of reference triangles
int	nmltri			#I maximum number of list triangles
int	nmatch			#I number of matches
char	xformat[ARB]		#I the output x column format
char	yformat[ARB]		#I the output y column format

int	i, lindex, rindex
pointer	sp, fmtstr

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)

	# Construct the format string.
	call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %%5d %%5d\n")
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)

	do i = 1, nmatch {
	    rindex = reftri[i,RG_MATCH]
	    lindex = listri[i,RG_MATCH]
	    call fprintf (ofd, Memc[fmtstr])
		call pargr (xref[rindex])
		call pargr (yref[rindex])
		call pargr (xlist[lindex])
		call pargr (ylist[lindex])
		call pargi (rlineno[rindex])
		call pargi (ilineno[lindex])
	}

	call sfree (sp)
end


# RG_LMWRITE -- Write out the intersection of the matched celestial coordinate
# and pixel lists to the output file.

procedure rg_lmwrite (ofd, lngref, latref, rlineno, xlist, ylist, ilineno,
	reftri, nmrtri, listri, nmltri, nmatch, lngformat, latformat,
	xformat, yformat)

int	ofd			#I the output file descriptor
double	lngref[ARB]		#I the x reference coordinates
double	latref[ARB]		#I the y reference coordinates
int	rlineno[ARB]		#I the reference coordinate line numbers
real	xlist[ARB]		#I the x list coordinates
real	ylist[ARB]		#I the y list coordinates
int	ilineno[ARB]		#I the input list line numbers
int	reftri[nmrtri,ARB]	#I list of reference triangles
int	nmrtri			#I maximum number of reference triangles
int	listri[nmltri,ARB]	#I list of reference triangles
int	nmltri			#I maximum number of list triangles
int	nmatch			#I number of matches
char	lngformat[ARB]		#I the output longitude column format
char	latformat[ARB]		#I the output latitude column format
char	xformat[ARB]		#I the output x column format
char	yformat[ARB]		#I the output y column format

int	i, lindex, rindex
pointer	sp, fmtstr

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)

	# Construct the format string.
	call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %%5d %%5d\n")
	if (lngformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (lngformat)
	if (latformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (latformat)
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)

	do i = 1, nmatch {
	    rindex = reftri[i,RG_MATCH]
	    lindex = listri[i,RG_MATCH]
	    call fprintf (ofd, Memc[fmtstr])
		call pargd (lngref[rindex])
		call pargd (latref[rindex])
		call pargr (xlist[lindex])
		call pargr (ylist[lindex])
		call pargi (rlineno[rindex])
		call pargi (ilineno[lindex])
	}

	call sfree (sp)
end


# RG_FACTORIAL -- Compute the combinatorial function which is defined as
# n! / ((n - ngroup)! * ngroup!).

int procedure rg_factorial (n, ngroup)

int	n		#I input argument
int	ngroup		#I combinatorial factor

int	i, fac, grfac

begin
	if (n <= 0)
	    return (1)

	fac = n
	do i = n - 1, n - 3 + 1, -1
	    fac = fac * i

	grfac = ngroup
	do i = ngroup - 1, 2, -1
	    grfac = grfac * i

	return (fac / grfac)
end


# RG_MODED -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

double procedure rg_moded (a, npts, nmin, zrange, fzbin, fzstep)

double  a[npts]                 #I the sorted input data array
int     npts                    #I the number of points
int     nmin                    #I the minimum number of points
double  zrange                  #I fraction of pixels around median to use
double  fzbin                   #I the bin size for the mode search
double  fzstep                  #I the step size for the mode search

int     x1, x2, x3, nmax
double  zstep, zbin, y1, y2, mode
bool    fp_equald()

begin
        # If there are too few points return the median.
        if (npts < nmin) {
            if (mod (npts,2) == 1)
                return (a[1+npts/2])
            else
                return ((a[npts/2] + a[1+npts/2]) / 2.0d0)
        }

        # Compute the data range that will be used to do the mode search.
        # If the data has no range then the constant value will be returned.
        x1 = max (1, int (1.0d0 + npts * (1.0d0 - zrange) / 2.0d0))
        x3 = min (npts, int (1.0d0 + npts * (1.0d0 + zrange) / 2.0d0))
        if (fp_equald (a[x1], a[x3]))
            return (a[x1])


        # Compute the bin and step size. The bin size is based on the
        # data range over a fraction of the pixels around the median
        # and a bin step which may be smaller than the bin size.

        zstep = fzstep #* (a[x3] - a[x1])
        zbin = fzbin #* (a[x3] - a[x1])

        nmax = 0
        x2 = x1
        for (y1 = a[x1]; x2 < x3; y1 = y1 + zstep) {
            for (; a[x1] < y1; x1 = x1 + 1)
                ;
            y2 = y1 + zbin
            for (; (x2 < x3) && (a[x2] < y2); x2 = x2 + 1)
                ;
            if (x2 - x1 > nmax) {
                nmax = x2 - x1
                if (mod (x2+x1,2) == 0)
                    mode = a[(x2+x1)/2]
                else
                    mode = (a[(x2+x1)/2] + a[(x2+x1)/2+1]) / 2.0d0
            }
        }

        return (mode)
end


#define  NMIN    10        # Minimum number of pixels for mode calculation
#define  ZRANGE  0.8d0     # Fraction of pixels about median to use
#define  ZSTEP   0.01d0    # Step size for search for mode
#define  ZBIN    0.1d0     # Bin size for mode.
#
## RG_MODED -- Compute mode of an array.  The mode is found by binning
## with a bin size based on the data range over a fraction of the
## pixels about the median and a bin step which may be smaller than the
## bin size.  If there are too few points the median is returned.
## The input array must be sorted.
#
#double	procedure rg_moded (a, n)
#
#double  a[n]                    # Data array
#int     n                       # Number of points
#
#int     i, j, k, nmax
#real    z1, z2, zstep, zbin
#double  mode
#bool    fp_equald()
#
#begin
#        if (n < NMIN)
#            return (a[n/2])
#
#        # Compute the mode.  The array must be sorted.  Consider a
#        # range of values about the median point.  Use a bin size which
#        # is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
#        # the bin size.
#
#	i = 1 + n * (1. - ZRANGE) / 2.0d0
#        j = 1 + n * (1. + ZRANGE) / 2.0d0
#        z1 = a[i]
#        z2 = a[j]
#        if (fp_equald (z1, z2)) {
#            mode = z1
#            return (mode)
#        }
#
#        zstep = ZSTEP * (z2 - z1)
#        zbin = ZBIN * (z2 - z1)
#
#        z1 = z1 - zstep
#        k = i
#        nmax = 0
#        repeat {
#            z1 = z1 + zstep
#            z2 = z1 + zbin
#            for (; i < j && a[i] < z1; i=i+1)
#                ;
#            for (; k < j && a[k] < z2; k=k+1)
#                ;
#            if (k - i > nmax) {
#                nmax = k - i
#                mode = a[(i+k)/2]
#            }
#        } until (k >= j)
#
#        return (mode)
#end
#
