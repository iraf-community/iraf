include <imhdr.h>
include <mach.h>
include "linmatch.h"
include "lsqfit.h"

# RG_LSCALE -- Compute the scaling parameters required to match the
# intensities of an image to a reference image.

int procedure rg_lscale (imr, im1, db, dformat, ls)

pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
pointer	db		#I pointer to the database file
int	dformat		#I write the output file in database format
pointer	ls		#I pointer to the linscale structure

pointer	sp, image, imname
real	bscale, bzero, bserr, bzerr
bool	streq()
int	rg_lstati(), fscan(), nscan()

#int	i, nregions
#int	rg_isfit ()
#pointer	rg_istatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, IMAGE, Memc[image], SZ_FNAME)

	# Initialize.
	bscale = 1.0
	bzero = 0.0

	# Compute the average bscale and bzero for the image either by
	# reading it from a file or by computing it directly from the
	# data.

	if (rg_lstati(ls, BZALGORITHM) == LS_FILE && rg_lstati (ls,
	    BSALGORITHM) == LS_FILE) {

	    # Read the results of a previous run from the database file or
	    # a simple text file.
	    if (dformat ==  YES) {
		call rg_lfile (db, ls, bscale, bzero, bserr, bzerr)
	    } else {
		if (fscan(db) != EOF) {
		    call gargwrd (Memc[imname], SZ_FNAME)
		    call gargr (bscale)
		    call gargr (bzero)
		    call gargr (bserr)
		    call gargr (bzerr)
		    if (! streq (Memc[image], Memc[imname]) || nscan() != 5) {
			bscale = 1.0
			bzero = 0.0
			bserr = INDEFR
			bzerr = INDEFR
		    }
		} else {
		    bscale = 1.0
		    bzero = 0.0
		    bserr = INDEFR
		    bzerr = INDEFR
		}
	    }

	    # Store the values.
	    call rg_lsetr (ls, TBSCALE, bscale)
	    call rg_lsetr (ls, TBZERO, bzero)
	    call rg_lsetr (ls, TBSCALEERR, bserr)
	    call rg_lsetr (ls, TBZEROERR, bzerr)

	} else {

	    # Write out the algorithm parameters.
	    if (dformat == YES)
		call rg_ldbparams (db, ls)

	    # Compute the individual scaling factors and their errors for
	    # all the regions and the average scaling factors and their
	    # errors.
	    call rg_scale (imr, im1, ls, bscale, bzero, bserr, bzerr, YES)

	    # Write out the results for the individual regions.
	    if (dformat == YES)
		call rg_lwreg (db, ls)

	    # Write out the final scaling factors
	    if (dformat == YES)
		call rg_ldbtscale (db, ls) 
	    else {
		call fprintf (db, "%s  %g %g  %g %g\n")
		    call pargstr (Memc[image])
		    call pargr (bscale)
		    call pargr (bzero)
		    call pargr (bserr)
		    call pargr (bzerr)
	    }
	}

	call sfree (sp)

	return (NO)
end


# RG_SCALE -- Compute the scaling parameters for a list of regions.

procedure rg_scale (imr, im1, ls, tbscale, tbzero, tbserr, tbzerr, refit)

pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
pointer	ls		#I pointer to the intensity matching structure
real	tbscale		#O the average scaling parameter
real	tbzero		#O the average offset parameter
real	tbserr		#O the average error in the scaling parameter
real	tbzerr		#O the average error in the offset parameter
int	refit		#I recompute entire fit, otherwise recompute averages

int	i, nregions, ngood
double	sumbscale, sumbzero, sumwbscale, sumbserr, sumbzerr, sumwbzero, dw
real	bscale, bzero, bserr, bzerr, avbscale, avbzero, avbserr, avbzerr
int	rg_lstati(), rg_limget(), rg_lbszfit()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	# Determine the number of regions.
	nregions = rg_lstati (ls, NREGIONS)

	# Initialize the statistics
	sumbscale = 0.0d0
	sumbserr = 0.0d0
	sumwbscale = 0.0d0
	sumbzero = 0.0d0
	sumbzerr = 0.0d0
	sumwbzero = 0.0d0
	ngood = 0

	# Loop over the regions.
	do i = 1, nregions {

	    if (refit == YES) {

	        # Set the current region.
	        call rg_lseti (ls, CNREGION, i)

	        # Fetch the data for the given region and estimate the mean,
	        # median, mode, standard deviation, and number of points in
	        # each region, if this is required by the algorithm.
	        if (imr != NULL) {
	            if (rg_limget (ls, imr, im1, i) == ERR) {
		        call rg_lgmmm (ls, i)
		        next
		    } else
		        call rg_lgmmm (ls, i)
	        }

	        # Compute bscale and bzero and store the results in the
		# internal arrays
	        if (rg_lbszfit (ls, i, bscale, bzero, bserr, bzerr) == ERR)
		    next

	    } else {
		bscale = Memr[rg_lstatp(ls,RBSCALE)+i-1]
		bzero = Memr[rg_lstatp(ls,RBZERO)+i-1]
		bserr = Memr[rg_lstatp(ls,RBSCALEERR)+i-1]
		bzerr = Memr[rg_lstatp(ls,RBZEROERR)+i-1]
	    }

	    # Accumulate the weighted sums of the scaling factors.
	    if (Memi[rg_lstatp(ls,RDELETE)+i-1] == LS_NO &&
	        ! IS_INDEFR(bserr) && ! IS_INDEFR(bzerr)) {

		if (bserr <= 0.0)
		    dw = 1.0d0
		else
		    dw = 1.0d0 / bserr ** 2
		sumbscale = sumbscale + dw * bscale 
		sumbserr = sumbserr + dw * bscale * bscale
		sumwbscale = sumwbscale + dw

		if (bzerr <= 0.0)
		    dw = 1.0d0
		else
		    dw = 1.0d0 / bzerr ** 2
		sumbzero = sumbzero + dw * bzero
		sumbzerr = sumbzerr + dw * bzero * bzero
		sumwbzero = sumwbzero + dw

		ngood = ngood + 1
	    }
	}

	# Compute the average scaling factors.
	call rg_avstats (sumbscale, sumbzero, sumwbscale, sumwbzero, sumbserr,
	    sumbzerr, bserr, bserr, avbscale, avbzero, avbserr, avbzerr, ngood)

	# Perform the rejection cycle.
	if (ngood > 2 && rg_lstati(ls, NREJECT) > 0 &&
	    (! IS_INDEFR(rg_lstatr(ls,LOREJECT)) || ! IS_INDEFR(rg_lstatr(ls,
	    HIREJECT)))) {
	    call rg_ravstats (ls, sumbscale, sumbzero, sumwbscale, sumwbzero,
	        sumbserr, sumbzerr, bserr, bzerr, avbscale, avbzero, avbserr,
		avbzerr, ngood)
	}

	# Compute the final scaling factors.
	if (ngood > 1) {
	    call rg_lbszavg (ls, avbscale, avbzero, avbserr, avbzerr,
	        tbscale, tbzero, tbserr, tbzerr)
	} else {
	    tbscale = avbscale
	    tbzero = avbzero
	    tbserr = avbserr
	    tbzerr = avbzerr
	}

	# Store the compute values.
	call rg_lsetr (ls, TBSCALE, tbscale)
	call rg_lsetr (ls, TBZERO, tbzero)
	call rg_lsetr (ls, TBSCALEERR, tbserr)
	call rg_lsetr (ls, TBZEROERR, tbzerr)
end


# RG_LIMGET -- Fetch the reference and input image data and compute the
# statistics for a given region.

int procedure rg_limget (ls, imr, im1, i)

pointer	ls		#I pointer to the intensity scaling structure
pointer	imr		#I pointer to reference image
pointer	im1		#I pointer to image
int	i		#I the region id

int	stat, nrimcols, nrimlines, nimcols, nimlines, nrcols, nrlines, ncols 
int	nlines, rc1, rc2, rl1, rl2, c1, c2, l1, l2, xstep, ystep, npts
pointer	sp, str, ibuf, rbuf, prc1, prc2, prxstep, prl1, prl2, prystep
int	rg_lstati(), rg_simget()
pointer	rg_lstatp()
real	rg_lstatr()

#int	c1, c2, l1, l2
#int	ncols, nlines, npts

define	nextregion_	11

begin
	stat = OK

	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Delete the data of the previous region if any.
	rbuf = rg_lstatp (ls, RBUF)
	if (rbuf != NULL)
	    call mfree (rbuf, TY_REAL)
	rbuf = NULL
	ibuf = rg_lstatp (ls, IBUF)
	if (ibuf != NULL)
	    call mfree (ibuf, TY_REAL)
	ibuf = NULL

	# Check for number of regions.
	if (i < 1 || i > rg_lstati (ls, NREGIONS)) {
	    stat = ERR
	    goto nextregion_
	}

	# Get the reference and input image sizes.
        nrimcols = IM_LEN(imr,1)
        if (IM_NDIM(imr) == 1)
            nrimlines = 1
        else
            nrimlines = IM_LEN(imr,2)
        nimcols = IM_LEN(im1,1)
        if (IM_NDIM(im1) == 1)
            nimlines = 1
        else
	    nimlines = IM_LEN(im1,2)

	# Get the reference region pointers.
	prc1 = rg_lstatp (ls, RC1)
	prc2 = rg_lstatp (ls, RC2)
	prl1 = rg_lstatp (ls, RL1)
	prl2 = rg_lstatp (ls, RL2)
	prxstep = rg_lstatp (ls, RXSTEP) 
	prystep = rg_lstatp (ls, RYSTEP) 

	# Get the reference subraster regions.
	rc1 = Memi[prc1+i-1]
	rc2 = Memi[prc2+i-1]
	rl1 = Memi[prl1+i-1]
	rl2 = Memi[prl2+i-1]
	xstep = Memi[prxstep+i-1]
	ystep = Memi[prystep+i-1]
	nrcols = (rc2 - rc1) / xstep + 1
	nrlines = (rl2 - rl1) / ystep + 1

	# Move to the next region if current reference region is off the image.
        if (rc1 < 1 || rc1 > nrimcols || rc2 < 1 || rc2 > nrimcols ||
	    rl1 > nrimlines || rl1 < 1 || rl2 < 1 || rl2 > nrimlines) {
            call rg_lstats (ls, REFIMAGE, Memc[str], SZ_LINE)
            call eprintf (
                "Reference region %d: %s[%d:%d:%d,%d:%d:%d] is off image.\n")
		call pargi (i)
                call pargstr (Memc[str])
                call pargi (rc1)
                call pargi (rc2)
		call pargi (xstep)
                call pargi (rl1)
                call pargi (rl2)
		call pargi (ystep)
            stat = ERR
            goto nextregion_
        }

	# Move to next region if current reference region is too small.
        if (nrcols < 3 || (IM_NDIM(imr) == 2 && nrlines < 3)) {
            call rg_lstats (ls, REFIMAGE, Memc[str], SZ_LINE)
            call eprintf (
            "Reference region %d: %s[%d:%d:%d,%d:%d:%d] has too few points.\n")
		call pargi (i)
                call pargstr (Memc[str])
                call pargi (rc1)
                call pargi (rc2)
		call pargi (xstep)
                call pargi (rl1)
                call pargi (rl2)
		call pargi (ystep)
            stat = ERR
            goto nextregion_
        }

	# Get the reference image data.
	npts = rg_simget (imr, rc1, rc2, xstep, rl1, rl2, ystep, rbuf)
	if (npts < 9) {
	    stat = ERR
	    go to nextregion_
	}
	call rg_lsetp (ls, RBUF, rbuf)
	Memi[rg_lstatp(ls,RNPTS)+i-1] = npts

	# Get the input image subraster regions.
	c1 = rc1 + rg_lstatr (ls, SXSHIFT)
	c2 = rc2 + rg_lstatr (ls, SXSHIFT)
	l1 = rl1 + rg_lstatr (ls, SYSHIFT)
	l2 = rl2 + rg_lstatr (ls, SYSHIFT)
	#c1 = max (1, min (nimcols, c1))
	#c2 = min (nimcols, max (1, c2))
	#l1 = max (1, min (nimlines, l1))
	#l2 = min (nimlines, max (1, l2))
	ncols = (c2 - c1) / xstep + 1
	nlines = (l2 - l1) / ystep + 1

	# Move to the next region if current input region is off the image.
        if (c1 < 1 || c1 > nimcols || c2 > nimcols ||  c2 < 1 ||
	    l1 > nimlines || l1 < 1 || l2 < 1 || l2 > nimlines) {
            call rg_lstats (ls, IMAGE, Memc[str], SZ_LINE)
            call eprintf (
                "Input region %d: %s[%d:%d:%d,%d:%d:%d] is off image.\n")
		call pargi (i)
                call pargstr (Memc[str])
                call pargi (c1)
                call pargi (c2)
		call pargi (xstep)
                call pargi (l1)
                call pargi (l2)
		call pargi (ystep)
            stat = ERR
            goto nextregion_
        }

	# Move to the next region if current input region is too small.
        if (ncols < 3 || (IM_NDIM(im1) == 2 && nlines < 3)) {
            call rg_lstats (ls, IMAGE, Memc[str], SZ_LINE)
            call eprintf (
            "Input regions %d: %s[%d:%d:%d,%d:%d:%d] has too few points.\n")
		call pargi (i)
                call pargstr (Memc[str])
                call pargi (c1)
                call pargi (c2)
		call pargi (xstep)
                call pargi (l1)
                call pargi (l2)
		call pargi (ystep)
            stat = ERR
            goto nextregion_
        }

	# Get the image data.
	npts = rg_simget (im1, c1, c2, xstep, l1, l2, ystep, ibuf)
	if (npts < 9) {
	    stat = ERR
	    go to nextregion_
        }
	call rg_lsetp (ls, IBUF, ibuf)
	Memi[rg_lstatp(ls,INPTS)+i-1] = npts


nextregion_
	call sfree (sp)
	if (stat == ERR) {
	    call rg_lsetp (ls, RBUF, rbuf) 
	    if (ibuf != NULL)
		call mfree (ibuf, TY_REAL)
	    call rg_lsetp (ls, IBUF, NULL) 
	    call rg_lseti (ls, CNREGION, i)
	    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
	    return (ERR)
	} else {
	    call rg_lsetp (ls, RBUF, rbuf) 
	    call rg_lsetp (ls, IBUF, ibuf) 
	    call rg_lseti (ls, CNREGION, i)
	    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_NO
	    return (OK)
	}
end


# RG_LGMMM -- Compute the mean, median and mode of a data region

procedure rg_lgmmm (ls, i)

pointer	ls		#I pointer to the intensity scaling structure
int	i		#I the current region

int	npts
pointer	rbuf, ibuf, buf
real	sigma, dmin, dmax
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lmode(), rg_lstatr()

begin
	# Test that the data buffers exist and contain data.
	rbuf = rg_lstatp (ls, RBUF)
	ibuf = rg_lstatp (ls, IBUF)
	npts = Memi[rg_lstatp (ls, RNPTS)+i-1]
	if (rbuf == NULL || npts <= 0) {
	    Memr[rg_lstatp(ls,RMEAN)+i-1] = 0.0
	    Memr[rg_lstatp(ls,RMEDIAN)+i-1] = 0.0
	    Memr[rg_lstatp(ls,RMODE)+i-1] = 0.0
	    Memr[rg_lstatp(ls,RSIGMA)+i-1] = 0.0
	    Memr[rg_lstatp(ls,IMEAN)+i-1] = 0.0
	    Memr[rg_lstatp(ls,IMEDIAN)+i-1] = 0.0
	    Memr[rg_lstatp(ls,IMODE)+i-1] = 0.0
	    Memr[rg_lstatp(ls,ISIGMA)+i-1] = 0.0
	    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
	    return
	}
	call malloc (buf, npts, TY_REAL)

	# Compute the mean, median, and mode of the reference region but
	# don't recompute the reference region statistics needlessly.
	if ((!IS_INDEFR(rg_lstatr(ls,DATAMIN)) || !IS_INDEFR(rg_lstatr(ls,
	    DATAMAX))) && (rg_lstati(ls,BSALGORITHM) != LS_FIT ||
	    rg_lstati(ls,BZALGORITHM) != LS_FIT)) {
	    call alimr (Memr[rbuf], npts, dmin, dmax)
	    if (!IS_INDEFR(rg_lstatr(ls,DATAMIN))) {
		if (dmin < rg_lstatr(ls,DATAMIN)) {
		    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
		    call eprintf (
		        "Reference region %d contains data < datamin\n")
			call pargi (i)
		}
	    }
	    if (!IS_INDEFR(rg_lstatr(ls,DATAMAX))) {
		if (dmax > rg_lstatr(ls,DATAMAX)) {
		    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
		    call eprintf (
		        "Reference region %d contains data > datamax\n")
			call pargi (i)
		}
	    }
	}
	call aavgr (Memr[rbuf], npts, Memr[rg_lstatp(ls,RMEAN)+i-1], sigma)
	Memr[rg_lstatp(ls,RSIGMA)+i-1] = sigma / sqrt (real(npts))
	call asrtr (Memr[rbuf], Memr[buf], npts)
	if (mod (npts,2) == 1)
	    Memr[rg_lstatp(ls,RMEDIAN)+i-1] = Memr[buf+npts/2]
	else
	    Memr[rg_lstatp(ls,RMEDIAN)+i-1] = (Memr[buf+npts/2-1] +
	            Memr[buf+npts/2]) / 2.0
	Memr[rg_lstatp(ls,RMODE)+i-1] = rg_lmode (Memr[buf], npts,
	    LMODE_NMIN, LMODE_ZRANGE, LMODE_ZBIN, LMODE_ZSTEP)
	sigma = sqrt ((max (Memr[rg_lstatp(ls,RMEAN)+i-1], 0.0) /
	    rg_lstatr(ls,RGAIN) + (rg_lstatr(ls,RREADNOISE) /
	    rg_lstatr (ls,RGAIN)) ** 2) / npts)
	Memr[rg_lstatp(ls,RSIGMA)+i-1] =
	    min (Memr[rg_lstatp(ls,RSIGMA)+i-1], sigma)

	if (ibuf == NULL) {
	    Memr[rg_lstatp(ls,IMEAN)+i-1] = Memr[rg_lstatp(ls,RMEAN)+i-1]
	    Memr[rg_lstatp(ls,IMEDIAN)+i-1] = Memr[rg_lstatp(ls,RMEDIAN)+i-1]
	    Memr[rg_lstatp(ls,IMODE)+i-1] = Memr[rg_lstatp(ls,RMODE)+i-1]
	    Memr[rg_lstatp(ls,ISIGMA)+i-1] = Memr[rg_lstatp(ls,RSIGMA)+i-1]
	    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
	    call mfree (buf, TY_REAL)
	    return
	}

	# Compute the mean, median, and mode of the input region.
	if ((!IS_INDEFR(rg_lstatr(ls,DATAMIN)) || !IS_INDEFR(rg_lstatr(ls,
	        DATAMAX))) && (rg_lstati(ls,BSALGORITHM) != LS_FIT ||
		rg_lstati(ls,BZALGORITHM) != LS_FIT)) {
	    call alimr (Memr[ibuf], npts, dmin, dmax)
	    if (!IS_INDEFR(rg_lstatr(ls,DATAMIN))) {
		if (dmin < rg_lstatr(ls,DATAMIN)) {
		    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
		    call eprintf ("Input region %d contains data < datamin\n")
			call pargi (i)
		}
	    }
	    if (!IS_INDEFR(rg_lstatr(ls,DATAMAX))) {
		if (dmax > rg_lstatr(ls,DATAMAX)) {
		    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADREGION
		    call eprintf ("Input region %d contains data > datamax\n")
			call pargi (i)
		}
	    }
	}
	call aavgr (Memr[ibuf], npts, Memr[rg_lstatp(ls,IMEAN)+i-1], sigma)
	Memr[rg_lstatp(ls,ISIGMA)+i-1] = sigma / sqrt (real(npts))
	call asrtr (Memr[ibuf], Memr[buf], npts)
	if (mod (npts,2) == 1)
	    Memr[rg_lstatp(ls,IMEDIAN)+i-1] = Memr[buf+npts/2]
	else
	    Memr[rg_lstatp(ls,IMEDIAN)+i-1] = (Memr[buf+npts/2-1] +
	        Memr[buf+npts/2]) / 2.0
	Memr[rg_lstatp(ls,IMODE)+i-1] = rg_lmode (Memr[buf], npts, LMODE_NMIN,
	    LMODE_ZRANGE, LMODE_ZBIN, LMODE_ZSTEP)
	sigma = sqrt ((max (Memr[rg_lstatp(ls,IMEAN)+i-1], 0.0) /
	    rg_lstatr(ls,IGAIN) + (rg_lstatr(ls,IREADNOISE) /
	    rg_lstatr (ls,IGAIN)) ** 2) / npts)
	Memr[rg_lstatp(ls,ISIGMA)+i-1] =
	    min (Memr[rg_lstatp(ls,ISIGMA)+i-1], sigma)


	call mfree (buf, TY_REAL)
end


# RG_LBSZFIT -- Compute the bscale and bzero factor for a single region.

int procedure rg_lbszfit (ls, i, bscale, bzero, bserr, bzerr)

pointer	ls		#I pointer to the intensity scaling strucuture
int	i		#I the number of the current region
real	bscale		#O the computed bscale factor
real	bzero		#O the computed bzero factor
real	bserr		#O the computed error in bscale
real	bzerr		#O the computed error in bzero

int	stat
real	bjunk, chi
bool	fp_equalr()
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	stat = OK

	# Compute the bscale factor.
	switch (rg_lstati (ls, BSALGORITHM)) {
	case LS_NUMBER:
	    bscale = rg_lstatr (ls, CBSCALE)
	    bserr = 0.0
	    chi = INDEFR
	case LS_MEAN:
	    if (fp_equalr (0.0, Memr[rg_lstatp(ls,IMEAN)+i-1])) {
		bscale = 1.0
		bserr = 0.0
	    } else {
		bscale = Memr[rg_lstatp(ls, RMEAN)+i-1] /
		    Memr[rg_lstatp (ls, IMEAN)+i-1]
	        if (fp_equalr (0.0, Memr[rg_lstatp(ls,RMEAN)+i-1]))
	            bserr = 0.0
		else
	            bserr = abs (bscale) * sqrt ((Memr[rg_lstatp(ls,
		        RSIGMA)+i-1] / Memr[rg_lstatp(ls,RMEAN)+i-1]) ** 2 +
		        (Memr[rg_lstatp(ls, ISIGMA)+i-1] /
		        Memr[rg_lstatp(ls,IMEAN)+i-1]) ** 2)
	    }
	    chi = INDEFR
	case LS_MEDIAN:
	    if (fp_equalr (0.0, Memr[rg_lstatp(ls,IMEDIAN)+i-1])) {
		bscale = 1.0
		bserr= 0.0
	    } else {
		bscale = Memr[rg_lstatp (ls,RMEDIAN)+i-1] /
		    Memr[rg_lstatp(ls,IMEDIAN)+i-1]
	        if (fp_equalr (0.0, Memr[rg_lstatp(ls,RMEDIAN)+i-1])) 
	            bserr = 0.0
		else
	            bserr = abs (bscale) * sqrt ((Memr[rg_lstatp(ls,
		        RSIGMA)+i-1] / Memr[rg_lstatp(ls,RMEDIAN)+i-1]) ** 2 +
		        (Memr[rg_lstatp(ls, ISIGMA)+i-1] / Memr[rg_lstatp(ls,
		        IMEDIAN)+i-1]) ** 2)
	    }
	    chi = INDEFR
	case LS_MODE:
	    if (fp_equalr (0.0, Memr[rg_lstatp (ls,IMODE)+i-1])) {
		bscale = 1.0
		bserr = 0.0
	    } else {
		bscale = Memr[rg_lstatp (ls, RMODE)+i-1] /
		    Memr[rg_lstatp (ls, IMODE)+i-1]
	        if (fp_equalr (0.0, Memr[rg_lstatp (ls,RMODE)+i-1]))
	            bserr = 0.0
		else
	            bserr = abs (bscale) * sqrt ((Memr[rg_lstatp(ls,
		        RSIGMA)+i-1] / Memr[rg_lstatp(ls,RMODE)+i-1]) ** 2 +
			(Memr[rg_lstatp(ls, ISIGMA)+i-1] / Memr[rg_lstatp(ls,
			IMODE)+i-1]) ** 2)
	    }
	    chi = INDEFR
	case LS_FIT:
	    call rg_llsqfit (ls, i, bscale, bzero, bserr, bzerr, chi)
	case LS_PHOTOMETRY:
	    if (IS_INDEFR(Memr[rg_lstatp(ls,RMAG)+i-1]) ||
	        IS_INDEFR(Memr[rg_lstatp(ls,IMAG)+i-1])) {
		bscale = 1.0
		bserr = 0.0
	    } else {
	        bscale = 10.0 ** ((Memr[rg_lstatp(ls,IMAG)+i-1] -
		    Memr[rg_lstatp(ls,RMAG)+i-1]) / 2.5)
	        if (IS_INDEFR(Memr[rg_lstatp(ls,RMAGERR)+i-1]) ||
	            IS_INDEFR(Memr[rg_lstatp(ls,IMAGERR)+i-1]))
	            bserr = 0.0
		else
		    bserr = 0.4 * log (10.0) * bscale *
		        sqrt (Memr[rg_lstatp(ls,RMAGERR)+i-1] ** 2 +
			Memr[rg_lstatp(ls,IMAGERR)+i-1] ** 2)
	    }
	    chi = INDEFR
	default:
	    bscale = 1.0
	    bserr = 0.0
	    chi = INDEFR
	}

	# Compute the bzero factor.
	switch (rg_lstati (ls, BZALGORITHM)) {
	case LS_NUMBER:
	    bzero = rg_lstatr (ls, CBZERO)
	    bzerr = 0.0
	    chi = INDEFR
	case LS_MEAN:
	    if (rg_lstati(ls, BSALGORITHM) == LS_NUMBER) {
	        bzero = Memr[rg_lstatp(ls,RMEAN)+i-1] - Memr[rg_lstatp(ls,
		    IMEAN)+i-1]
	        bzerr = sqrt (Memr[rg_lstatp(ls,RSIGMA)+i-1] ** 2 +
	            Memr[rg_lstatp(ls,ISIGMA)+i-1] ** 2)
	    } else {
		bzero = 0.0
		bzerr = 0.0
	    }
	    chi = INDEFR
	case LS_MEDIAN:
	    if (rg_lstati(ls, BSALGORITHM) == LS_NUMBER) {
	        bzero = Memr[rg_lstatp(ls,RMEDIAN)+i-1] -
	            Memr[rg_lstatp(ls,IMEDIAN)+i-1]
	        bzerr = sqrt (Memr[rg_lstatp(ls,RSIGMA)+i-1] ** 2 +
	            Memr[rg_lstatp(ls,ISIGMA)+i-1] ** 2)
	    } else {
		bzero = 0.0
		bzerr = 0.0
	    }
	    chi = INDEFR
	case LS_MODE:
	    if (rg_lstati(ls, BSALGORITHM) == LS_NUMBER) {
	        bzero = Memr[rg_lstatp(ls,RMODE)+i-1] - Memr[rg_lstatp(ls,
		    IMODE)+i-1]
	        bzerr = sqrt (Memr[rg_lstatp(ls,RSIGMA)+i-1] ** 2 +
	            Memr[rg_lstatp(ls,ISIGMA)+i-1] ** 2)
	    } else {
		bzero = 0.0
		bzerr = 0.0
	    }
	    chi = INDEFR
	case LS_FIT:
	    if (rg_lstati(ls, BSALGORITHM) == LS_NUMBER)
	        call rg_llsqfit (ls, i, bjunk, bzero, bjunk, bzerr, chi)
	case LS_PHOTOMETRY:
	    if (IS_INDEFR(Memr[rg_lstatp(ls,RSKY)+i-1]) ||
	        IS_INDEFR(Memr[rg_lstatp(ls,ISKY)+i-1])) {
	        bzero = 0.0
	        bzerr = 0.0
	    } else {
	        bzero = Memr[rg_lstatp(ls,RSKY)+i-1] - bscale *
		    Memr[rg_lstatp(ls,ISKY)+i-1]
	        if (IS_INDEFR(Memr[rg_lstatp(ls,RSKYERR)+i-1]) ||
	            IS_INDEFR(Memr[rg_lstatp(ls,ISKYERR)+i-1]))
	            bzerr = 0.0
		else
		    bzerr = sqrt (Memr[rg_lstatp(ls,RSKYERR)+i-1] ** 2 +
		        bserr ** 2 * Memr[rg_lstatp(ls,ISKY)+i-1] ** 2 +
			bscale ** 2 * Memr[rg_lstatp(ls,ISKYERR)+i-1] ** 2)
			
	    }
	    chi = INDEFR
	default:
	    bzero = 0.0
	    bzerr = 0.0
	    chi = INDEFR
	}

	# Store the results.
	Memr[rg_lstatp(ls,RBSCALE)+i-1] = bscale
	Memr[rg_lstatp(ls,RBZERO)+i-1] = bzero
	Memr[rg_lstatp(ls,RBSCALEERR)+i-1] = bserr
	Memr[rg_lstatp(ls,RBZEROERR)+i-1] = bzerr
	Memr[rg_lstatp(ls,RCHI)+i-1] = chi

	return (stat)
end


# RG_LBSZAVG -- Compute the final scaling parameters.

procedure rg_lbszavg (ls, avbscale, avbzero, avbserr, avbzerr, tbscale,
	tbzero, tbserr, tbzerr)

pointer	ls		#I pointer to the intensity scaling strucuture
real	avbscale	#I the computed bscale factor
real	avbzero		#I the computed bzero factor
real	avbserr		#I the computed error in bscale
real	avbzerr		#I the computed error in bzero
real	tbscale		#O the computed bscale factor
real	tbzero		#O the computed bzero factor
real	tbserr		#O the computed error in bscale
real	tbzerr		#O the computed error in bzero

int	i, bsalg, bzalg, nregions
pointer	sp, weight
real	answers[MAX_NFITPARS]
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	bsalg = rg_lstati (ls, BSALGORITHM)
	bzalg = rg_lstati (ls, BZALGORITHM)
	nregions = rg_lstati (ls, NREGIONS)

	call smark (sp)
	call salloc (weight, nregions, TY_REAL)

	if (bsalg == LS_MEAN || bzalg == LS_MEAN) {
	    do i = 1, nregions {
		if (IS_INDEFR(Memr[rg_lstatp(ls,IMEAN)+i-1]) ||
		    IS_INDEFR(Memr[rg_lstatp(ls,RMEAN)+i-1]) ||
		    Memi[rg_lstatp(ls,RDELETE)+i-1] != LS_NO)
		    Memr[weight+i-1] = 0.0
		else
		    Memr[weight+i-1] = 1.0
	    }
	    call ll_lsqf1 (Memr[rg_lstatp(ls,IMEAN)], Memr[rg_lstatp(ls,
	        RMEAN)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		answers)
	    if (nregions > 2 && rg_lstati(ls,NREJECT) > 0 &&
	        (! IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
		! IS_INDEFR(rg_lstatr(ls,HIREJECT)))) {
	        call ll_rlsqf1 (Memr[rg_lstatp(ls,IMEAN)], Memr[rg_lstatp(ls,
	            RMEAN)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		    RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		    answers, rg_lstati(ls,NREJECT), rg_lstatr(ls,LOREJECT),
		    rg_lstatr(ls,HIREJECT))
	        do i = 1, nregions {
		    if (Memr[weight+i-1] <= 0.0 && Memi[rg_lstatp(ls,
		        RDELETE)+i-1] == LS_NO)
		        Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADSIGMA
	        }
	    }
	    if (IS_INDEFR(CHI[answers])) {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else if (bsalg == LS_MEAN && bzalg == LS_MEAN) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = YINCPT[answers]
	        tbzerr = EYINCPT[answers]
	    } else if (bsalg == LS_MEAN) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    }

	} else if (bsalg == LS_MEDIAN || bzalg == LS_MEDIAN) {
	    do i = 1, nregions {
		if (IS_INDEFR(Memr[rg_lstatp(ls,IMEDIAN)+i-1]) ||
		    IS_INDEFR(Memr[rg_lstatp(ls,RMEDIAN)+i-1]) ||
		    Memi[rg_lstatp(ls,RDELETE)+i-1] != LS_NO)
		    Memr[weight+i-1] = 0.0
		else
		    Memr[weight+i-1] = 1.0
	    }
	    call ll_lsqf1 (Memr[rg_lstatp(ls,IMEDIAN)], Memr[rg_lstatp(ls,
	        RMEDIAN)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		answers)
	    if (nregions > 2 && rg_lstati(ls,NREJECT) > 0 &&
	        (! IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
	        ! IS_INDEFR(rg_lstatr(ls,HIREJECT)))) {
	        call ll_rlsqf1 (Memr[rg_lstatp(ls,IMEDIAN)], Memr[rg_lstatp(ls,
	            RMEDIAN)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		    RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		    answers, rg_lstati(ls,NREJECT), rg_lstatr(ls,LOREJECT),
		    rg_lstatr(ls,HIREJECT))
	        do i = 1, nregions {
		    if (Memr[weight+i-1] <= 0.0 && Memi[rg_lstatp(ls,
		        RDELETE)+i-1] == LS_NO)
		        Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADSIGMA
	        }
	    }
	    if (IS_INDEFR(CHI[answers])) {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else if (bsalg == LS_MEDIAN && bzalg == LS_MEDIAN) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = YINCPT[answers]
	        tbzerr = EYINCPT[answers]
	    } else if (bsalg == LS_MEDIAN) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    }
	} else if (bsalg == LS_MODE || bzalg == LS_MODE) {
	    do i = 1, nregions {
		if (IS_INDEFR(Memr[rg_lstatp(ls,IMODE)+i-1]) ||
		    IS_INDEFR(Memr[rg_lstatp(ls,RMODE)+i-1]) ||
		    Memi[rg_lstatp(ls,RDELETE)+i-1] != LS_NO)
		    Memr[weight+i-1] = 0.0
		else
		    Memr[weight+i-1] = 1.0
	    }
	    call ll_lsqf1 (Memr[rg_lstatp(ls,IMODE)], Memr[rg_lstatp(ls,
	        RMODE)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		answers)
	    if (nregions > 2 && rg_lstati(ls,NREJECT) > 0 &&
	        (! IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
	        ! IS_INDEFR(rg_lstatr(ls,HIREJECT)))) {
	        call ll_rlsqf1 (Memr[rg_lstatp(ls,IMODE)], Memr[rg_lstatp(ls,
	            RMODE)], Memr[rg_lstatp(ls,ISIGMA)], Memr[rg_lstatp(ls,
		    RSIGMA)], Memr[weight], nregions, rg_lstati(ls,MAXITER),
		    answers, rg_lstati(ls,NREJECT), rg_lstatr(ls,LOREJECT),
		    rg_lstatr(ls,HIREJECT))
	        do i = 1, nregions {
		    if (Memr[weight+i-1] <= 0.0 && Memi[rg_lstatp(ls,
		        RDELETE)+i-1] == LS_NO)
		        Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADSIGMA
	        }
	    }
	    if (IS_INDEFR(CHI[answers])) {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else if (bsalg == LS_MODE && bzalg == LS_MODE) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = YINCPT[answers]
	        tbzerr = EYINCPT[answers]
	    } else if (bsalg == LS_MODE) {
	        tbscale = SLOPE[answers]
	        tbserr = ESLOPE[answers]
	        tbzero = avbzero
	        tbzerr = avbzerr
	    } else {
	        tbscale = avbscale
	        tbserr = avbserr
	        tbzero = avbzero
	        tbzerr = avbzerr
	    }
	} else {
	    tbscale = avbscale
	    tbzero = avbzero
	    tbserr = avbserr
	    tbzerr = avbzerr
	}


	call sfree (sp)
end


# RG_LFILE -- Fetch the scaling parameters from the datafile.

procedure rg_lfile (db, ls, bscale, bzero, bserr, bzerr)

pointer	db		#I pointer to the database file
pointer	ls		#I pointer to the intensity scaling structure
real	bscale		#O the average scaling parameter
real	bzero		#O the average offset parameter
real	bserr		#O the error in bscale
real	bzerr		#O the error in bzero

int	rec
pointer	sp, record
int	dtlocate()
real	dtgetr()

begin
	call smark (sp)
	call salloc (record, SZ_FNAME, TY_CHAR)

	call rg_lstats (ls, RECORD, Memc[record], SZ_FNAME)
	iferr {
	    rec = dtlocate (db, Memc[record])
	    bscale = dtgetr (db, rec, "bscale")
	    bzero = dtgetr (db, rec, "bzero")
	    bserr = dtgetr (db, rec, "bserr")
	    bzerr = dtgetr (db, rec, "bzerr")
	} then {
	    bscale = 1.0
	    bzero = 0.0
	    bserr = INDEFR
	    bzerr = INDEFR
	}

	call sfree (sp)
end


# RG_SIMGET -- Fill a buffer from a specified region of the image including a
# step size in x and y.

int procedure rg_simget (im, c1, c2, cstep, l1, l2, lstep, ptr)

pointer im              #I the pointer to the iraf image
int     c1, c2          #I the column limits
int     cstep           #I the column step size
int     l1, l2          #I the line limits
int     lstep           #I the line step size
pointer ptr             #I the pointer to the output buffer

int	i, j, ncols, nlines, npts
pointer	iptr, buf
pointer imgs2r()

begin
	ncols = (c2 - c1) / cstep + 1
	nlines = (l2 - l1) / lstep + 1
	npts = ncols * nlines
        call malloc (ptr, npts, TY_REAL)

        iptr = ptr
        do j = l1, l2, lstep {
            buf = imgs2r (im, c1, c2, j, j)
            do i = 1, ncols {
                Memr[iptr+i-1] = Memr[buf]
                buf = buf + cstep
            }
            iptr = iptr + ncols
        }

        return (npts)
end


# RG_LMODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

real procedure rg_lmode (a, npts, nmin, zrange, fzbin, fzstep)

real	a[npts]			#I the sorted input data array
int	npts			#I the number of points
int	nmin			#I the minimum number of points
real	zrange			#I fraction of pixels around median to use
real	fzbin			#I the bin size for the mode search
real	fzstep			#I the step size for the mode search

int	x1, x2, x3, nmax
real	zstep, zbin, y1, y2, mode
bool	fp_equalr()

begin
	# If there are too few points return the median.
	if (npts < nmin) {
	    if (mod (npts,2) == 1)
	        return (a[1+npts/2])
	    else
	        return ((a[npts/2] + a[1+npts/2]) / 2.0)
	}

	# Compute the data range that will be used to do the mode search.
	# If the data has no range then the constant value will be returned.
	x1 = max (1, int (1.0 + npts * (1.0 - zrange) / 2.0))
	x3 = min (npts, int (1.0 + npts * (1.0 + zrange) / 2.0))
	if (fp_equalr (a[x1], a[x3]))
	    return (a[x1])

	# Compute the bin and step size. The bin size is based on the
	# data range over a fraction of the pixels around the median
	# and a bin step which may be smaller than the bin size.

	zstep = fzstep * (a[x3] - a[x1])
	zbin = fzbin * (a[x3] - a[x1])

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
                    mode = (a[(x2+x1)/2] + a[(x2+x1)/2+1]) / 2.0
            }
        }

	return (mode)
end


# RG_LLSQFIT -- Compute the bscale and bzero factors by doing a least squares
# fit to the region data. For this technque to be successful the data must
# be registered and psf matched.

procedure rg_llsqfit (ls, i, bscale, bzero, bserr, bzerr, chi)

pointer	ls			#I pointer to the intensity scaling structure
int	i			#I the current region
real	bscale			#O the computed bscale factor
real	bzero			#O the computed bzero factor
real	bserr			#O the estimated error in bscale
real	bzerr			#O the estimated error in bzero
real	chi			#O the output chi at unit weight

int	j, npts
pointer	rbuf, ibuf, rerr, ierr, weight
real	rgain, igain, rrnoise, irnoise, answers[MAX_NFITPARS]
real	datamin, datamax
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	# Get the data pointers.
	rbuf = rg_lstatp (ls, RBUF)
	ibuf = rg_lstatp (ls, IBUF)

	# Allocate space for the error and weight arrays.
	npts = Memi[rg_lstatp(ls,RNPTS)+i-1]
	call malloc (rerr, npts, TY_REAL)
	call malloc (ierr, npts, TY_REAL)
	call malloc (weight, npts, TY_REAL)

	# Compute the errors. 
	rgain = rg_lstatr (ls, RGAIN)
	igain = rg_lstatr (ls, IGAIN)
	rrnoise = rg_lstatr (ls, RREADNOISE) ** 2 / rgain
	irnoise = rg_lstatr (ls, IREADNOISE) ** 2 / igain
	do j = 1, npts {
	    Memr[rerr+j-1] = (Memr[rbuf+j-1] + rrnoise) / rgain
	    Memr[ierr+j-1] = (Memr[ibuf+j-1] + irnoise) / igain
	}

	# Compute the weights.
	if (IS_INDEFR(rg_lstatr(ls,DATAMIN)) && IS_INDEFR(ls,DATAMAX))
	    call amovkr (1.0, Memr[weight], npts)
	else {
	    if (IS_INDEFR(rg_lstatr(ls,DATAMIN)))
		datamin = -MAX_REAL
	    else
		datamin = rg_lstatr (ls, DATAMIN)
	    if (IS_INDEFR(rg_lstatr(ls,DATAMAX)))
		datamax = MAX_REAL
	    else
		datamax = rg_lstatr (ls, DATAMAX)
	    do j = 1, npts {
		if (Memr[rbuf+j-1] < datamin || Memr[rbuf+j-1] > datamax)
		    Memr[weight+j-1] = 0.0
		else if (Memr[ibuf+j-1] < datamin || Memr[ibuf+j-1] > datamax)
		    Memr[weight+j-1] = 0.0
		else
		    Memr[weight+j-1] = 1.0
	    }
	}

	# Compute the fit.
	call ll_lsqf1 (Memr[ibuf], Memr[rbuf], Memr[ierr], Memr[rerr],
	    Memr[weight], npts, rg_lstati(ls, MAXITER), answers)

	# Perform the rejection cycle.
	if (npts > 2 && rg_lstati(ls,NREJECT) > 0 &&
	    (! IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
	    ! IS_INDEFR(rg_lstatr(ls,HIREJECT))))
	    call ll_rlsqf1 (Memr[ibuf], Memr[rbuf], Memr[ierr], Memr[rerr],
		Memr[weight], npts, rg_lstati(ls,MAXITER), answers,
		rg_lstati(ls,NREJECT), rg_lstatr(ls,LOREJECT),
		rg_lstatr(ls,HIREJECT))
	bscale = SLOPE[answers]
	bzero = YINCPT[answers]
	bserr = ESLOPE[answers]
	bzerr = EYINCPT[answers]
	chi = CHI[answers]

	# Free the working space.
	call mfree (rerr, TY_REAL)
	call mfree (ierr, TY_REAL)
	call mfree (weight, TY_REAL)
end


# RG_RAVSTATS -- Compute the average statistics.

procedure rg_ravstats (ls, sumbscale, sumbzero, sumwbscale, sumwbzero, sumbserr,
	sumbzerr, bserr, bzerr, avbscale, avbzero, avbserr, avbzerr, ngood)

pointer	ls				#I pointer to the linmatch structure
double	sumbscale			#I/O sum of the bscale values
double	sumbzero			#I/O sum of the bzero values
double	sumwbscale			#I/O sum of the weighted bscale values
double	sumwbzero			#I/O sum of the weighted bzero values
double	sumbserr			#I/O sum of the bscale error
double	sumbzerr			#I/O sum of the bscale error
real	bserr				#I/O the bscale error of 1 observation
real	bzerr				#I/O the bzero error of 1 observation
real	avbscale			#I/O the average bscale factor
real	avbzero				#I/O the average bzero factor
real	avbserr				#O the average bscale error factor
real	avbzerr				#O the average bzero error factor
int	ngood				#I/O the number of good data values

int	i, nregions, nrej, nbad
real	sigbscale, sigbzero, lobscale, hibscale, lobzero, hibzero
real	bscale, bzero, bsresid, bzresid
double	dw
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lsigma(), rg_lstatr()

begin
	nregions = rg_lstati (ls,NREGIONS)

	nrej = 0
	repeat {

	    # Compute sigma.
	    sigbscale = rg_lsigma (Memr[rg_lstatp(ls,RBSCALE)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, avbscale)
	    if (sigbscale <= 0.0)
		break
	    sigbzero = rg_lsigma (Memr[rg_lstatp(ls,RBZERO)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, avbzero)
	    if (sigbzero <= 0.0)
		break

	    if (IS_INDEFR(rg_lstatr(ls,LOREJECT))) {
		lobscale = -MAX_REAL
		lobzero = -MAX_REAL
	    } else {
		lobscale = -sigbscale * rg_lstatr (ls, LOREJECT)
		lobzero = -sigbzero * rg_lstatr (ls, LOREJECT)
	    }
	    if (IS_INDEFR(rg_lstatr(ls,HIREJECT))) {
		hibscale = MAX_REAL
		hibzero = MAX_REAL
	    } else {
		hibscale = sigbscale * rg_lstatr (ls, HIREJECT)
		hibzero = sigbzero * rg_lstatr (ls, HIREJECT)
	    }

	    nbad = 0
	    do i = 1, nregions {
		if (Memi[rg_lstatp(ls,RDELETE)+i-1] != LS_NO)
		    next
		bscale = Memr[rg_lstatp(ls,RBSCALE)+i-1]
		if (IS_INDEFR(bscale))
		    next
		bzero = Memr[rg_lstatp(ls,RBZERO)+i-1]
		if (IS_INDEFR(bzero))
		    next
		bserr = Memr[rg_lstatp(ls,RBSCALEERR)+i-1]
		bsresid = bscale - avbscale
		bzerr = Memr[rg_lstatp(ls,RBZEROERR)+i-1]
		bzresid = bzero - avbzero
		if (bsresid >= lobscale && bsresid <= hibscale && bzresid >=
		    lobzero && bzresid <= hibzero)
		    next

		if (bserr <= 0.0)
		    dw = 1.0d0
		else
		    dw = 1.0d0 / bserr ** 2
		sumbscale = sumbscale - dw * bscale
		sumbserr = sumbserr - dw * bscale * bscale
		sumwbscale = sumwbscale - dw

		if (bzerr <= 0.0)
		    dw = 1.0d0
		else
		    dw = 1.0d0 / bzerr ** 2
		sumbzero = sumbzero - dw * bzero
		sumbzerr = sumbzerr - dw * bzero * bzero
		sumwbzero = sumwbzero - dw

		nbad = nbad + 1
		Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_BADSIGMA
		ngood = ngood - 1
	    }

	    if (nbad <= 0)
		break

	    call rg_avstats (sumbscale, sumbzero, sumwbscale, sumwbzero,
	        sumbserr, sumbzerr, bserr, bzerr, avbscale, avbzero,
		avbserr, avbzerr, ngood)
	    if (ngood <= 0)
		break

	    nrej = nrej + 1

	} until (nrej >= rg_lstati(ls,NREJECT))
end


# RG_AVSTATS -- Compute the average statistics.

procedure rg_avstats (sumbscale, sumbzero, sumwbscale, sumwbzero, sumbserr,
	sumbzerr, bserr, bzerr, avbscale, avbzero, avbserr, avbzerr, ngood)

double	sumbscale			#I sum of the bscale values
double	sumbzero			#I sum of the bzero values
double	sumwbscale			#I sum of the weighted bscale values
double	sumwbzero			#I sum of the weighted bzero values
double	sumbserr			#I sum of the bscale error
double	sumbzerr			#I sum of the bscale error
real	bserr				#I the bscale error of 1 observation
real	bzerr				#I the bzero error of 1 observation
real	avbscale			#O the average bscale factor
real	avbzero				#O the average bzero factor
real	avbserr				#O the average bscale error factor
real	avbzerr				#O the average bzero error factor
int	ngood				#I the number of good data values

begin
	# Compute the average scaling factors.
	if (ngood > 0) {
	    avbscale = sumbscale / sumwbscale
	    if (ngood > 1) {
		avbserr = ngood * (sumbserr / sumwbscale - (sumbscale /
		    sumwbscale) ** 2) /
		    (ngood - 1)
		if (avbserr >= 0.0)
		    avbserr = sqrt (avbserr)
		else
		    avbserr = 0.0
	    } else
	        avbserr = bserr
	    avbzero = sumbzero / sumwbzero
	    if (ngood > 1) {
		avbzerr = ngood * (sumbzerr / sumwbzero - (sumbzero /
		    sumwbzero) ** 2) /
		    (ngood - 1)
		if (avbzerr >= 0.0)
		    avbzerr = sqrt (avbzerr)
		else
		    avbzerr = 0.0
	    } else
	        avbzerr = bzerr
	} else {
	    avbscale = 1.0
	    avbzero = 0.0
	    avbserr = INDEFR 
	    avbzerr = INDEFR 
	}
end


# RG_LSIGMA -- Compute the standard deviation of an array taken into
# account any existing deletions.

real procedure rg_lsigma (a, del, npts, mean)

real	a[ARB]			#I the input array
int	del[ARB]		#I the deletions array
int	npts			#I the number of points in the array
real	mean			#I the mean of the array

int	i, ngood
double	sumsq

begin
	sumsq = 0.0d0
	ngood = 0

	do i = 1, npts {
	    if (del[i] != LS_NO)
		next
	    if (IS_INDEFR(a[i]))
		next
	    sumsq = sumsq + (a[i] - mean) ** 2
	    ngood = ngood + 1
	}

	if (ngood <= 1)
	    return (0.0)
	else if (sumsq <= 0.0)
	    return (0.0)
	else
	    return (sqrt (real (sumsq / (ngood - 1))))
end
