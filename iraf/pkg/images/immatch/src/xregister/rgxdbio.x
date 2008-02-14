include "xregister.h"

# RG_XWREC -- Procedure to write out the whole record.

procedure rg_xwrec (db, dformat, xc)

pointer	db		#I pointer to the database file
int	dformat		#I is the shifts file in database format
pointer	xc		#I pointer to the cross correlation structure

int	i, nregions, ngood, c1, c2, l1, l2, xlag, ylag
pointer	sp, image, prc1, prc2, prl1, prl2, pxshift, pyshift
real	xin, yin, xout, yout, xavshift, yavshift
int	rg_xstati()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Write the header record.
	if (dformat == YES)
	    call rg_xdbparams (db, xc)

	# Fetch the pointers to the columns.
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)
	pxshift = rg_xstatp (xc, XSHIFTS)
	pyshift = rg_xstatp (xc, YSHIFTS)
	nregions = rg_xstati (xc, NREGIONS)

	xavshift = 0.0
	yavshift = 0.0
	ngood = 0
	do i = 1, nregions {

	    xin = (Memi[prc1+i-1] + Memi[prc2+i-1]) / 2.0
	    yin = (Memi[prl1+i-1] + Memi[prl2+i-1]) / 2.0
	    if (rg_xstati (xc, NREFPTS) > 0) {
	        call rg_etransform (xc, xin, yin, xout, yout)
		xlag = xout - xin
		ylag = yout - yin
	    } else {
		xlag = rg_xstati (xc, XLAG)
		ylag = rg_xstati (xc, YLAG)
	    }
	    c1 = Memi[prc1+i-1] + xlag
	    c2 = Memi[prc2+i-1] + xlag
	    l1 = Memi[prl1+i-1] + ylag
	    l2 = Memi[prl2+i-1] + ylag

	    if (IS_INDEFR(Memr[pxshift+i-1]) || IS_INDEFR(Memr[pyshift+i-1])) {
		if (dformat == YES)
	            call rg_xdbshiftr (db, Memi[prc1+i-1], Memi[prc2+i-1],
		        Memi[prl1+i-1], Memi[prl2+i-1], c1, c2, l1, l2,
		        INDEFR, INDEFR)
	    } else {
		if (dformat == YES)
	            call rg_xdbshiftr (db, Memi[prc1+i-1], Memi[prc2+i-1],
		        Memi[prl1+i-1], Memi[prl2+i-1], c1, c2, l1, l2,
		        Memr[pxshift+i-1], Memr[pyshift+i-1])
		ngood = ngood + 1
	        xavshift = xavshift + Memr[pxshift+i-1]
	        yavshift = yavshift + Memr[pyshift+i-1]
	    }
	}

	# Compute the average shift.
	if (ngood <= 0) {
	    xavshift = 0.0
	    yavshift = 0.0
	} else {
	    xavshift = xavshift / ngood
	    yavshift = yavshift / ngood
	}
	call rg_xsetr (xc, TXSHIFT, xavshift)
	call rg_xsetr (xc, TYSHIFT, yavshift)

	if (dformat == YES)
	    call rg_xdbshift (db, xc)
	else {
	    call rg_xstats (xc, IMAGE, Memc[image], SZ_FNAME)
	    call fprintf (db, "%s  %g  %g\n")
		call pargstr (Memc[image])
		call pargr (xavshift)
		call pargr (yavshift)
	}

	call sfree (sp)
end


# RG_XDBPARAMS -- Write the cross-correlation parameters to the database file.

procedure rg_xdbparams (db, xc)

pointer	db		#I pointer to the database file
pointer	xc		#I pointer to the cross-correlation structure

pointer	sp, str
int	rg_xstati()
#real	rg_xstatr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Write out the time record was written.
	call dtput (db, "\n")
	call dtptime (db)

	# Write out the record name.
	call rg_xstats (xc, RECORD, Memc[str], SZ_FNAME)
	call dtput (db, "begin\t%s\n")
	    call pargstr (Memc[str])

	# Write the image names.
	call rg_xstats (xc, IMAGE, Memc[str], SZ_FNAME)
	call dtput (db, "\t%s\t\t%s\n")
	    call pargstr (KY_IMAGE)
	    call pargstr (Memc[str])
	call rg_xstats (xc, REFIMAGE, Memc[str], SZ_FNAME)
	call dtput (db, "\t%s\t%s\n")
	    call pargstr (KY_REFIMAGE)
	    call pargstr (Memc[str])

	call dtput (db, "\t%s\t%d\n")
	    call pargstr (KY_NREGIONS)
	    call pargi (rg_xstati (xc, NREGIONS))

	call sfree (sp)
end


# RG_XWREG -- Write out the results for each region individually into
# the shifts file.

procedure rg_xwreg (db, xc)

pointer	db		#I pointer to the database file
pointer	xc		#I pointer to the cross-correlation structure

int	i, nregions, c1, c2, l1, l2, xlag, ylag
pointer	prc1, prc2, prl1, prl2, pxshift, pyshift
real	xin, yin, xout, yout
int	rg_xstati()
pointer	rg_xstatp()

begin
	# Fetch the regions pointers.
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)
	pxshift = rg_xstatp (xc, XSHIFTS)
	pyshift = rg_xstatp (xc, YSHIFTS)
	nregions = rg_xstati (xc, NREGIONS)

	# Write out the reference image region(s) and the equivalent
	# input image regions.
	do i = 1, nregions {

	    xin = (Memi[prc1+i-1] + Memi[prc2+i-1]) / 2.0
	    yin = (Memi[prl1+i-1] + Memi[prl2+i-1]) / 2.0
	    if (rg_xstati (xc, NREFPTS) > 0) {
	        call rg_etransform (xc, xin, yin, xout, yout)
		xlag = xout - xin
		ylag = yout - yin
	    } else {
		xlag = rg_xstati (xc, XLAG)
		ylag = rg_xstati (xc, YLAG)
	    }
	    c1 = Memi[prc1+i-1] + xlag
	    c2 = Memi[prc2+i-1] + xlag
	    l1 = Memi[prl1+i-1] + ylag
	    l2 = Memi[prl2+i-1] + ylag

	    if (IS_INDEFR(Memr[pxshift+i-1]) || IS_INDEFR(Memr[pyshift+i-1]))
	        call rg_xdbshiftr (db, Memi[prc1+i-1], Memi[prc2+i-1],
		    Memi[prl1+i-1], Memi[prl2+i-1], c1, c2, l1, l2,
		    INDEFR, INDEFR)
	    else
	        call rg_xdbshiftr (db, Memi[prc1+i-1], Memi[prc2+i-1],
		    Memi[prl1+i-1], Memi[prl2+i-1], c1, c2, l1, l2,
		    Memr[pxshift+i-1], Memr[pyshift+i-1])
	}
end


# RG_XDBSHIFTR -- Write out the reference image section, input image
# section and x and y shifts for each region.

procedure rg_xdbshiftr (db, rc1, rc2, rl1, rl2, c1, c2, l1, l2, xshift, yshift)

pointer	db		#I pointer to the database file
int	rc1, rc2	#I reference region column limits
int	rl1, rl2	#I reference region line limits
int	c1, c2		#I image region column limits
int	l1, l2		#I image region line limits
real	xshift		#I x shift
real	yshift		#I y shift

begin
	call dtput (db,"\t[%d:%d,%d:%d]\t[%d:%d,%d:%d]\t%g\t%g\n")
	    call pargi (rc1)
	    call pargi (rc2)
	    call pargi (rl1)
	    call pargi (rl2)
	    call pargi (c1)
	    call pargi (c2)
	    call pargi (l1)
	    call pargi (l2)
	    call pargr (xshift)
	    call pargr (yshift)
end


# RG_XDBSHIFT -- Write the average shifts to the shifts database.

procedure rg_xdbshift (db, xc)

pointer	db		#I pointer to text database file
pointer	xc		#I pointer to the cross-correlation structure

real	rg_xstatr()

begin
	call dtput (db, "\t%s\t\t%g\n")
	    call pargstr (KY_TXSHIFT)
	    call pargr (rg_xstatr (xc, TXSHIFT))
	call dtput (db, "\t%s\t\t%g\n")
	    call pargstr (KY_TYSHIFT)
	    call pargr (rg_xstatr (xc, TYSHIFT))
end


# RG_XPWREC -- Print the computed shift for a region.

procedure rg_xpwrec (xc, i)

pointer	xc		#I pointer to the cross-correlation structure
int	i		#I the current region

int	xlag, ylag, c1, c2, l1, l2
pointer	prc1, prc2, prl1, prl2
real	xin, yin, rxlag, rylag
int	rg_xstati()
pointer	rg_xstatp()

begin
	# Fetch the pointers to the reference regions.
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)

	# Transform the reference region to the input region.
	xin = (Memi[prc1+i-1] + Memi[prc2+i-1]) / 2.0
	yin = (Memi[prl1+i-1] + Memi[prl2+i-1]) / 2.0
	if (rg_xstati (xc, NREFPTS) > 0) {
	    call rg_etransform (xc, xin, yin, rxlag, rylag)
	    xlag = rxlag - xin
	    ylag = rylag - yin
	} else {
	    xlag = rg_xstati (xc, XLAG)
	    ylag = rg_xstati (xc, YLAG)
	}

	c1 = Memi[prc1+i-1] + xlag
	c2 = Memi[prc2+i-1] + xlag
	l1 = Memi[prl1+i-1] + ylag
	l2 = Memi[prl2+i-1] + ylag

	# Print the results.
	call printf ("Region %d: [%d:%d,%d:%d]  [%d:%d,%d:%d]  %g  %g\n")
	    call pargi (i)
	    call pargi (Memi[prc1+i-1])
	    call pargi (Memi[prc2+i-1])
	    call pargi (Memi[prl1+i-1])
	    call pargi (Memi[prl2+i-1])
	    call pargi (c1)
	    call pargi (c2)
	    call pargi (l1)
	    call pargi (l2)
	    call pargr (Memr[rg_xstatp(xc,XSHIFTS)+i-1])
	    call pargr (Memr[rg_xstatp(xc,YSHIFTS)+i-1])
end
