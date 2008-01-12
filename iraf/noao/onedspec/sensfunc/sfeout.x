include	<error.h>
include	<ctype.h>
include	<mach.h>
include	<math/curfit.h>

define	NEPOINTS	100		# Number of points in extinction table

# SF_EOUT -- Output a revised extinction table.  This is only done if there
# is at least one residual extinction curve.  No assumption is made about
# overlapping extinction curves.  In the overlap the extinction corrections
# are averaged.

procedure sf_eout (wextn, extn, nextn, ecvs, necvs)

real	wextn[nextn]		# Standard extinction wavelengths
real	extn[nextn]		# Standard extinction values
int	nextn			# Number of standard extinction points
pointer	ecvs[necvs]		# Residual extinction curves (one for each beam)
int	necvs			# Number of residual extinction curves

int	i, j, fd, open(), scan(), nscan()
real	w, ext, wmin, wmax, dw, xmin, xmax, cvstatr(), cveval()
pointer	sp, fname, waves, extns, navg, cv

define	newfile_  99

begin
	# If there are no residual extinction values then do nothing.
	for (i=1; (i<=necvs) && (ecvs[i]==NULL); i=i+1)
	    ;
	if (i > necvs)
	    return

	# The output table consists of NEPOINTS.
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (waves, NEPOINTS, TY_REAL)
	call salloc (extns, NEPOINTS, TY_REAL)
	call salloc (navg, NEPOINTS, TY_INT)
	call aclrr (Memr[extns], NEPOINTS)
	call aclri (Memi[navg], NEPOINTS)

	# Open the extinction table.  If it fails allow the user to
	# enter a new name.

	call clgstr ("newextinction", Memc[fname], SZ_FNAME)
	for (i=fname; (Memc[i]!=EOS) && IS_WHITE(Memc[i]); i=i+1)
	if (Memc[i] == EOS) {
	    call sfree (sp)
	    return
	}

newfile_
	iferr (fd = open (Memc[i], NEW_FILE, TEXT_FILE)) {
	    call printf ("Cannot create %s -- Enter new extinction filename: ")
		call pargstr (Memc[fname])
	    call flush (STDOUT)
	    if (scan() != EOF) {
	        call gargwrd (Memc[fname], SZ_FNAME)
	        if (nscan() == 1)
	    	    goto newfile_
	    }
	    call sfree (sp)
	    call printf ("No revised extinction file created\n")
	    return
	}

	# Determine the range of the extinction table.
	wmin = MAX_REAL
	wmax = -MAX_REAL
	do i = 1, necvs {
	    if (ecvs[i] == NULL)
		next
	    wmin = min (wmin, cvstatr (ecvs[i], CVXMIN))
	    wmax = max (wmax, cvstatr (ecvs[i], CVXMAX))
	}
	dw = (wmax - wmin) / (NEPOINTS - 1)
	do i = 1, NEPOINTS
	    Memr[waves+i-1] = wmin + (i - 1) * dw

	# Average the residual extinctions and add the original extinction.
	do j = 1, necvs {
	    if (ecvs[j] == NULL)
		next
	    cv = ecvs[j]
	    xmin = cvstatr (cv, CVXMIN)
	    xmax = cvstatr (cv, CVXMAX)
	    do i = 1, NEPOINTS {
		w = Memr[waves+i-1]
		if ((w < xmin) || (w > xmax))
		    next
		Memr[extns+i-1] = Memr[extns+i-1] + cveval (cv, w)
		Memi[navg+i-1] = Memi[navg+i-1] + 1
	    }
	}
	do i = 1, NEPOINTS {
	    if (Memi[navg+i-1] > 0)
		Memr[extns+i-1] = Memr[extns+i-1] / Memi[navg+i-1]
	    w = Memr[waves+i-1]
	    call intrp (1, wextn, extn, nextn, w, ext, j)
	    Memr[extns+i-1] = Memr[extns+i-1] + ext
	}

	# Output extinction table.
	call fprintf (fd, "# Revised extinction table.\n")
	do i = 1, NEPOINTS {
	    call fprintf (fd, "%7.2f  %6.3f\n")
		call pargr (Memr[waves+i-1])
		call pargr (Memr[extns+i-1])
	}
	call close (fd)

	call sfree (sp)
end
