include	<mach.h>
include	<pkg/gtools.h>
include <pkg/igsfit.h>

define	HELP	"noao$lib/scr/igsfit.key"
define	PROMPT	"fitcoords surface fitting options"


# IGS_FIT1 -- Fit z = f(x, y)

procedure igs_fit1 (sf, gp, gplog, gt, axis, pts, npts, labels, interactive)

pointer	sf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gplog			# GIO pointer for plot log
pointer	gt			# GTOOLS pointer
int	axis[2]			# Axis definitions
real	pts[npts, ARB]		# Data
int	npts			# Number of pts points
char	labels[SZ_LINE, ARB]	# Identification labels
int	interactive		# Interactive?

extern	igs_solve1()

begin
	call igs_fit (sf, gp, gplog, gt, axis, pts, npts, labels, interactive,
	    igs_solve1)
end


# IGS_FIT2 -- Fit z = x + f(y)

procedure igs_fit2 (sf, gp, gplog, gt, axis, pts, npts, labels, interactive)

pointer	sf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gplog			# GIO pointer for plot log
pointer	gt			# GTOOLS pointer
int	axis[2]			# Axis definitions
real	pts[npts, ARB]		# Data
int	npts			# Number of pts points
char	labels[SZ_LINE, ARB]	# Identification labels
int	interactive		# Interactive?

extern	igs_solve2()

begin
	call igs_fit (sf, gp, gplog, gt, axis, pts, npts, labels, interactive,
	    igs_solve2)
end


# IGS_FIT3 -- Fit z = y + f(x)

procedure igs_fit3 (sf, gp, gplog, gt, axis, pts, npts, labels, interactive)

pointer	sf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gplog			# GIO pointer for plot log
pointer	gt			# GTOOLS pointer
int	axis[2]			# Axis definitions
real	pts[npts, ARB]		# Data
int	npts			# Number of pts points
char	labels[SZ_LINE, ARB]	# Identification labels
int	interactive		# Interactive?

extern	igs_solve3()

begin
	call igs_fit (sf, gp, gplog, gt, axis, pts, npts, labels, interactive,
	    igs_solve3)
end


# IGS_FIT -- Interactive surface fitting.

procedure igs_fit (sf, gp, gplog, gt, axis, pts, npts, labels, interactive, igs_solve)

pointer	sf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gplog			# GIO pointer for plot log
pointer	gt			# GTOOLS pointer
int	axis[2]			# Axis definitions
real	pts[npts, ARB]		# Data
int	npts			# Number of pts points
char	labels[SZ_LINE, ARB]	# Identification labels
int	interactive		# Interactive?
extern	igs_solve()		# Surface solution routine

int	i, newgraph, ztype, dtype, refpt, refpt1
real	zval, zval1
pointer	wts

real	wx, wy
int	wcs, key
char	cmd[SZ_LINE]

int	clgcur(), gt_gcur(), igs_nearest(), igs_nearestd(), igs_nearestu()
errchk	igs_solve

include	"igsfit.com"

begin
	# Compute a solution and set the residuals.

	call igs_solve (sf, pts[1,X], pts[1,Y], pts[1,Z], pts[1,W], npts)
	call xgsvector (sf, pts[1,X], pts[1,Y], pts[1,S], npts)
	call asubr (pts[1,Z], pts[1,S], pts[1,R], npts)
	call aavgr (pts[1,R], npts, mean, rms)
	call igs_params (gt)

	# Return if not interactive.

	ztype = INDEFI
	if ((gp == NULL) || (interactive == NO))
	    goto 30

	call malloc (wts, npts, TY_REAL)
	call amovr (pts[1,W],  Memr[wts], npts)

	call igs_graph (gp, gt, ztype, refpt, axis, pts, npts, labels)
	newgraph = NO

	# Read cursor commands.

10	while (gt_gcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) != EOF) {
	    switch (key) {
	    case '?':
		# Print help text.

		call gpagefile (gp, HELP, PROMPT)

	    case ':':
		# List or set parameters

		if (cmd[1] == '/')
		    call gt_colon (cmd, gp, gt, newgraph)
		else
		    call igs_colon (cmd, gp, sf)

	    # Set abscissa

	    case 'x':
		call printf ("Select abscissa (x, y, z, s, r): ")
		if (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    goto 10
		call printf ("\n")

		switch (key) {
		case 'x':
		    i = X
		case 'y':
		    i = Y
		case 'z':
		    i = Z
		case 's':
		    i = S
		case 'r':
		    i = R
		default:
		    call printf ("\07\n")
		    goto 10
		}

		if (axis[1] != i) {
		    axis[1] = i
		    call gt_setr (gt, GTXMIN, INDEF)
		    call gt_setr (gt, GTXMAX, INDEF)
		}

	    # Set ordinate

	    case 'y':
		call printf ("Select ordinate (x, y, z, s, r): ")
		if(clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    goto 10
		call printf ("\n")

		switch (key) {
		case 'x':
		    i = X
		case 'y':
		    i = Y
		case 'z':
		    i = Z
		case 's':
		    i = S
		case 'r':
		    i = R
		default:
		    call printf ("\07\n")
		    goto 10
		}

		if (axis[2] != i) {
		    axis[2] = i
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		}

	    case 'r':
		newgraph = YES

	    case 'z':
		if (IS_INDEFI (ztype)) {
		    refpt = igs_nearest (gp, ztype, refpt, axis, pts, npts, wx,
			wy, wcs)

		    call printf ("Zoom type (x, y, z): ")
		    if (clgcur ("cursor",wx,wy,wcs,key,cmd,SZ_LINE) == EOF)
			goto 10
		    call printf ("\n")

		    switch (key) {
		    case 'x':
		        ztype = X
		    case 'y':
		        ztype = Y
		    case 'z':
		        ztype = Z
		    default:
		        call printf ("\07\n")
		        goto 10
		    }

		    newgraph = YES
		}

	    case 'p':
		if (!IS_INDEFI (ztype)) {
		    ztype = INDEFI
		    newgraph = YES
		}

	    case 'l':
		if (!IS_INDEFI (ztype)) {
		    refpt1 = 0
		    zval = pts[refpt, ztype]
		    zval1 = -MAX_REAL
		    do i = 1, npts {
			if ((pts[i,ztype] < zval) && (pts[i,ztype] > zval1)) {
			    refpt1 = i
			    zval1 = pts[refpt1,ztype]
			}
		    }

		    if (refpt1 != 0) {
			refpt = refpt1
		        newgraph = YES
		    }
		}

	    case 'n':
		if (!IS_INDEFI (ztype)) {
		    refpt1 = 0
		    zval = pts[refpt, ztype]
		    zval1 = MAX_REAL
		    do i = 1, npts {
			if ((pts[i,ztype] > zval) && (pts[i,ztype] < zval1)) {
			    refpt1 = i
			    zval1 = pts[refpt1,ztype]
			}
		    }

		    if (refpt1 != 0) {
			refpt = refpt1
		        newgraph = YES
		    }
		}

	    case 'c':
		# cursor read
		i = igs_nearest (gp, ztype, refpt, axis, pts, npts, wx, wy, wcs)
		call printf ("%g %g %g %g %g %g\n")
		    call pargr (pts[i, X])
		    call pargr (pts[i, Y])
		    call pargr (pts[i, Z])
		    call pargr (pts[i, W])
		    call pargr (pts[i, S])
		    call pargr (pts[i, R])

	    case 'd':
		i = igs_nearestd (gp, ztype, refpt, axis, pts, npts, wx, wy,
		    wcs)
		if (i == 0)
		    goto 10

	        call gscur (gp, real (pts[i,axis[1]]), real (pts[i,axis[2]]))

		call printf ( "Delete 'p'oint or constant 'x', 'y', or 'z': ")
		if (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    goto 10
		call printf ("\n")

		switch (key) {
		case 'p':
		    dtype = 0
		case 'x':
		    dtype = X
		case 'y':
		    dtype = Y
		case 'z':
		    dtype = Z
		default:
		    call printf ("\07\n")
		    goto 10
		}

		call igs_delete (gp, gt, ztype, i, axis, pts, npts, dtype)

	    case 'u':
		i = igs_nearestu (gp, ztype, refpt, axis, pts, npts, wx, wy,
		    wcs)
		if (i == 0)
		    goto 10

	        call gscur (gp, real (pts[i,axis[1]]), real (pts[i,axis[2]]))

		call printf ( "Undelete 'p'oint or constant 'x', 'y', or 'z': ")
		if (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    goto 10
		call printf ("\n")

		switch (key) {
		case 'p':
		    dtype = 0
		case 'x':
		    dtype = X
		case 'y':
		    dtype = Y
		case 'z':
		    dtype = Z
		default:
		    call printf ("\07\n")
		    goto 10
		}

		call igs_undelete (gp, gt, ztype, i, axis, pts, Memr[wts],
		    npts, dtype)

	    case 'f':
		#call printf ("Fitting ...")
		#call flush (STDOUT)
		call igs_solve (sf,pts[1,X],pts[1,Y],pts[1,Z],pts[1,W],npts)
		call xgsvector (sf, pts[1,X], pts[1,Y], pts[1,S], npts)
		call asubr (pts[1,Z], pts[1,S], pts[1,R], npts)
		call aavgr (pts[1,R], npts, mean, rms)
		call igs_params (gt)
		newgraph = YES

	    case 'w':
		call gt_window (gt, gp, "cursor", newgraph)

	    case 'I':
		call fatal (0, "Interrupt")

	    default:
		# Ring the bell.

		call printf ("\07\n")
	    }

	    if (newgraph == YES) {
		call igs_graph (gp, gt, ztype, refpt, axis, pts, npts, labels)
		newgraph = NO
	    }
	}

	call mfree (wts, TY_REAL)

30	call igs_graph (gplog, gt, ztype, refpt, axis, pts, npts, labels)

end
