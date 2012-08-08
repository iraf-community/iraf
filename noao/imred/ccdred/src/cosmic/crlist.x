include	<error.h>
include	<syserr.h>
include	<gset.h>
include	"crlist.h"

define	HELP	"noao$lib/scr/cosmicrays.key"
define	PROMPT	"cosmic ray options"

# CR_OPEN    -- Open cosmic ray list
# CR_CLOSE   -- Close cosmic ray list
# CR_ADD     -- Add a cosmic ray candidate to cosmic ray list.
# CR_TRAIN   -- Set flux ratio threshold from a training set.
# CR_FINDTHRESH -- Find flux ratio.
# CR_WEIGHT  -- Compute the training weight at a particular flux ratio.
# CR_FLAGS   -- Set cosmic ray reject flags.
# CR_BADPIX  -- Store cosmic rays in bad pixel list.
# CR_REPLACE -- Replace cosmic rays in image with replacement values.

# CR_OPEN -- Open cosmic ray list

procedure cr_open (cr)

pointer	cr		# Cosmic ray list pointer
errchk	malloc

begin
	call malloc (cr, CR_LENSTRUCT, TY_STRUCT)
	call malloc (CR_COL(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_LINE(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_FLUX(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_RATIO(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_WT(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_REPLACE(cr), CR_ALLOC, TY_REAL)
	call malloc (CR_FLAG(cr), CR_ALLOC, TY_INT)
	CR_NCR(cr) = 0
	CR_NALLOC(cr) = CR_ALLOC
end


# CR_CLOSE -- Close cosmic ray list

procedure cr_close (cr)

pointer	cr		# Cosmic ray list pointer

begin
	call mfree (CR_COL(cr), TY_REAL)
	call mfree (CR_LINE(cr), TY_REAL)
	call mfree (CR_FLUX(cr), TY_REAL)
	call mfree (CR_RATIO(cr), TY_REAL)
	call mfree (CR_WT(cr), TY_REAL)
	call mfree (CR_REPLACE(cr), TY_REAL)
	call mfree (CR_FLAG(cr), TY_INT)
	call mfree (cr, TY_STRUCT)
end

# CR_ADD -- Add a cosmic ray candidate to cosmic ray list.

procedure cr_add (cr, col, line, flux, ratio, wt, replace, flag)

pointer	cr		# Cosmic ray list pointer
int	col		# Cofluxn
int	line		# Line
real	flux		# Luminosity
real	ratio		# Ratio
real	wt		# Weight
real	replace		# Sky value
int	flag		# Flag value

int	ncr
errchk	realloc

begin
	if (CR_NCR(cr) == CR_NALLOC(cr)) {
	    CR_NALLOC(cr) = CR_NALLOC(cr) + CR_ALLOC
	    call realloc (CR_COL(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_LINE(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_FLUX(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_RATIO(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_WT(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_REPLACE(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_FLAG(cr), CR_NALLOC(cr), TY_INT)
	}

	ncr = CR_NCR(cr)
	CR_NCR(cr) = ncr + 1
	Memr[CR_COL(cr)+ncr] = col
	Memr[CR_LINE(cr)+ncr] = line
	Memr[CR_FLUX(cr)+ncr] = flux
	Memr[CR_RATIO(cr)+ncr] = ratio
	Memr[CR_WT(cr)+ncr] = wt
	Memr[CR_REPLACE(cr)+ncr] = replace
	Memi[CR_FLAG(cr)+ncr] = flag
end


# CR_TRAIN -- Set flux ratio threshold from a training set.

procedure cr_train (cr, gp, gt, im, fluxratio, fname)

pointer	cr		#I Cosmic ray list
pointer	gp		#I GIO pointer
pointer	gt		#I GTOOLS pointer
pointer	im		#I IMIO pointer
real	fluxratio	#O Flux ratio threshold
char	fname[ARB]	#I Save file name

char	cmd[10]
bool	gflag
real	x, y, y1, y2, w, r, rmin
int	i, j, n, f, ncr, wcs, key, fd, clgcur(), open(), errcode()
pointer	col, line, ratio, flux, wt, flag

begin
	# Open save file
	iferr (fd = open (fname, APPEND, TEXT_FILE)) {
	    if (errcode() != SYS_FNOFNAME)
		call erract (EA_WARN)
	    fd = 0
	}

	ncr = CR_NCR(cr)
	col = CR_COL(cr) - 1
	line = CR_LINE(cr) - 1
	flux = CR_FLUX(cr) - 1
	ratio = CR_RATIO(cr) - 1
	wt = CR_WT(cr) - 1
	flag = CR_FLAG(cr) - 1

	gflag = false
	n = 0
	while (clgcur ("objects", x, y, wcs, key, cmd, 10) != EOF) {
	    switch (key) {
	    case '?':
		call gpagefile (gp, HELP, PROMPT)
		next
	    case 'q':
		break
	    case 's':
		w = 1
		f = ALWAYSNO
	    case 'c':
		w = -1
		f = ALWAYSYES
	    case 'g':
		if (gflag)
		    call cr_examine (cr, gp, gt, im, fluxratio, 'z')
		else {
		    if (n > 1)
			call cr_findthresh (cr, fluxratio)
		    call cr_flags (cr, fluxratio)
		    call cr_examine (cr, gp, gt, im, fluxratio, 'r')
		    gflag = true
		}
		next
	    default:
		next
	    }

	    y1 = y - CR_RMAX
	    y2 = y + CR_RMAX
	    for (i=10; i<ncr && y1>Memr[line+i]; i=i+10)
		;
	    j = i - 9
	    rmin = (Memr[col+j] - x) ** 2 + (Memr[line+j] - y) ** 2
	    for (i=j+1; i<ncr && y2>Memr[line+i]; i=i+1) {
		r = (Memr[col+i] - x) ** 2 + (Memr[line+i] - y) ** 2
		if (r < rmin) {
		    rmin = r
		    j = i
		}
	    }
	    if (sqrt (rmin) > CR_RMAX)
		next

	    Memr[wt+j] = w
	    Memi[flag+j] = f
	    n = n + 1

	    if (gflag) {
		if (n > 1) {
		    call cr_findthresh (cr, r)
		    call cr_update (gp, r, cr, fluxratio, 0)
		}
		call gmark (gp, Memr[flux+j], Memr[ratio+j], GM_BOX, 2., 2.)
	    }
	    if (fd > 0) {
		call fprintf (fd, "%g %g %d %c\n")
		    call pargr (x)
		    call pargr (y)
		    call pargi (wcs)
		    call pargi (key)
	    }
	}

	if (fd > 0)
	    call close (fd)
end


# CR_FINDTHRESH -- Find flux ratio.

procedure cr_findthresh (cr, fluxratio)

pointer	cr		#I Cosmic ray list
real	fluxratio	#O Flux ratio threshold

real	w, r, rmin, cr_weight()
int	i, ncr
pointer	ratio, wt

begin
	ncr = CR_NCR(cr)
	ratio = CR_RATIO(cr) - 1
	wt = CR_WT(cr) - 1

	fluxratio = Memr[ratio+1]
	rmin = cr_weight (fluxratio, Memr[ratio+1], Memr[wt+1], ncr)
	do i = 2, ncr {
	    if (Memr[wt+i] == 0.)
		next
	    r = Memr[ratio+i]
	    w = cr_weight (r, Memr[ratio+1], Memr[wt+1], ncr)
	    if (w <= rmin) {
		if (w == rmin)
		    fluxratio = min (fluxratio, r)
		else {
		    rmin = w
		    fluxratio = r
		}
	    }
	}
end


# CR_WEIGHT -- Compute the training weight at a particular flux ratio.

real procedure cr_weight (fluxratio, ratio, wts, ncr)

real	fluxratio		#I Flux ratio
real	ratio[ARB]		#I Ratio Values
real	wts[ARB]		#I Weights
int	ncr			#I Number of ratio values
real	wt			#O Sum of weights

int	i

begin
	wt = 0.
	do i = 1, ncr {
	    if (ratio[i] > fluxratio) {
		if (wts[i] < 0.)
		    wt = wt - wts[i]
	    } else {
		if (wts[i] > 0.)
		    wt = wt + wts[i]
	    }
	}
	return (wt)
end


# CR_FLAGS -- Set cosmic ray reject flags.

procedure cr_flags (cr, fluxratio)

pointer	cr			# Cosmic ray candidate list
real	fluxratio		# Rejection limits

int	i, ncr
pointer	ratio, flag

begin
	ncr = CR_NCR(cr)
	ratio = CR_RATIO(cr) - 1
	flag = CR_FLAG(cr) - 1

	do i = 1, ncr {
	    if ((Memi[flag+i] == ALWAYSYES) || (Memi[flag+i] == ALWAYSNO))
		next
	    if (Memr[ratio+i] > fluxratio)
		Memi[flag+i] = NO
	    else
		Memi[flag+i] = YES
	}
end


# CR_BADPIX -- Store cosmic rays in bad pixel list.
# This is currently a temporary measure until a real bad pixel list is
# implemented.

procedure cr_badpix (cr, fname)

pointer	cr		# Cosmic ray list
char	fname[ARB]	# Bad pixel file name

int	i, ncr, c, l, f, fd, open(), errcode()
pointer	col, line, ratio, flux, flag
errchk	open

begin
	# Open bad pixel file
	iferr (fd = open (fname, APPEND, TEXT_FILE)) {
	    if (errcode() != SYS_FNOFNAME)
		call erract (EA_WARN)
	    return
	}

	ncr = CR_NCR(cr)
	col = CR_COL(cr) - 1
	line = CR_LINE(cr) - 1
	flux = CR_FLUX(cr) - 1
	ratio = CR_RATIO(cr) - 1
	flag = CR_FLAG(cr) - 1

	do i = 1, ncr {
	    f = Memi[flag+i]
	    if ((f == NO) || (f == ALWAYSNO))
		next

	    c = Memr[col+i]
	    l = Memr[line+i]
	    call fprintf (fd, "%d %d\n")
	        call pargi (c)
	        call pargi (l)
	}
	call close (fd)
end


# CR_REPLACE -- Replace cosmic rays in image with replacement values.

procedure cr_replace (cr, offset, im, nreplaced)

pointer	cr		# Cosmic ray list
int	offset		# Offset in list
pointer	im		# IMIO pointer of output image
int	nreplaced	# Number replaced (for log)

int	i, ncr, c, l, f
real	r
pointer	col, line, replace, flag, imps2r()

begin
	ncr = CR_NCR(cr)
	if (ncr <= offset)
	    return

	col = CR_COL(cr) - 1
	line = CR_LINE(cr) - 1
	replace = CR_REPLACE(cr) - 1
	flag = CR_FLAG(cr) - 1

	do i = offset+1, ncr {
	    f = Memi[flag+i]
	    if ((f == NO) || (f == ALWAYSNO))
		next

	    c = Memr[col+i]
	    l = Memr[line+i]
	    r = Memr[replace+i]
	    Memr[imps2r (im, c, c, l, l)] = r
	    nreplaced = nreplaced + 1
	}
end
