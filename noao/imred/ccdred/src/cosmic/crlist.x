include	<error.h>
include	<syserr.h>
include	"crlist.h"

# CR_OPEN    -- Open cosmic ray list
# CR_CLOSE   -- Close cosmic ray list
# CR_ADD     -- Add a cosmic ray candidate to cosmic ray list.
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
	call mfree (CR_REPLACE(cr), TY_REAL)
	call mfree (CR_FLAG(cr), TY_INT)
	call mfree (cr, TY_STRUCT)
end

# CR_ADD -- Add a cosmic ray candidate to cosmic ray list.

procedure cr_add (cr, col, line, flux, ratio, replace)

pointer	cr		# Cosmic ray list pointer
int	col		# Cofluxn
int	line		# Line
real	flux		# Luminosity
real	ratio		# Luminosity / pixel
real	replace		# Sky value

int	ncr
errchk	realloc

begin
	if (CR_NCR(cr) == CR_NALLOC(cr)) {
	    CR_NALLOC(cr) = CR_NALLOC(cr) + CR_ALLOC
	    call realloc (CR_COL(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_LINE(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_FLUX(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_RATIO(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_REPLACE(cr), CR_NALLOC(cr), TY_REAL)
	    call realloc (CR_FLAG(cr), CR_NALLOC(cr), TY_REAL)
	}

	ncr = CR_NCR(cr)
	CR_NCR(cr) = ncr + 1
	Memr[CR_COL(cr)+ncr] = col
	Memr[CR_LINE(cr)+ncr] = line
	Memr[CR_FLUX(cr)+ncr] = flux
	Memr[CR_RATIO(cr)+ncr] = ratio
	Memr[CR_REPLACE(cr)+ncr] = replace
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
pointer	col, line, flag
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
