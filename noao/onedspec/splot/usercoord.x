include	"../idsmtn.h"

# USER_COORD -- Compute user coordinate from two marked lines
#               Assumes a linear dispersion

procedure user_coord (ids, wave_scl, npts, w1, x1, x2, dx)

pointer	ids
bool	wave_scl
int	npts
real	w1, x1, x2, dx

int	junk, key
real	w2, u1, u2, xjunk, wc
char	command[SZ_FNAME]

int	clgcur()
real	clgetr()

begin
	# Get user coord for  first point
	call clputr ("wavelength", w1)
	if (wave_scl)
#	    w1 = (w1 - W0(ids)) / WPC(ids) + 1
	    w1 = (w1 - x1) / dx + 1
	call printf ("pixel 1:%7.2f ")
	    call pargr (w1)
	call flush (STDOUT)
	u1 = clgetr ("wavelength")

	# Get second point
	call printf ("u again")
	call flush (STDOUT)
	junk = clgcur ("cursor", w2, xjunk, wc, key, command, SZ_FNAME)
	call clputr ("wavelength", w2)
	if ((wave_scl) && !IS_INDEF (W0(ids)) && !IS_INDEF (WPC(ids)))
#	    w2 = (w2 - W0(ids)) / WPC(ids) + 1
	    w2 = (w2 - x1) / dx + 1

	if (w2 == w1) {
	    call printf ("cursor was not moved")
	    call flush (STDOUT)
	    return
	}

	call printf ("pixel 2:%7.2f ")
	    call pargr (w2)
	call flush (STDOUT)
	u2 = clgetr ("wavelength")

	# Compute dispersion and starting wavelength
	dx = (u2 - u1) / (w2 - w1)
	x1 = u1 - (w1-1) * dx
	x2 = x1 + (npts-1) * dx
	W0(ids) = x1
	WPC(ids) = dx
	DC_FLAG(ids) = 0
	wave_scl = true
end
