include	"../shdr.h"

# USER_COORD -- Compute user coordinate from two marked lines or CL parameters
#               Assumes a linear dispersion

procedure user_coord (sh, wave_scl, x, n, w1)

pointer	sh
bool	wave_scl
real	x[n]
int	n
real	w1

int	i, wc, key
real	w2, u1, u2, xjunk, x1, x2, dx
char	command[SZ_FNAME]

int	clgcur()
real	clgetr()
errchk	un_ctranr

begin
	x1 = x[1]
	x2 = x[n]
	dx = (x2 - x1) / (n - 1)

	if (IS_INDEF(w1)) {
	    call clputr ("wstart", x1)
	    call clputr ("wend", x2)
	    call clputr ("dw", dx)

	    x1 = clgetr ("wstart")
	    x2 = clgetr ("wend")
	    if (IS_INDEF(x1)) {
		dx = clgetr ("dw")
		x1 = x2 - (n-1) * dx
	    } else if (IS_INDEF(x2)) {
		dx = clgetr ("dw")
		x2 = x1 + (n-1) * dx
	    } else
		dx = (x2-x1) / (n-1)
	} else {
	    # Get user coord for  first point
	    call clputr ("wavelength", w1)
	    if (wave_scl)
    #	    w1 = (w1 - W0(sh)) / WP(sh) + 1
		w1 = (w1 - x1) / dx + 1
	    call printf ("pixel 1:%7.2f ")
		call pargr (w1)
	    call flush (STDOUT)
	    u1 = clgetr ("wavelength")

	    # Get second point
	    call printf ("u again")
	    call flush (STDOUT)
	    i = clgcur ("cursor", w2, xjunk, wc, key, command, SZ_FNAME)
	    call clputr ("wavelength", w2)
	    if ((wave_scl) && !IS_INDEF (W0(sh)) && !IS_INDEF (WP(sh)))
    #	    w2 = (w2 - W0(sh)) / WP(sh) + 1
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
	    x2 = x1 + (n-1) * dx
	}

	iferr {
	    call un_ctranr (UN(sh), MWUN(sh), x1, W0(sh), 1)
	    call un_ctranr (UN(sh), MWUN(sh), x2, W1(sh), 1)
	} then
	    ;
	WP(sh) = (W1(sh) - W0(sh)) / n
	DC(sh) = DCLINEAR
	CTLW(sh) = NULL
	CTWL(sh) = NULL
	wave_scl = true

	do i = 1, n
	    x[i] = x1 + (i - 1) * dx
end
