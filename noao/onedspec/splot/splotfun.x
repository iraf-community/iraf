include <error.h>
include	<mach.h>
include	<smw.h>

# Function Mode for STEK

# FUN_DO -- Branch and execute proper function

procedure fun_do (key, sh1, y, n, w0, wpc)

int	key
pointer	sh1
real	y[n]
int	n
double	w0, wpc

char	spec2[SZ_FNAME]
int	i, nline, nband, nap, strlen()
real	const, clgetr()
pointer	im, mw, sh2
bool	wave_scl
errchk	getimage, shdr_rebin

begin
	switch (key) {
	case 'a': # Absolute value
	    do i = 1, n
		y[i] = abs (y[i])
	case 'd': # Dexp (base 10)
	    const = log10 (MAX_REAL)
	    do i = 1, n
		if (abs (y[i]) < const)
		    y[i] = 10.0 ** y[i]
		else if (y[i] >= const)
		    y[i] = MAX_REAL
		else
		    y[i] = 0.0
	case 'e': # Exp base e
	    const = log (MAX_REAL)
	    do i = 1, n
		if (abs (y[i]) < const)
		    y[i] = exp (y[i])
		else if (y[i] >= const)
		    y[i] = MAX_REAL
		else
		    y[i] = 0.0
	case 'i': # Inverse
	    do i = 1, n
		if (y[i] != 0.0)
		    y[i] = 1.0/y[i]
		else
		    y[i] = 0.0
	case 'l': # Log10
	    do i = 1, n
		if (y[i] > 0.0)
		    y[i] = log10 (y[i])
		else
		    y[i] = -0.5
	case 'm': # Multiply by constant
	    const = clgetr ("constant")
	    call amulkr (y, const, y, n)
	case 'n': # Log base e
	    do i = 1, n
		if (y[i] > 0.0)
		    y[i] = log (y[i])
		else
		    y[i] = -0.5
	case 'p': # Add constant
	    const = clgetr ("constant")
	    call aaddkr (y, const, y, n)
	case 's': # Square root
	    do i = 1, n
		if (y[i] >= 0.0)
		    y[i] = sqrt (y[i])
		else
		    y[i] = 0.0

	case '+', '-', '*', '/': # Binary operations
	    call printf ("Second spectrum ")
	    call clgstr ("spec2", spec2, SZ_FNAME)
	    if (strlen (spec2) == 0)
		return

	    wave_scl = true
	    nline = 0
	    nband = 0
	    nap = 0
	    im = NULL
	    mw = NULL
	    sh2 = NULL
	    call getimage (spec2, nline, nband, nap, wave_scl, w0, wpc,
		"angstroms", im, mw, sh2, NULL)
	    call shdr_rebin (sh2, sh1)
	    switch (key) {
	    case '+':
		call aaddr (y, Memr[SY(sh2)], y, n)
	    case '-':
		call asubr (y, Memr[SY(sh2)], y, n)
	    case '*':
		call amulr (y, Memr[SY(sh2)], y, n)
	    case '/':
		do i = 1, n
		    if (Memr[SY(sh2)+i-1] == 0.0)
			y[i] = 0.0
		else
		    y[i] = y[i] / Memr[SY(sh2)+i-1]
	    }
	    call shdr_close (sh2)
	    call smw_close (mw)
	    call imunmap (im)

	# Redraw
	case 'r':
	    ;
	default:
	    call error (0, "Unknown function")
	}
end

# FUN_HELP

procedure fun_help ()

begin
	call printf ("q=quit l,n=log10,e d,e=d,exp s=sqrt a=abs i=1/s")
	call printf (" p=+k m=*k  +,-,*,/=2spec ops\n")
end
