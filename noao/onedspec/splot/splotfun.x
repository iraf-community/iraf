include	<mach.h>
include "../oned.h"
include "../idsmtn.h"

# Function Mode for STEK

# FUN_DO -- Branch and execute proper function

procedure fun_do (key, pix, npts)

real	pix[ARB]
int	key, npts

begin
	switch (key) {
	case 'l':
	    # Log10
	    call fun_l10 (pix, npts)

	case 'n':
	    # Log base e
	    call fun_le  (pix, npts)

	case 'd':
	    # Dexp (base 10)
	    call fun_dxp (pix, npts)

	case 'e':
	    # Exp base e
	    call fun_exp (pix, npts)

	case 's':
	    # Square root
	    call fun_sqr (pix, npts)

	case 'a':
	    # Absolute value
	    call fun_abs (pix, npts)

	case 'i':
	    # Inverse
	    call fun_invers (pix, npts)

	# Constant operators
	case 'p':
	    call fun_plus (pix, npts)

	case 'm':
	    call fun_mult (pix, npts)

	# Two spectrum operators
	case '+':
	    call fun_splus (pix, npts)

	case '-':
	    call fun_sminus (pix, npts)

	case '*':
	    call fun_smult (pix, npts)

	case '/':
	    call fun_sdiv (pix, npts)

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

# FUN_L10 -- Log 10 of spectrum.
# if value is less than or equal to 0.0, log10 (value) = -0.5

procedure fun_l10 (pix, n)

real	pix[ARB]
int	n

int	i

begin
	do i = 1, n
	    if (pix[i] > 0.0)
		pix[i] = log10 (pix[i])
	    else
		pix[i] = -0.5
end

# FUN_LE -- Natural log (base e)

procedure fun_le (pix, n)

real	pix[ARB]
int	n

int	i

begin
	do i = 1, n
	    if (pix[i] > 0.0)
		pix[i] = log (pix[i])
	    else
		pix[i] = -0.5
end

# FUN_EXP -- Exponential base e

procedure fun_exp (pix, n)

real	pix[ARB]
int	n

int	i
real	max_exp

begin
	max_exp = log (MAX_REAL)
	do i = 1, n
	    if (abs (pix[i]) < max_exp)
		pix[i] = exp (pix[i])
	    else if (pix[i] >= max_exp)
		pix[i] = MAX_REAL
	    else
		pix[i] = 0.0
end

# FUN_DXP -- Exponential base 10

procedure fun_dxp (pix, n)

real	pix[ARB]
int	n

int	i
real	max_exp

begin
	max_exp = log10 (MAX_REAL)
	do i = 1, n
	    if (abs (pix[i]) < max_exp)
		pix[i] = 10.0 ** pix[i]
	    else if (pix[i] >= max_exp)
		pix[i] = MAX_REAL
	    else
		pix[i] = 0.0
end

# FUN_SQR -- Sqrt of value. If negative, set to 0.0

procedure fun_sqr (pix, n)

real	pix[ARB]
int	n

int	i

begin
	do i = 1, n
	    if (pix[i] >= 0.0)
		pix[i] = sqrt (pix[i])
	    else
		pix[i] = 0.0
end

# FUN_ARB -- Absolute value

procedure fun_abs (pix, n)

real	pix[ARB]
int	n

int	i

begin
	do i = 1, n
	    pix[i] = abs (pix[i])
end

# FUN_INVERS -- Inverse

procedure fun_invers (pix, n)

real	pix[ARB]
int	n

int	i

begin
	do i = 1, n
	    if (pix[i] != 0.0)
		pix[i] = 1.0/pix[i]
	    else
		pix[i] = 0.0
end

# FUN_PLUS -- Add a constant to spectrum
# Will someday be able to add another spectrum

procedure fun_plus (pix, n)

real	pix[ARB]
int	n

real	const

real	clgetr()

begin
	# Get constant
	const = clgetr ("constant")

	call aaddkr (pix, const, pix, n)
end

# FUN_MULT -- Multiply spectrum by a constant

procedure fun_mult (pix, n)

real	pix[ARB]
int	n

real	const

real	clgetr()

begin
	# Get constant
	const = clgetr ("constant")

	call amulkr (pix, const, pix, n)
end

# FUN_SPLUS -- Add spectrum to a spectrum

procedure fun_splus (pix, n)

real	pix[ARB]
int	n

int	npt2, nval, nline
real	x1, x2, dx
pointer	im, pix2, ids2, sp
char	spec2[SZ_FNAME]
bool	wave_scl

int	strlen()
errchk	getimage

begin
	# Get second spectrum
	call printf ("add to ")
	call clgstr ("spec2", spec2, SZ_FNAME)
	if (strlen (spec2) == 0)
	    return

	call smark (sp)
	call salloc (ids2, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids2), MAX_NCOEFF, TY_REAL)

	wave_scl = true
	call getimage (spec2, nline, wave_scl, im, ids2, NULL, pix2, npt2,
	    x1, x2, dx)
	nval = min (n, npt2)
	call aaddr (pix, Memr[pix2], pix, nval)
	call imunmap (im)

	call sfree (sp)
end

# FUN_SMULT -- Multiply spectrum by a spectrum

procedure fun_smult (pix, n)

real	pix[ARB]
int	n

int	npt2, nval, nline
real	x1, x2, dx
pointer	im, pix2, ids2, sp
char	spec2[SZ_FNAME]
bool	wave_scl

int	strlen()
errchk	getimage

begin
	# Get second spectrum
	call printf ("multiply by ")
	call clgstr ("spec2", spec2, SZ_FNAME)
	if (strlen (spec2) == 0)
	    return

	call smark (sp)
	call salloc (ids2, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids2), MAX_NCOEFF, TY_REAL)

	wave_scl = true
	call getimage (spec2, nline, wave_scl, im, ids2, NULL, pix2, npt2,
	    x1, x2, dx)

	nval = min (n, npt2)
	call amulr (pix, Memr[pix2], pix, nval)
	call imunmap (im)

	call sfree (sp)
end

# FUN_SMINUS -- Subtract spectrum from a spectrum

procedure fun_sminus (pix, n)

real	pix[ARB]
int	n

int	npt2, nval, nline
real	x1, x2, dx
pointer	im, pix2, ids2, sp
char	spec2[SZ_FNAME]
bool	wave_scl

int	strlen()
errchk	getimage

begin
	# Get second spectrum
	call printf ("subtract ")
	call clgstr ("spec2", spec2, SZ_FNAME)
	if (strlen (spec2) == 0)
	    return

	call smark (sp)
	call salloc (ids2, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids2), MAX_NCOEFF, TY_REAL)

	wave_scl = true
	call getimage (spec2, nline, wave_scl, im, ids2, NULL, pix2, npt2,
	    x1, x2, dx)
	
	nval = min (n, npt2)
	call asubr (pix, Memr[pix2], pix, nval)
	call imunmap (im)

	call sfree (sp)
end

# FUN_SDIV -- Divide spectrum by a spectrum

procedure fun_sdiv (pix, n)

real	pix[ARB]
int	n

int	npt2, nval, i, nline
real	x1, x2, dx
pointer	im, pix2, ids2, sp
char	spec2[SZ_FNAME]
bool	wave_scl

int	strlen()
errchk	getimage

begin
	# Get second spectrum
	call printf ("divide by ")
	call clgstr ("spec2", spec2, SZ_FNAME)
	if (strlen (spec2) == 0)
	    return

	call smark (sp)
	call salloc (ids2, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids2), MAX_NCOEFF, TY_REAL)

	wave_scl = true
	call getimage (spec2, nline, wave_scl, im, ids2, NULL, pix2, npt2,
	    x1, x2, dx)
	
	nval = min (n, npt2)
	do i = 1, nval
	    if (Memr[pix2+i-1] == 0.0)
		pix[i] = 0.0
	    else
		pix[i] = pix[i] / Memr[pix2+i-1]

	call imunmap (im)

	call sfree (sp)
end
