include	<error.h>
include	<math.h>


# T_GRATINGS -- Compute grating parameters.
# Given a subset of grating parameters the remainder are computed.

procedure t_gratings()

bool	e			# Echelle grating?
real	f			# Focal length (mm)
real	g			# Grating grooves per mm
real	b			# Blaze angle (degrees)
real	t			# Angle of incidence (degrees)
int	m			# Order
real	w			# Blaze wavelength (Angstroms)
real	d			# Blaze dispersion (Angstroms / mm)

bool	clgetb()
int	clgeti()
real	clgetr()

begin
	# Input grating parameters.
	e = clgetb ("echelle")
	f = clgetr ("f")
	g = clgetr ("gmm")
	b = clgetr ("blaze")
	t = clgetr ("theta")
	m = clgeti ("order")
	w = clgetr ("wavelength")
	d = clgetr ("dispersion")

	# Derive and check grating parameters.
	iferr (call ast_grating (e, f, g, b, t, m, w, d))
	    call erract (EA_WARN)

	# Print final parameters.
	call printf ("Grating parameters:\n")
	call printf ("  Focal length = %g mm\n")
	    call pargr (f)
	call printf ("  Grating = %g grooves/mm\n")
	    call pargr (g)
	call printf ("  Blaze angle = %g degrees\n")
	    call pargr (b)
	call printf ("  Incidence angle = %g degrees\n")
	    call pargr (t)
	call printf ("  Order = %d\n")
	    call pargi (m)
	call printf ("  Blaze wavelength = %g Angstroms\n")
	    call pargr (w)
	call printf ("  Blaze dispersion = %g Angstroms/mm\n")
	    call pargr (d)
end


# Definitions of INDEF parameter flags.
define	F	1B
define	G	2B
define	B	4B
define	T	10B
define	M	20B
define	W	40B
define	D	100B

# Combinations
define	FG	3B
define	FB	5B
define	FT	11B
define	FM	21B
define	FW	41B
define	GB	6B
define	GT	12B
define	GW	42B
define	GD	102B
define	BT	14B
define	BM	24B
define	BW	44B
define	BD	104B
define	TM	30B
define	TW	50B
define	TD	110B
define	MW	60B
define	MD	120B
define	WD	140B


# AST_GRATING -- Derive and check grating parameters.

procedure ast_grating (e, f, g, b, t, m, w, d)

bool	e
real	f,  g, b, t, w, d, x
int	m

int	i, flags
define	err_	10

begin
	if (!IS_INDEF(f)) {
	    if (f <= 0.)
		f = INDEF
	}
	if (!IS_INDEF(g)) {
	    if (g <= 0.)
		g = INDEF
	    else
		g = g / 1e7
	}
	if (!IS_INDEF(b)) {
	    b = DEGTORAD (b)
	    if (b == 0. && t == 0.)
		t = INDEF
	}
	if (!IS_INDEF(t)) {
	    t = DEGTORAD (t)
	    if (t > PI && !IS_INDEF(b))
		t = t - TWOPI + b
	}
	if (!IS_INDEFI(m) && m <= 0)
	    m = INDEFI
	if (!IS_INDEF(w) && w <= 0.)
	    w = INDEF
	if (!IS_INDEF(d) && d <= 0.)
	    d = INDEF

	flags = 0
	if (IS_INDEF(f))
	    flags = flags + F
	if (IS_INDEF(g))
	    flags = flags + G
	if (IS_INDEF(b))
	    flags = flags + B
	if (IS_INDEF(t))
	    flags = flags + T
	if (IS_INDEFI(m))
	    flags = flags + M
	if (IS_INDEF(w))
	    flags = flags + W
	if (IS_INDEF(d))
	    flags = flags + D

	switch (flags) {
	case 0, F, G, B, T, M, W, D:
	    switch (flags) {
	    case F:
		f = cos (2 * b - t) / (g * m * d)
	    case G: 
		g = (sin (t) + sin (2 * b - t)) / (m * w)
		if (g == 0.)
		    g = INDEF
	    case B:
		if (t > PI) {
		    x = g * m * w / (2 * cos (t))
		    if (abs (x) > 1.)
			goto err_
		    b = asin (x)
		    t = t - TWOPI + b
		} else {
		    x = g * m * w - sin (t)
		    if (abs (x) > 1.)
			goto err_
		    b = (t + asin (x)) / 2
		}
	    case T:
		x = g * m * w / (2 * sin(b))
		if (abs (x) > 1.)
		    goto err_
		if (e)
		    t = b + acos (x)
		else
		    t = b - acos (x)
	    case M:
		m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    }
	    if (!IS_INDEF(g)) {
		w = (sin (t) + sin (2 * b - t)) / (g * m)
		d = cos (2 * b - t) / (f * g * m)
	    }
	case FG:
	    x = (sin (t) + sin (2 * b - t)) / (m * w)
	    if (x == 0.)
		goto err_
	    g = x
	    f = cos (2 * b - t) / (g * m * d)
	case FB:
	    if (t > PI) {
		x = g * m * w / (2 * cos (t))
		if (abs (x) > 1.)
		    goto err_
		b = asin (x)
		t = t - TWOPI + b
	    } else {
		x = g * m * w - sin (t)
		if (abs (x) > 1.)
		    goto err_
		b = (t + asin (x)) / 2
	    }
	    f = cos (2 * b - t) / (g * m * d)
	case FT:
	    x = g * m * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    f = cos (2 * b - t) / (g * m * d)
	case FM:
	    m = nint ((sin (t) + sin (2 * b - t)) / (g * w))
	    f = cos (2 * b - t) / (g * m * d)
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case FW:
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    f = cos (2 * b - t) / (g * m * d)
	case GB:
	    x = f * d / w
	    if (t > PI) {
		b = atan (1 / (2 * x - tan (t)))
		t = t - TWOPI + b
	    } else {
		x = (tan (t) - x) / (1 + 2 * x * tan (t))
		b = atan (x + sqrt (1 + x * x))
	    }
	    g = (sin (t) + sin (2 * b - t)) / (m * w)
	case GT:
	    t = b + atan (2 * f * d / w - 1 / tan (b))
	    g = (sin (t) + sin (2 * b - t)) / (m * w)
	case GW:
	    g = cos (2 * b - t) / (f * m * d)
	    if (g == 0.)
		g = INDEF
	    else
		w = (sin (t) + sin (2 * b - t)) / (g * m)
	case GD:
	    x = (sin (t) + sin (2 * b - t)) / (m * w)
	    if (x == 0.)
		goto err_
	    g = x
	    d = cos (2 * b - t) / (f * g * m)
	case BT:
	    x = f * g * m * d
	    if (abs (x) > 1.)
		goto err_
	    x = acos (x)
	    x = g * m * w - sin (x)
	    if (abs (x) > 1.)
		goto err_
	    t = asin (x)
	    b = (acos (f * g * m * d) + t) / 2
	case BM:
	    x = f * d / w
	    if (t > PI) {
		b = atan (1 / (2 * x - tan (t)))
		t = t - TWOPI + b
	    } else {
		x = (tan (t) - x) / (1 + 2 * x * tan (t))
		b = atan (x + sqrt (1 + x * x))
	    }
	    m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    b = (t + asin (g * m * w - sin (t))) / 2
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case BW:
	    b = (t + acos (f * g * m * d)) / 2
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	case BD:
	    if (t > PI) {
		x = g * m * w / (2 * cos (t))
		if (abs (x) > 1.)
		    goto err_
		b = asin (x)
		t = t - TWOPI + b
	    } else {
		x = g * m * w - sin (t)
		if (abs (x) > 1.)
		    goto err_
		b = (t + asin (x)) / 2
	    }
	    d = cos (2 * b - t) / (f * g * m)
	case TM:
	    x = f * d / w
	    x = b + 2 * atan (x - 1 / (2 * tan (b)))
	    i = max (1, nint ((sin(x) + sin(2*b-x)) / (g * w)))
	    x = g * i * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    m = i
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case TW:
	    x = f * g * m * d
	    if (abs (x) > 1.)
		goto err_
	    t = 2 * b - acos (x)
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	case TD:
	    x = g * m * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    d = cos (2 * b - t) / (f * g * m)
	case MW:
	    m = max (1, nint (cos (2 * b - t) / (f * g * d)))
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case MD:
	    m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case WD:
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	}

	if (!IS_INDEF(g))
	    g = g * 1e7
	if (!IS_INDEF(b))
	    b = RADTODEG (b)
	if (!IS_INDEF(t))
	    t = RADTODEG (t)
			   
	if (IS_INDEF(f) || IS_INDEF(g) || IS_INDEF(b) || IS_INDEF(t) ||
	    IS_INDEF(m) || IS_INDEF(w) || IS_INDEF(d))
	    call error (1,
	        "Insufficient information to to determine grating parameters")

	return

err_	if (!IS_INDEF(g))
	    g = g * 1e7
	if (!IS_INDEF(b))
	    b = RADTODEG (b)
	if (!IS_INDEF(t))
	    t = RADTODEG (t)
	call error (2, "Impossible combination of grating parameters")
end
