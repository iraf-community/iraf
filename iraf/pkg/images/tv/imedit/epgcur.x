include	"epix.h"

# EP_GCUR -- Get EPIX cursor value.
# This is an interface between the standard cursor input and EPIX.  It
# returns an aperture consisting of an aperture type and the two integer
# pixel corners containing the aperture.  This interface also provides
# for interpreting the FIXPIX type files.  A default key may be
# supplied which allows simple X-Y files to be read.

int procedure ep_gcur (ep, ap, x1, y1, x2, y2, key, strval, maxch)

pointer	ep			# EPIX structure
int	ap			# Aperture type
long	x1, y1, x2, y2		# Corners of aperture
int	key			# Keystroke value of cursor event
char	strval[ARB]		# String value, if any
int	maxch

size_t	sz_val
real	a, b, c, d, e
pointer	sp, buf
int	nitems, wcs, ip
int	ctor(), clglstr(), clgcur()
long	labs(), lnint()

begin
	# FIXPIX format consists of a rectangle with column and line ranges.
	# The key returned is for interpolation across the narrow dimension
	# of the rectangle.

	if (EP_FIXPIX(ep) == YES) {
	    call smark (sp)
	    sz_val = SZ_LINE
	    call salloc (buf, sz_val, TY_CHAR)

	    # Read the list structured string.
	    if (clglstr ("cursor", Memc[buf], SZ_LINE) == EOF) {
	        call sfree (sp)
	        return (EOF)
	    }

	    ip = 1
	    nitems = 0
	    if (ctor (Memc[buf], ip, a) > 0)
	        nitems = nitems + 1
	    if (ctor (Memc[buf], ip, b) > 0)
	        nitems = nitems + 1
	    if (ctor (Memc[buf], ip, c) > 0)
	        nitems = nitems + 1
	    if (ctor (Memc[buf], ip, d) > 0)
	        nitems = nitems + 1

	    e = max (a, b)
	    a = min (a, b)
	    b = e
	    e = max (c, d)
	    c = min (c, d)
	    d = e
	    x1 = lnint(a)
	    y1 = lnint(c)
	    x2 = lnint(b)
	    y2 = lnint(d)
	    ap = APRECTANGLE
	    if (x2 - x1 <= y2 - y1)
		key = 'c'
	    else
		key = 'l'

	    call sfree (sp)
	    return (nitems)
	}

	# The standard cursor value is read for centered apertures and
	# for two values are read for rectangular apertures.  The
	# returned coordinates are properly defined.

	key = EP_DEFAULT(ep)
	strval[1] = EOS
	nitems = clgcur ("cursor", a, b, wcs, key, strval, maxch)
	switch (key) {
	case 'a', 'c', 'd', 'l', 'f', 'j', 'v':
	    call printf ("again:")
	    nitems = clgcur ("cursor", c, d, wcs, key, strval, SZ_LINE)
	    call printf ("\n")
	    if (!IS_INDEF(a))
	        x1 = lnint(a)
	    if (!IS_INDEF(b))
	        y1 = lnint(b)
	    if (!IS_INDEF(c))
	        x2 = lnint(c)
	    if (!IS_INDEF(d))
	        y2 = lnint(d)
	    if (key == 'f' || key == 'v') {
	        if (labs(x2-x1) > labs(y2-y1))
	            ap = APLDIAG
	        else
	            ap = APCDIAG
	    } else
	        ap = APRECTANGLE
	case 'b', 'e', 'k', 'm', 'n', 'p', 's', ' ':
	    if (!IS_INDEF(a)) {
	        x1 = lnint(a - EP_RADIUS(ep))
	        x2 = lnint(a + EP_RADIUS(ep))
	    }
	    if (!IS_INDEF(b)) {
	        y1 = lnint(b - EP_RADIUS(ep))
	        y2 = lnint(b + EP_RADIUS(ep))
	    }
	    ap = EP_APERTURE(ep)
	case 'E':
	    call printf ("again:")
	    nitems = clgcur ("cursor", c, d, wcs, key, strval, SZ_LINE)
	    call printf ("\n")
	    if (!IS_INDEF(a))
	        x1 = lnint(a)
	    if (!IS_INDEF(b))
	        y1 = lnint(b)
	    if (!IS_INDEF(c))
	        x2 = lnint(c)
	    if (!IS_INDEF(d))
	        y2 = lnint(d)
	default:
	    if (!IS_INDEF(a))
	        x1 = lnint(a)
	    if (!IS_INDEF(b))
	        y1 = lnint(b)
	}

	return (nitems)
end
