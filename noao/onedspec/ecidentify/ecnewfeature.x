include	<mach.h>
include	<smw.h>
include	"ecidentify.h"

# EC_NEWFEATURE -- Allocate and initialize memory for a new feature.

procedure ec_newfeature (ec, ap, pix, fit, user, width, type)

pointer	ec			# ID pointer
int	ap			# Order
double	pix			# Pixel coordinate
double	fit			# Fit coordinate
double	user			# User coordinate
real	width			# Feature width
int	type			# Feature type

int	i, j, ec_line()
double	delta

define	NALLOC	20		# Length of additional allocations

begin
	if (IS_INDEFD (pix))
	    return

	delta = MAX_REAL
	do i = 1, EC_NFEATURES(ec) {
	    if (APN(ec,i) != ap)
		next
	    if (abs (pix - PIX(ec,i)) < delta) {
		delta = abs (pix - PIX(ec,i))
		j = i
	    }
	}

	if (delta >= EC_MINSEP(ec)) {
	    EC_NFEATURES(ec) = EC_NFEATURES(ec) + 1
	    if (EC_NALLOC(ec) < EC_NFEATURES(ec)) {
	        EC_NALLOC(ec) = EC_NALLOC(ec) + NALLOC
	        call realloc (EC_APNUM(ec), EC_NALLOC(ec), TY_INT)
	        call realloc (EC_LINENUM(ec), EC_NALLOC(ec), TY_INT)
	        call realloc (EC_ORD(ec), EC_NALLOC(ec), TY_INT)
	        call realloc (EC_PIX(ec), EC_NALLOC(ec), TY_DOUBLE)
	        call realloc (EC_FIT(ec), EC_NALLOC(ec), TY_DOUBLE)
	        call realloc (EC_USER(ec), EC_NALLOC(ec), TY_DOUBLE)
	        call realloc (EC_FWIDTHS(ec), EC_NALLOC(ec), TY_REAL)
	        call realloc (EC_FTYPES(ec), EC_NALLOC(ec), TY_INT)
	    }
	    for (j=EC_NFEATURES(ec); (j>1)&&(ap<APN(ec,j-1)); j=j-1) {
		APN(ec,j) = APN(ec,j-1)
		LINE(ec,j) = LINE(ec,j-1)
	        ORDER(ec,j) = ORDER(ec,j-1)
	        PIX(ec,j) = PIX(ec,j-1)
	        FIT(ec,j) = FIT(ec,j-1)
	        USER(ec,j) = USER(ec,j-1)
	        FWIDTH(ec,j) = FWIDTH(ec,j-1)
	        FTYPE(ec,j) = FTYPE(ec,j-1)
	    }
	    for (; (j>1)&&(ap==APN(ec,j-1))&&(pix<PIX(ec,j-1)); j=j-1) {
		APN(ec,j) = APN(ec,j-1)
		LINE(ec,j) = LINE(ec,j-1)
	        ORDER(ec,j) = ORDER(ec,j-1)
	        PIX(ec,j) = PIX(ec,j-1)
	        FIT(ec,j) = FIT(ec,j-1)
	        USER(ec,j) = USER(ec,j-1)
	        FWIDTH(ec,j) = FWIDTH(ec,j-1)
	        FTYPE(ec,j) = FTYPE(ec,j-1)
	    }
	    APN(ec,j) = ap
	    LINE(ec,j) = ec_line (ec, ap)
	    ORDER(ec,j) = ORDERS(ec,LINE(ec,j))
	    PIX(ec,j) = pix
	    FIT(ec,j) = fit
	    USER(ec,j) = user
	    FWIDTH(ec,j) = width
	    FTYPE(ec,j) = type
	    EC_NEWFEATURES(ec) = YES
	} else if (abs (fit-user) < abs (FIT(ec,j)-USER(ec,j))) {
	    APN(ec,j) = ap
	    LINE(ec,j) = ec_line (ec, ap)
	    ORDER(ec,j) = ORDERS(ec,LINE(ec,j))
	    PIX(ec,j) = pix
	    FIT(ec,j) = fit
	    USER(ec,j) = user
	    FWIDTH(ec,j) = width
	    FTYPE(ec,j) = type
	    EC_NEWFEATURES(ec) = YES
	}

	EC_CURRENT(ec) = j
end
