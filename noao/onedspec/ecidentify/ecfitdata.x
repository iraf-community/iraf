include	<smw.h>
include	"ecidentify.h"

# EC_FITDATA -- Compute fit coordinates from pixel coordinates.

procedure ec_fitdata (ec)

pointer	ec				# ID pointer

int	i, ecf_oeval()

begin
	call mfree (EC_FITDATA(ec), TY_DOUBLE)
	call malloc (EC_FITDATA(ec), EC_NCOLS(ec)*EC_NLINES(ec), TY_DOUBLE)

	do i = 1, EC_NLINES(ec) {
	    call ec_gline (ec, i)
	    if (EC_ECF(ec) == NULL)
	       call achtrd (Memr[SX(EC_SH(ec))],  FITDATA(ec,1), EC_NPTS(ec))
	    else {
		ORDERS(ec,i) = ecf_oeval (EC_ECF(ec), APS(ec,i))
	        call ecf_vector (EC_ECF(ec), APS(ec,i), PIXDATA(ec,1),
		    FITDATA(ec,1), EC_NPTS(ec))
	    }
	}

	call ec_gline (ec, EC_LINE(ec))
	EC_ORDER(ec) = ORDERS(ec,EC_LINE(ec))
end


# EC_FITFEATURES -- Compute fit coordinates for features.

procedure ec_fitfeatures (ec)

pointer	ec				# ID pointer

int	i, ec_line()
double	ec_fitpt()

begin
	if (EC_NFEATURES(ec) < 1)
	    return

	do i = 1, EC_NFEATURES(ec) {
	    LINE(ec,i) = ec_line (ec, APN(ec,i))
	    ORDER(ec,i) = ORDERS(ec,LINE(ec,i))
	    FIT(ec,i) = ec_fitpt (ec, APN(ec,i), PIX(ec,i))
	}
end


# EC_FITPT -- Compute fit coordinates from pixel coordinates.

double procedure ec_fitpt (ec, order, pix)

pointer	ec			# ID pointer
int	order			# Order
double	pix			# Pixel coordinate

double	fit, ecf_eval(), smw_c1trand(), shdr_lw()

begin
	if (EC_ECF(ec) == NULL) {
	    fit = smw_c1trand (EC_LP(ec), pix)
	    fit = shdr_lw (EC_SH(ec), fit)
	} else
	    fit = ecf_eval (EC_ECF(ec), order, pix)

	return (fit)
end


# EC_FITTOPIX -- Transform fit coordinate to pixel coordinate.

define	DXMIN	.01

double procedure ec_fittopix (ec, fitcoord)

pointer	ec				# ID pointer
double	fitcoord		# Fit coordinate to be transformed
double	pixcoord		# Pixel coordinate returned

int	i, n
double	dx

double	ec_fitpt(), smw_c1trand()

begin
	n = EC_NPTS(ec)
	if (FITDATA(ec,1) < FITDATA(ec,n)) {
	    if ((fitcoord<FITDATA(ec,1)) || (fitcoord>FITDATA(ec,n)))
	        return (INDEFD)

	    for (i = 1; fitcoord > FITDATA(ec,i); i = i + 1)
	        ;

	    if (FITDATA(ec,i) == fitcoord)
	        return (double (i))

	    pixcoord = smw_c1trand (EC_LP(ec), double(i-.5))
	    dx = smw_c1trand (EC_LP(ec), double(i+.5)) - pixcoord
	    while (dx > DXMIN) {
	        dx = dx / 2
	        if (ec_fitpt (ec, EC_AP(ec), pixcoord) < fitcoord)
		    pixcoord = pixcoord + dx
	        else
		    pixcoord = pixcoord - dx
	    }
	} else {
	    if ((fitcoord<FITDATA(ec,n)) || (fitcoord>FITDATA(ec,1)))
	        return (INDEFD)

	    for (i = 1; fitcoord < FITDATA(ec,i); i = i + 1)
	        ;

	    if (FITDATA(ec,i) == fitcoord)
	        return (double (i))

	    pixcoord = smw_c1trand (EC_LP(ec), double(i-.5))
	    dx = smw_c1trand (EC_LP(ec), double(i+.5)) - pixcoord
	    while (dx > DXMIN) {
	        dx = dx / 2
	        if (ec_fitpt (ec, EC_AP(ec), pixcoord) < fitcoord)
		    pixcoord = pixcoord - dx
	        else
		    pixcoord = pixcoord + dx
	    }
	}

	return (pixcoord)
end
