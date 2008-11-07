include	<math/nlfit.h>
include	<pkg/inlfit.h>


# ING_UAXES -- Set user axis

procedure ing_uaxesd (keynum, in, nl, x, y, z, npts, nvars)

int	keynum				# Key number for axes
pointer	in				# INLFIT pointer
pointer	nl				# NLFIT pointer
double	x[ARB]				# Independent variable
double	y[npts]				# Dependent variable
double	z[npts]				# Output values
size_t	npts				# Number of points
int	nvars				# Number of variables

size_t	sz_val
size_t	npars				# number of parameters
pointer	uaxes				# user defined procedure
pointer	params				# parameter values
pointer	sp

long	nlstatl()
pointer	in_getp()

begin
	# Check if equation is defined
	uaxes = in_getp (in, INLUAXES)
	if ( uaxes != NULL ) {

	    # Get number of parameters, allocate space
	    # for parameter values, and get parameter values
	    npars = nlstatl (nl, NLNPARAMS)
	    call smark (sp)
	    sz_val = npars
	    call salloc (params, sz_val, TY_DOUBLE)
	    call nlpgetd (nl, Memd[params], npars)

	    # Call user plot functions
	    call zcall8 (uaxes, keynum, Memd[params], npars,
			 x, y, z, npts, nvars)

	    # Free memory
	    call sfree (sp)

	} else
	    call eprintf ("Warning: User plot function not defined\n")
end
