include	<math/nlfit.h>
include	<pkg/inlfit.h>


# ING_UAXES -- Set user axis

procedure ing_uaxesr (keynum, in, nl, x, y, z, npts, nvars)

int	keynum				# Key number for axes
pointer	in				# INLFIT pointer
pointer	nl				# NLFIT pointer
real	x[ARB]				# Independent variable
real	y[npts]				# Dependent variable
real	z[npts]				# Output values
int	npts				# Number of points
int	nvars				# Number of variables

int	npars				# number of parameters
int	uaxes				# user defined procedure
pointer	params				# parameter values
pointer	sp

int	nlstati()
int	in_geti()

begin
	# Check if equation is defined
	uaxes = in_geti (in, INLUAXES)
	if (!IS_INDEFI (uaxes)) {

	    # Get number of parameters, allocate space
	    # for parameter values, and get parameter values
	    npars = nlstati (nl, NLNPARAMS)
	    call smark (sp)
	    call salloc (params, npars, TY_REAL)
	    call nlpgetr (nl, Memr[params], npars)

	    # Call user plot functions
	    call zcall8 (uaxes, keynum, Memr[params], npars,
			 x, y, z, npts, nvars)

	    # Free memory
	    call sfree (sp)

	} else
	    call eprintf ("Warning: User plot function not defined\n")
end
