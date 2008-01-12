include	"hdicfit.h"

# ICG_UAXIS -- Set user axis.

procedure icg_uaxesd (ic, key, cv, x, y, z, npts, label, units, maxchars)

pointer	ic				# Pointer to ic structure
int	key				# Key for axes
pointer	cv				# CURFIT pointer
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
double	z[npts]				# Output values
int	npts				# Number of points
char	label[maxchars]			# Axis label
char	units[maxchars]			# Units for axis
int	maxchars			# Maximum chars in label

int	offset
double	fog
real	ic_getr()
include	"hdic.com"

begin
	# Axis type 'u' returns the untransformed independent variable
	# in the z array.  That is, the original density values after
	# subtracting the current fog value.  Some density values could be
	# below fog were excluded from the transformed vector. 

	call strcpy ("Density above fog", label, maxchars)
	fog = double (ic_getr (ic, "fog"))

	if (npts == nraw)
	    call asubkd (Memd[den], fog, z, npts)
	else {
	    offset = big_den + (NVALS_FIT - npts)
	    call asubkd (Memd[offset], fog, z, npts)
	}
end
