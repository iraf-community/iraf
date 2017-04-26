include <pkg/inlfit.h>


# ING_VSHOW -- Show fit parameters in verbose mode on the screen.

procedure ing_vshowd (in, file, nl, x, y, wts, names, npts, nvars, len_name,
	gt)

pointer	in		        # INLFIT pointer
char	file[ARB]		# Output file name
pointer	nl			# NLFIT pointer
double	x[ARB]			# Ordinates (npts * nvars)
double	y[ARB]			# Abscissas
double	wts[ARB]		# Weights
char	names[ARB]		# Object ids
int	npts			# Number of data points
int	nvars			# Number of variables
int	len_name		# Length of id name
pointer	gt			# Graphics tools pointer

begin
	# Print the title.
	call ing_title (in, file, gt)

	# Do the standard ing_show option.
	call ing_showd (in, file)

	# Print the error analysis information.
	call ing_errorsd (in, file, nl, x, y, wts, npts, nvars)

	# Print the results.
	call ing_resultsd (in, file, nl, x, y, wts, names, npts, nvars,
	    len_name)
end
