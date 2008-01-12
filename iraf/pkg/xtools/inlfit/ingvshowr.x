include <pkg/inlfit.h>


# ING_VSHOW -- Show fit parameters in verbose mode on the screen.

procedure ing_vshowr (in, file, nl, x, y, wts, names, npts, nvars, len_name,
	gt)

pointer	in		        # INLFIT pointer
char	file[ARB]		# Output file name
pointer	nl			# NLFIT pointer
real	x[ARB]			# Ordinates (npts * nvars)
real	y[ARB]			# Abscissas
real	wts[ARB]		# Weights
char	names[ARB]		# Object ids
int	npts			# Number of data points
int	nvars			# Number of variables
int	len_name		# Length of id name
pointer	gt			# Graphics tools pointer

begin
	# Print the title.
	call ing_title (in, file, gt)

	# Do the standard ing_show option.
	call ing_showr (in, file)

	# Print the error analysis information.
	call ing_errorsr (in, file, nl, x, y, wts, npts, nvars)

	# Print the results.
	call ing_resultsr (in, file, nl, x, y, wts, names, npts, nvars,
	    len_name)
end
