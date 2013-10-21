include <math/curfit.h>
include	"ms.h"

# T_FIT_FUNCTION -- Fit a function to selected spectra parameters.
#
# A function is fit to the parameter values determined at the sample
# lines for selected spectra.  The function coefficients are stored in
# the database and the fitted values replace the original values at
# the sample lines.  The type of function, the parameter to be fitted,
# the sample lines used in the fit, and the spectra to be fitted
# are all selected by the user.

procedure t_fit_function()

char	image[SZ_FNAME]			# Image affected
char	parameter[SZ_LINE]		# Parameter to be fit
int	function			# Type of fitting function
int	order				# Order of the fitting function
int	spectra[3, MAX_RANGES]		# Spectra to be fitted
pointer	samples				# Sample lines to be fitted.

int	i, param_id, nsamples
pointer	ms, sp

int	ms_db_id(), clgranges(), get_sample_lines()
pointer	msmap()

begin
	# Access database and determine parameter to be fit and the
	# fitting function and order.

	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_WRITE, 0)
	call clgstr ("parameter", parameter, SZ_LINE)
	param_id = ms_db_id (ms, parameter)
	call clgcurfit ("function", "order", function, order)

	# Get the image lines to be used in the fit and convert to sample
	# lines.  Get the spectra to be fit.

	i = clgranges ("lines", 1, MS_LEN(ms, 2), spectra, MAX_RANGES)
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	nsamples = get_sample_lines (ms, spectra, Memi[samples])
	i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra,
	    MAX_RANGES)

	# Fit the parameters for each spectrum, store the fits in the database,
	# and substitute the fitted values for the parameter values at all
	# the sample lines.

	call fit_function (ms, Memi[samples], nsamples, spectra, param_id,
	    function, order)

	# Finish up.
	call msphdr (ms)
	call msunmap (ms)
	call sfree (sp)
end



# FIT_FUNCTION -- Fit a function to the parameter data.
#
# If the fit coefficients for the specified parameter are not in
# the database then the database entry is created.

procedure fit_function (ms, lines, nlines, spectra, param_id, function, order)

pointer	ms				# MULTISPEC data structure
int	lines[nlines]			# Sample lines to be used
int	nlines				# Number of sample lines
int	spectra[ARB]			# Spectra to be fitted
int	param_id			# Parameter being fit
int	function			# Function to be fit
int	order				# Order of the function

char	comment[SZ_LINE]
int	i, spectrum, fit_id, ier
real	x, wt

int	ms_fit_id(), get_next_number()
real	cveval()
bool	dbaccess()

begin
	# Determine the MULTISPEC fit id from the parameter id.
	fit_id = ms_fit_id (param_id)
	if (fit_id == ERR)
	    call error (MS_ERROR, "Unknown fit identifier")

	# Enter the fit records in the database if necessary.
	if (!dbaccess (MS_DB(ms), NAME(ms, fit_id)))
	    call dbenter (MS_DB(ms), NAME(ms, fit_id),
		(7 + MS_NSAMPLES(ms)) * SZ_REAL, MS_NSPECTRA(ms))

	# Allocate memory for the curfit data structures pointers.
	if (MS_DATA(ms, fit_id) == NULL)
	    call malloc (MS_DATA(ms, fit_id), MS_NSPECTRA(ms), TY_INT)

	# Initialize the curfit data structures.
	# If the order is INDEF then use maximum order assuming no INDEF points.
	spectrum = 0
	while (get_next_number (spectra, spectrum) != EOF) {
	    if (IS_INDEFI (order)) {
	        switch (function) {
	        case LEGENDRE, CHEBYSHEV:
		    order =  nlines
	        case SPLINE3:
		    order = nlines - 3
	        }
	    }
	    call cvinit (CV(ms, fit_id, spectrum), function, order, 1.,
		real (MS_LEN(ms, 2)))
	}

	# Accumulate the parameter values.
	do i = 1, nlines {
	    x = LINE(ms, lines[i])
	    call msgparam (ms, param_id, lines[i])

	    spectrum = 0
	    while (get_next_number (spectra, spectrum) != EOF)
		call cvaccum (CV(ms, fit_id, spectrum), x,
		    PARAMETER(ms, param_id, spectrum), wt, WTS_UNIFORM)
	}

	# Compute and write the fit coeffients to the database.

	spectrum = 0
	while (get_next_number (spectra, spectrum) != EOF) {
	    call cvsolve (CV(ms, fit_id, spectrum), ier)
	    if (ier == NO_DEG_FREEDOM)
		call error (MS_ERROR, "Error fitting parameters")
	    call mspfit (ms, fit_id, spectrum)
	}

	# For each sample line and each selected spectrum replace the
	# selected parameter value with the fit evaluation.

	do i = 1, MS_NSAMPLES(ms) {
	    x = LINE(ms, i)
	    call msgparam (ms, param_id, i)

	    spectrum = 0
	    while (get_next_number (spectra, spectrum) != EOF)
		PARAMETER(ms, param_id, spectrum) =
		    cveval (CV(ms, fit_id, spectrum), x)

	    call mspparm (ms, param_id, i)
	}

	# Add a comment to the database comments.

	call sprintf (comment, SZ_LINE, "Fit a function to parameter %s.")
	    call pargstr (NAME(ms, param_id))
	call history (ms, comment)
end
