include	<imhdr.h>
include	<math/interp.h>
include "ms.h"

# EX_SMOOTH    -- Extract spectra using the SMOOTH model.
# FIT_PROFILES -- Get SMOOTH profiles and fit the profiles to the data while
#                 replacing deviant pixels by model profile values.


# EX_SMOOTH -- Extract spectra using the SMOOTH model.
#
# This procedure is called either by t_extract to extract spectra (either
# integrated or strip) or by t_newimage to extract a new image (either
# model or cleaned data).  It is called only if model SMOOTH must be used
# for cleaning or model extraction.  It outputs the extracted spectra to
# the output image file.  Note that this task does CLIO.

procedure ex_smooth (ms, im_in, im_out, spectra, lines, lower, upper,
    ex_spectra, ex_model, ex_integral)

pointer	ms				# MULTISPEC pointer
pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	spectra[ARB]			# Spectra range list
int	lines[ARB]			# Line range list
real	lower				# Lower limit of strips
real	upper				# Upper limit of strips
bool	ex_spectra			# Extract spectra or image line?
bool	ex_model			# Extract model or data?
bool	ex_integral			# Extract integrated or strip spectra?

# User input parameters:
int	nlines				# Lines to average for smooth model
int	interpolator			# Line interpolator type

int	len_line, nspectra, len_profile, len_profiles
int	line_in, line_out
pointer	sp, data, data_in, data_out, model, ranges, profiles, coeff

int	clgeti(), get_next_number(), clginterp()
pointer	impl2r()

begin
	# Get parameters for model SMOOTH.
	nlines = clgeti ("naverage") + 1
	interpolator = clginterp ("interpolator")

	# Set array lengths.
	len_line = IM_LEN(im_in, 1)
	nspectra = MS_NSPECTRA(ms)
	len_profile = nint (upper - lower + 1)
	len_profiles = len_profile * nspectra

	# Allocate working memory.
	call smark (sp)
	call salloc (data, len_profiles, TY_REAL)
	call salloc (model, len_profiles, TY_REAL)
	call salloc (ranges, nspectra * LEN_RANGES, TY_REAL)
	call salloc (profiles, len_profiles * (nlines + 1), TY_REAL)
	call salloc (coeff, 2 * len_line + SZ_ASI, TY_REAL)

	# Initialize ranges and interpolation arrays.
	call amovkr (lower, Memr[ranges + (DX_START-1)*nspectra], nspectra) 
	call asiset (Memr[coeff], interpolator)

	# Get fit position functions from the database.
	call msgfits (ms, X0_FIT)

	# Loop through the input image lines and write output line.
	line_in = 0
	line_out = 0
	while (get_next_number (lines, line_in) != EOF) {
	    line_out = line_out + 1
	    call ex_prnt2 (line_in, line_out)

	    # Get the SMOOTH profiles and the data for the input line.
	    call set_smooth (ms, im_in, line_in, Memr[ranges], Memr[profiles],
		Memr[coeff], len_profile, nspectra, nlines, Memr[data],
		Memr[model])

	    # Fit and clean the data and model.
	    call fit_smooth (line_in, Memr[data], Memr[model],
		Memr[profiles], len_profile, nspectra, nlines)

	    # Select model or data to be output.
	    if (ex_model)
		data_in = model
	    else
		data_in = data

	    if (ex_spectra) {
		# Extract model or data spectra.
	        call ex_out (im_out, line_out, spectra, lower, upper,
		    Memr[ranges], Memr[data_in], len_profile, nspectra,
		    ex_integral)
	    } else {
		# Extract model or data image line.
		data_out = impl2r(im_out, line_out)
		call set_model1 (ms, line_in, Memr[data_in], Memr[coeff],
		    Memr[ranges], len_line, len_profile, nspectra,
		    Memr[data_out])
	    }
	}

	# Free allocated memory.
	call sfree (sp)
end
