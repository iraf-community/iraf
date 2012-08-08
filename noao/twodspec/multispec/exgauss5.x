include	<imhdr.h>
include "ms.h"


# EX_GAUSS5 -- Extract spectra using the GAUSS5 model.
#
# This procedure is called either by t_extract to extract spectra (either
# integrated or strip) or by t_newimage to extract a new image (either
# model or cleaned data).  It is called only if model GAUSS5 must be used
# for cleaning, blending corrections, or model extraction.

procedure ex_gauss5 (ms, im_in, im_out, spectra, lines, lower, upper,
    ex_spectra, ex_model, ex_integral)

pointer	ms				# MULTISPEC pointer
pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	spectra[ARB]			# Spectra range list
int	lines[ARB]			# Line range list
real	lower				# Lower limit of strip
real	upper				# Upper limit of strip
bool	ex_spectra			# Extract spectra or image line
bool	ex_model			# Extract model or data
bool	ex_integral			# Extract integrated spectra or strip

int	len_line, len_profile, nspectra, nparams
int	line_in, line_out
pointer	data, data_in, data_out
pointer	sp, model, profiles, ranges, data_profiles

int	get_next_number()
pointer	imgl2r(), impl2r()

begin
	# Set array size variables.
	len_line = MS_LEN(ms, 1)
	nspectra = MS_NSPECTRA(ms)
	nparams = MS_NGAUSS5
	len_profile = nint (upper - lower + 2)

	# Allocate and setup necessary arrays.
	call smark (sp)
	call salloc (model, len_line, TY_REAL)
	call salloc (ranges, nspectra * LEN_RANGES * 3, TY_REAL)
	call salloc (profiles, len_profile * nspectra * nparams * 3, TY_REAL)
	call salloc (data_profiles, len_profile * nspectra, TY_REAL)

	# Initialize ranges arrays.
	Memr[ranges] = INDEFR

	# Loop through the input lines and write an output line for each
	# input line.
	line_in = 0
	line_out = 0
	while (get_next_number (lines, line_in) != EOF) {
	    line_out = line_out + 1
	    call ex_prnt2 (line_in, line_out)

	    # Get the multi-spectra image data.
	    data = imgl2r (im_in, line_in)

	    # Get the GAUSS5 model profiles using interpolation between the
	    # sample lines.
	    call int_gauss5 (ms, lower, Memr[profiles], Memr[ranges],
		len_profile, nspectra, nparams, line_in)

	    # Iteratively fit the profile scales to the data and replace
	    # deviant pixels by model values.
	    call fit_and_clean (ms, Memr[data], Memr[model], Memr[ranges],
		Memr[profiles], len_line, len_profile, nspectra, nparams)

	    # Unblend data spectra only if needed.
	    if (ex_spectra && !ex_model)
		call unblend (Memr[data], Memr[data_profiles], Memr[model],
		    Memr[profiles], Memr[ranges], len_line, len_profile,
		    nspectra)

	    if (!ex_spectra) {
		# Output a new model or data image line.
		data_out = impl2r (im_out, line_out)
		if (ex_model)
		    data_in = model
		else
		    data_in = data
		call amovr (Memr[data_in], Memr[data_out], len_line)
	    } else {
		# Output either model or data extracted spectra.
		if (ex_model)
		    data_in = profiles
		else
		    data_in = data_profiles
	        call ex_out (im_out, line_out, spectra, lower, upper,
		    Memr[ranges], Memr[data_in], len_profile, nspectra,
		    ex_integral)
	    }
	}

	# Free allocated memory.
	call sfree (sp)
end
