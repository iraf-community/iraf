include	<imhdr.h>
include	<math/interp.h>
include "ms.h"

# EX_STRIP  -- Simple strip extraction of spectra.
# EX_STRIP1 -- Extract integrated spectra.
# EX_STRIP2 -- Extract two dimensional strip spectra.


# EX_STRIP -- Simple strip extraction of spectra.
#
# This procedure is called either by t_extract to extract spectra (either
# integrated or strip) or by t_newimage to extract a new image.
# Since there is no modeling only data spectra or image lines are extracted.
# It outputs the extracted spectra or image lines to the output image file.

procedure ex_strip (ms, im_in, im_out, spectra, lines, lower, upper,
    ex_spectra, ex_model, ex_integral)

pointer	ms				# MULTISPEC pointer
pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	spectra[ARB]			# Spectra range list
int	lines[ARB]			# Line range list
real	lower				# Lower limit of strips
real	upper				# Upper limit of strips
bool	ex_spectra			# Extract spectra or image line
bool	ex_model			# Extract model or data
bool	ex_integral			# Extract integrated spectra or strip

int	line_in, line_out
pointer	data_in, data_out

int	get_next_number()
pointer	imgl2r(), impl2r()

begin
	if (ex_model)
	    call error (MS_ERROR, "Can't extract model")

	if (ex_spectra) {
	    # Extract spectra using ex_strip1 for integrated spectra and
	    # ex_strip2 for strip spectra.
	    if (ex_integral)
	        call ex_strip1 (ms, im_in, im_out, spectra, lines, lower,
		    upper)
	    else
	        call ex_strip2 (ms, im_in, im_out, spectra, lines, lower,
		    upper)
	} else {
	    # Create a new multi-spectra image by copying the selected
	    # input image lines to the output image.
	    line_in = 0
	    line_out = 0
	    while (get_next_number (lines, line_in) != EOF) {
		line_out = line_out + 1
		data_in = imgl2r (im_in, line_in)
		data_out = impl2r (im_out, line_out)
		call amovr (Memr[data_in], Memr[data_out], IM_LEN(im_out, 1))
	    }
	}
end

# EX_STRIP1 -- Extract integrated spectra.
#
# For each spectrum in the spectra range list and for each line in
# the line range list the pixels between lower and upper (relative
# to the spectrum center) are summed.
# The spectra positions are obtained from the MULTISPEC database.

procedure ex_strip1 (ms, im_in, im_out, spectra, lines, lower, upper)

pointer	ms				# MULTISPEC pointer
pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	spectra[ARB]			# Spectra range list
int	lines[ARB]			# Line range list
real	lower				# Lower limit of strips
real	upper				# Upper limit of strips

int	line_in, line_out, spectrum_in, spectrum_out
real	x_center, x_start, x_end
pointer	buf_in, buf_out

real	sum_pixels(), cveval()
int	get_next_number()
pointer	imgl2r(), impl3r()

begin
	# Get fit functions for spectra positions. 
	call msgfits (ms, X0_FIT)

	# Loop through the input lines and write integrated spectra out.
	line_in = 0
	line_out = 0
	while (get_next_number (lines, line_in) != EOF) {
	    line_out = line_out + 1

	    # Get the input data line.
	    buf_in = imgl2r (im_in, line_in)

	    # Loop the the spectra, calculate the integrated luminosity and
	    # write it to the output image.
	    spectrum_in = 0
	    spectrum_out = 0
	    while (get_next_number (spectra, spectrum_in) != EOF) {
		spectrum_out = spectrum_out + 1

		buf_out = impl3r (im_out, line_out, spectrum_out)

		# Determine the spectrum limits from spectrum center position.
		x_center = cveval (CV(ms, X0_FIT, spectrum_in), real (line_in))
		x_start = max (1., x_center + lower)
		x_end = min (real (IM_LEN(im_in, 1)), x_center + upper)
		Memr[buf_out] =
		    sum_pixels (Memr[buf_in], x_start, x_end)
	    }
	}
end

# EX_STRIP2 -- Extract two dimensional strip spectra.
#
# Each line in the range list is fit by an image interpolator and then for
# each spectrum in spectra range list the interpolator values between lower
# and upper (relative to the spectrum center) are written to a three
# dimensional image.  There is one band for each spectrum.  The spectra
# positions are obtained from the MULTISPEC database.
# The procedure requests the interpolator type using CLIO.

procedure ex_strip2 (ms, im_in, im_out, spectra, lines, lower, upper)

pointer	ms				# MULTISPEC pointer
pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	spectra[ARB]			# Spectra range list
int	lines[ARB]			# Line range list
real	lower				# Lower limit of strip
real	upper				# Upper limit of strip

int	interpolator			# Array interpolar type

int	i, len_in, len_out, line_in, line_out, spectrum_in, spectrum_out
real	x, x_start
pointer	buf_in, buf_out
pointer	sp, coeff

int	get_next_number(), clginterp()
real	asival(), cveval()
pointer	imgl2r(), impl3r()
errchk	salloc, imgl2r, impl3r
errchk	asiset, asifit, asival, clginterp

begin
	# Get the image interpolator type.
	interpolator = clginterp ("interpolator")

	len_in = IM_LEN (im_in, 1)
	len_out = nint (upper - lower + 1)

	# Set up the interpolator coefficient array.
	call smark (sp)
	call salloc (coeff, 2 * len_in + SZ_ASI, TY_REAL)
	call asiset (Memr[coeff], interpolator)

	# Get the spectra position functions from the database.
	call msgfits (ms, X0_FIT)

	# Loop through the input lines, do the image interpolation and write
	# the strip spectra to the output.
	line_in = 0
	line_out = 0
	while (get_next_number (lines, line_in) != EOF) {
	    line_out = line_out + 1

	    # Get the input data and fit an interpolation function.
	    buf_in = imgl2r (im_in, line_in)
	    call asifit (Memr[buf_in], len_in, Memr[coeff])

	    # Loop through the spectra writing the strip spectra.
	    spectrum_in = 0
	    spectrum_out = 0
	    while (get_next_number (spectra, spectrum_in) != EOF) {
		spectrum_out = spectrum_out + 1
		buf_out = impl3r (im_out, line_out, spectrum_out)

		# Determine the starting position for the strips and
		# evaluate the interpolation function at each point in
		# the strip.
		x_start = cveval (CV(ms, X0_FIT, spectrum_in), real (line_in)) +
		    lower
		do i = 1, len_out {
		    x = x_start + i - 1
		    if ((x < 1) || (x > len_in))
			Memr[buf_out + i - 1] = 0.
		    else
		        Memr[buf_out + i - 1] = asival (x, Memr[coeff])
		}
	    }
	}

	# Free interpolator memory.
	call sfree (sp)
end
