include <fset.h>
include	<imhdr.h>
include "ms.h"

# EX_OUT -- Write and format the extracted spectra to the output image.
# SUM_PIXELS -- Sum pixel array between the limits lower and upper.
# EX_SET_VEBOSE -- Set and print verbose output.


# EX_OUT -- Write and format the extracted spectra to the output image.
#
# The type of output is selected by the value of ex_integral.
# If ex_integral = yes then sum the spectra profiles and output one value
# per spectrum otherwise output the strip spectra profiles.

procedure ex_out (im_out, line_out, spectra, lower, upper, ranges, profiles,
	len_profile, nspectra, ex_integral)

pointer	im_out				# Output image file descriptor
int	line_out			# Output line
int	spectra[ARB]			# Spectra range list
real	lower				# Lower integral limit
real	upper				# Upper integral limit
real	ranges[nspectra, LEN_RANGES]	# Starting points of profiles
real	profiles[len_profile, nspectra]	# Real spectra profiles
int	len_profile			# Length of spectra profiles
int	nspectra			# Number of spectra profiles
bool	ex_integral

int	i, spectrum_in, spectrum_out
real	x_min, x_max
pointer	buf_out

int	get_next_number()
real	sum_pixels()
pointer	impl3r()

begin
	# Loop through the selected spectra write an image line for one.
	spectrum_in = 0
	spectrum_out = 0
	while (get_next_number (spectra, spectrum_in) != EOF) {
	    spectrum_out = spectrum_out + 1
	    buf_out = impl3r (im_out, line_out, spectrum_out)

	    # Select between integrated and strip spectra output.  If
	    # integrated spectra call sum_pixels to integrate the spectrum
	    # profile else output the spectrum profile.
	    if (ex_integral) {
	        x_min = lower - ranges[spectrum_in, DX_START] + 1
	        x_max = upper - ranges[spectrum_in, DX_START] + 1
	        Memr[buf_out] =
		    sum_pixels (profiles[1, spectrum_in], x_min, x_max)
	    } else {
	        do i = 1, len_profile
		    Memr[buf_out + i - 1] = profiles[i, spectrum_in]
	    }
	}
end


# SUM_PIXELS -- Sum pixel array between the limits lower and upper.
# The limits may be partial pixels.  There is no checking for out of
# array range limits.

real procedure sum_pixels (pixels, x_min, x_max)

real	pixels[ARB]			# Pixel array to be summed
real	x_min				# Lower limit of sum
real	x_max				# Upper limit of sum

int	i, i_min, i_max
real	f, value

begin
	# Determine bounding integer limits.
	i_min = x_min + 0.5
	i_max = x_max + 0.5

	# Add partial pixel endpoints.

	f = min (x_max, i_min + 0.5) - x_min
	value = f * pixels[i_min]
	if (i_min >= i_max)
	    return (value)

	f = x_max - (i_max - 0.5)
	value = value + f * pixels[i_max]
	if (i_min + 1 > i_max - 1)
	    return (value)

	# Sum non-endpoint pixels.

	do i = i_min + 1, i_max - 1
	    value = value + pixels[i]

	return (value)
end

# EX_SET_VERBOSE -- Output procedures for verbose mode.

procedure ex_set_verbose (verbose)

bool	verbose

#entry ex_prnt1 (image_in, image_out)
char	image_in[1]
char	image_out[1]

# entry ex_prnt2 (line_in, line_out)
int	line_in, line_out, nreplaced

bool	flag

begin
	# Toggle verbose output.
	flag = verbose
	if (flag)
	    call fseti (STDOUT, F_FLUSHNL, YES)
	else
	    call fseti (STDOUT, F_FLUSHNL, NO)
	return

entry ex_prnt1 (image_in, image_out)

	# Set the verbose flag and print general header information.
	if (flag) {
	    call printf ("\nMULTISPEC Extraction Program\n\n")
	    call printf ("Image being extracted is %s.\n")
	        call pargstr (image_in)
	    call printf ("Output extraction image is %s.\n")
	        call pargstr (image_out)
	}
	return

entry ex_prnt2 (line_in, line_out)

	# Print the image line being extracted.
	if (flag) {
	    call printf ("Input image line = %d and output image line = %d.\n")
	        call pargi (line_in)
	        call pargi (line_out)
	}
	return

entry ex_prnt3 (nreplaced)

	# Print the number of pixels replaced in cleaning.
	if (flag && (nreplaced > 0)) {
	    call printf ("  Number of pixels replaced: %d\n")
	        call pargi (nreplaced)
	}
	return
end
