# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CRT_TWEAK_NDC -- Alter the input ndc endpoints so that the ratio
# of device_pixels to image_pixels is an integer.  This procedure insures
# an integer replication (or decimation) factor.  For replication, 
# ndevice_pixels_spanned / nimage_values_output = an_integer.  For
# decimation, ndevice_pixels_spanned / nimage_values_output = 1 / an_integer.
# The NDC coordinates returned also represent integer device pixels.

procedure crt_tweak_ndc (nvalues, ndc_start, ndc_end, device_res)

int	nvalues			# Number of image values to output
real	ndc_start, ndc_end	# NDC endpoints - changed on output
int	device_res		# Full device resolution

int	ndevice_elements, first_device_element, last_device_element, int_extra
int	dev_pixel_1, dev_pixel_2, desired_dev_elements, desired_inverse
real	ndc_extra, extra, real_extra
double	gki_tweak

begin
	gki_tweak = double (32767) / double (32768)
	if (int (ndc_end) == 1)
	    ndc_end = gki_tweak
	first_device_element = ndc_start * device_res 
	last_device_element  = ndc_end * device_res
	ndevice_elements = (last_device_element - first_device_element) + 1

	if (mod (ndevice_elements, nvalues) != 0) {
	    # Calculate amount to be altered
	    real_extra = real (ndevice_elements) / real (nvalues)
	    if (real_extra > 1.0) {
	        # Tweak to get an integer replication factor
	        int_extra  = ndevice_elements / nvalues
	        extra = real ((real_extra - int_extra) * nvalues)
	        ndc_extra = extra / device_res
	        ndc_start = ndc_start + (ndc_extra / 2.0)
	        ndc_end   = ndc_end   - (ndc_extra / 2.0)
	    } else {
		# Tweak to get an integer decimation factor
		real_extra = real (nvalues) / real (ndevice_elements)
		desired_inverse = int (real_extra) + 1
		desired_dev_elements = nvalues / desired_inverse
		extra = desired_dev_elements - ndevice_elements
		ndc_extra = real (extra) / real (device_res)
		ndc_start = ndc_start - (ndc_extra / 2.0)
		ndc_end   = ndc_end   + (ndc_extra / 2.0)
	    }
	}

	# Now have ndc coordinates of starting and ending pixel such
	# that the replication or decimation factor is
	# an integer.  Now insure that the ndc coordinates refer to 
	# integer device pixels  so that truncation later in the
	# processing doesn't alter this replication factor.  In what
	# follows, note that dev_pixel_1 and dev_pixel_2
	# are 0-based; dev_pixel_1 is the first pixel to be filled, and 
	# dev_pixel_2 is the first pixel NOT to be filled, in accordance
	# with Richard's notes.

	dev_pixel_1 = ndc_start * device_res
	dev_pixel_2 = (ndc_end  * device_res) + 1

	ndc_start = real (dev_pixel_1) / real (device_res) / gki_tweak
	ndc_end = real (dev_pixel_2) / real (device_res) / gki_tweak
end
