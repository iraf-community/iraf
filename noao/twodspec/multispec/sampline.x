include <mach.h>
include	"ms.h"


# GET_SAMPLE_LINE -- Get the nearest sample line to the given image lines.
#
# The nearest sample line to each image line is found an returned
# as the function value.

int procedure get_sample_line (ms, line)

pointer	ms				# MULTISPEC data structure
int	line				# Image line

int	sample, midpoint

begin
	sample = 0
	midpoint = 0

	repeat {
	    sample = sample + 1
	    if (sample < MS_NSAMPLES(ms))
		midpoint = (LINE(ms, sample) + LINE(ms, sample + 1)) / 2
	    else if (sample == MS_NSAMPLES(ms))
		midpoint = MAX_INT
	    else
		break
	} until (line < midpoint)

	return (sample)
end


# GET_SAMPLE_LINES -- Get the sample lines for the given image lines.
#
# Image lines in the form of a range array are given.
# The nearest sample line to each image line is found.  The array of
# sample lines is returned and the function value is the number of
# sample lines.

int procedure get_sample_lines (ms, lines, samples)

pointer	ms				# MULTISPEC data structure
int	lines[ARB]			# Image line range array
int	samples[ARB]			# Return sample lines

int	nsamples, sample, line, midpoint
int	get_next_number()

begin
	nsamples = 0
	sample = 0
	midpoint = 0
	line = 0

	while (get_next_number (lines, line) != EOF) {
	    repeat {
	        sample = sample + 1
	        if (sample < MS_NSAMPLES(ms))
		    midpoint = (LINE(ms, sample) + LINE(ms, sample + 1)) / 2
	        else if (sample == MS_NSAMPLES(ms))
		    midpoint = MAX_INT
	        else
		    return (nsamples)
	    } until (line < midpoint)

	    nsamples = nsamples + 1
	    samples[nsamples] = sample
	    line = midpoint - 1
	}
	return (nsamples)
end
