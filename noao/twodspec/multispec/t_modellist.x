include	<imhdr.h>
include "ms.h"


# T_MODEL_LIST -- List model values for selected columns and sample lines.
#
# The output list format is column, image line, data value, model value.
# This task differs from t_new_image primarily in that there is no profile
# interpolation.  The model is evaluated only at the sample lines.  It
# is used to check the results of the model fitting tasks.

procedure t_model_list ()

# User parameters:
char	image[SZ_FNAME]			# Image
int	model_type			# Model type: gauss5, profile
int	columns[3, MAX_RANGES]		# Columns to be listed
int	lines[3, MAX_RANGES]		# Sample Lines to be listed
int	naverage			# Number of image lines to average
real	lower				# Lower limit of profile model
real	upper				# Upper limit of profile model

int	i, sample, nsamples, line, column
pointer	ms, im
pointer	sp, samples, data, model

int	clgeti(), ms_model_id(), clgranges()
int	get_next_number(), get_sample_lines
real	clgetr()
pointer	msmap(), immap()

begin
	# Access the database and image.
	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_ONLY, 0)
	im = immap (image, READ_ONLY, 0)

	# Get the task parameters.
	model_type = ms_model_id ("model")
	i = clgranges ("columns", 1, IM_LEN(im, 1), columns, MAX_RANGES)
	i = clgranges ("lines", 1, IM_LEN(im, 2), lines, MAX_RANGES)
	naverage = clgeti ("naverage")
	lower = clgetr ("lower")
	upper = clgetr ("upper")

	# Currently only model GAUSS5 is available.
	if (model_type != GAUSS5)
	    return

	# Allocate memory for the sample lines, data and model.
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	call salloc (data, IM_LEN(im, 1), TY_REAL)
	call salloc (model, IM_LEN(im, 1), TY_REAL)

	# Convert to sample lines.
	nsamples = get_sample_lines (ms, lines, Memi[samples])

	# For each sample line get the data line and compute a model line.
	# Print the data and model values for the selected image columns.
	do i = 1, nsamples {
	    sample = Memi[samples + i - 1]
	    line = LINE(ms, sample)

	    call msgimage (im, line, naverage, Memr[data])

	    switch (model_type) {
	    case GAUSS5:
	        call gauss5_model (ms, sample, lower, upper, Memr[model])
	    }

	    column = 0
	    while (get_next_number (columns, column) != EOF) {
	        call printf ("%d %d %g %g\n")
		    call pargi (column)
		    call pargi (line)
		    call pargr (Memr[data + column - 1])
		    call pargr (Memr[model + column - 1])
	    }
	}

	call sfree (sp)
	call imunmap (im)
	call msunmap (ms)
end


# GAUSS5_MODEL -- Generate a line of the GAUSS5 model.

procedure gauss5_model (ms, line, lower, upper,  model)

pointer	ms			# MULTISPEC data structure
int	line			# Sample line
real	lower			# Lower profile limit
real	upper			# Upper profile limit
real	model[ARB]		# Model data array to be returned

int	nspectra, nparams, len_line, len_profile
pointer	sp, profiles, ranges

begin
	# Set the dimensions of the arrays.
	nspectra = MS_NSPECTRA(ms)
	nparams = MS_NGAUSS5
	len_line = MS_LEN(ms, 1)
	len_profile = nint (upper - lower + 2)

	# Allocate arrays.
	call smark (sp)
	call salloc (ranges, nspectra * LEN_RANGES, TY_REAL)
	call salloc (profiles, len_profile * nspectra * nparams, TY_REAL)

	# Read the model parameters for the specified sample line.
	call msggauss5 (ms, line)

	# Calculate the model profiles.
	call mod_gauss5 (ms, lower, Memr[profiles], Memr[ranges], len_profile,
	    nspectra)

	# Make a model line using the model profiles.
	call set_model (ms, model, Memr[profiles], Memr[ranges], len_line,
	    len_profile, nspectra)

	# Return memory.
	call sfree (sp)
end
