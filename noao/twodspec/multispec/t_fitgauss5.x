include "ms.h"

# T_FIT_GAUSS5 -- Fit the GAUSS5 model.
#
# This task selects the database, the sample lines to be modeled, the
# model fitting algorithm, whether to track models from one sample line
# to the next or model them independently.

procedure t_fit_gauss5 ()

char	image[SZ_FNAME]			# Image
int	lines[3, MAX_RANGES]		# Sample lines to be modeled
bool	track				# Track model solution
int	start				# Starting line for modeling
int	naverage			# Number of image lines to average
real	lower				# Starting point of profile
real	upper				# Ending point of profile

int	i, nsamples, sample_start, sample, line, improved
int	len_line, len_profile, nspectra, nparams
pointer	ms, im
pointer	sp, data, model, profiles, ranges, samples

int	get_sample_line(), get_sample_lines()
int	g5_fit1(), g5_fit2()
int	clgeti(), clgranges(), btoi()
bool	clgetb()
real	clgetr()
pointer	msmap(), immap()

include	"fitgauss5.com"

begin
	# Access the database and the image.
	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_WRITE, 0)
	im = immap (image, READ_ONLY, 0)

	# Get the task parameters.
	i = clgranges ("lines", 1, MS_LEN(ms, 2), lines, MAX_RANGES)
	i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra, MAX_RANGES)
	track = clgetb ("track")
	start = clgeti ("start")
	naverage = clgeti ("naverage")
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	factor = clgetr ("factor")

	# Algorithm 1 fits the parameters selected in the parameters array
	# simultaneously.  Algorithm 2 does not require the user to specify
	# the parameters.

	algorithm = clgeti ("algorithm")
	if (algorithm == 1) {
	    parameters[I0_INDEX] = btoi (clgetb ("fit_i0"))
	    parameters[X0_INDEX] = btoi (clgetb ("fit_x0"))
	    parameters[S0_INDEX] = btoi (clgetb ("fit_s0"))
	    parameters[S1_INDEX] = btoi (clgetb ("fit_s1"))
	    parameters[S2_INDEX] = btoi (clgetb ("fit_s2"))
	}

	# Select whether to smooth the shape parameters after fitting.
	# If smoothing is desired get the spline smoothing parameters.

	smooth[S0_INDEX] = btoi (clgetb ("smooth_s0"))
	smooth[S1_INDEX] = btoi (clgetb ("smooth_s1"))
	smooth[S2_INDEX] = btoi (clgetb ("smooth_s2"))
	if ((smooth[S0_INDEX] == YES) || (smooth[S1_INDEX] == YES) ||
	    (smooth[S2_INDEX] == YES)) {
	    call ms_set_smooth (1., real(MS_LEN(ms, 1)), MS_NSPECTRA(ms))
	}

	call g5_set_verbose (clgetb ("verbose"))
	call g5_prnt1 (image, naverage, track, start)
	    
	# Set the various array dimensions and allocate memory.
	len_line = MS_LEN(ms, 1)
	len_profile = nint (upper - lower + 2)
	nspectra = MS_NSPECTRA(ms)
	nparams = MS_NGAUSS5
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	call salloc (data, len_line, TY_REAL)
	call salloc (model, len_line, TY_REAL)
	call salloc (profiles, len_profile * nspectra * nparams, TY_REAL)
	call salloc (ranges, nspectra * LEN_RANGES, TY_REAL)

	# Convert from image lines to sample lines.
	nsamples = get_sample_lines (ms, lines, Memi[samples])
	sample_start = get_sample_line (ms, start)

	# Initialize forward tracking.  If tracking get the initial parameters,
	# model profiles and model line from the starting line.

	if (track) {
	    call msggauss5 (ms, sample_start)
	    call mod_gauss5 (ms, lower, Memr[profiles], Memr[ranges],
		len_profile, nspectra)
	    call set_model (ms, Memr[model], Memr[profiles], Memr[ranges],
		len_line, len_profile, nspectra)
	}

	# Track forward from the starting line to the specified sample lines.

	do i = 1, nsamples {
	    sample = Memi[samples + i - 1]
	    if (sample < sample_start)
		next
	    line = LINE(ms, sample)

	    # Get the image data line.
	    call msgimage (im, line, naverage, Memr[data])

	    # If not tracking get the initial parameters, model profiles, and
	    # model line for the current line.  Otherwise record the starting
	    # parameters.

	    if (!track) {
	        call msggauss5 (ms, sample)
	        call mod_gauss5 (ms, lower, Memr[profiles], Memr[ranges],
		    len_profile, nspectra)
	        call set_model (ms, Memr[model], Memr[profiles], Memr[ranges],
		    len_line, len_profile, nspectra)
	    } else
		call mspgauss5 (ms, sample)

	    call g5_prnt2 (line, Memr[data], len_line)

	    # Do the model fitting using the selected algorithm.
	    switch (algorithm) {
	    case 1:
	        improved = g5_fit1 (ms, Memr[data], Memr[model], Memr[profiles],
		    Memr[ranges], lower, len_profile)
	    case 2:
	        improved = g5_fit2 (ms, Memr[data], Memr[model], Memr[profiles],
		    Memr[ranges], lower, len_profile)
	    }

	    # If the new model parameters have improved the fit record them in
	    # the database.
	    if (improved == YES)
		call mspgauss5 (ms, sample)
	}

	# Initialize backward tracking.  If tracking get the initial parameters,
	# model profiles and model line from the starting line.

	if (track) {
	    call msggauss5 (ms, sample_start)
	    call mod_gauss5 (ms, lower, Memr[profiles], Memr[ranges],
		len_profile, nspectra)
	    call set_model (ms, Memr[model], Memr[profiles], Memr[ranges],
		len_line, len_profile, nspectra)
	}

	# Track backward from the starting line to the specified sample lines.

	do i = nsamples, 1, -1 {
	    sample = Memi[samples + i - 1]
	    if (sample >= sample_start)
		next
	    line = LINE(ms, sample)

	    # Get the image data line.
	    call msgimage (im, line, naverage, Memr[data])

	    # If not tracking get the initial parameters, model profiles, and
	    # model line for the current line.  Else record the starting
	    # parameters.

	    if (!track) {
	        call msggauss5 (ms, sample)
	        call mod_gauss5 (ms, lower, Memr[profiles], Memr[ranges],
		    len_profile, nspectra)
	        call set_model (ms, Memr[model], Memr[profiles], Memr[ranges],
		    len_line, len_profile, nspectra)
	    } else
		call mspgauss5 (ms, sample)

	    call g5_prnt2 (line, Memr[data], len_line)


	    # Do the model fitting using the selected algorithm.
	    switch (algorithm) {
	    case 1:
	        improved = g5_fit1 (ms, Memr[data], Memr[model], Memr[profiles],
		    Memr[ranges], lower, len_profile)
	    case 2:
	        improved = g5_fit2 (ms, Memr[data], Memr[model], Memr[profiles],
		    Memr[ranges], lower, len_profile)
	    }

	    # If the new model parameters have improved the fit record them in
	    # the database.

	    if (improved == YES)
		call mspgauss5 (ms, sample)
	}

	# Finish up.
	if ((smooth[S0_INDEX] == YES) || (smooth[S1_INDEX] == YES) ||
	    (smooth[S2_INDEX] == YES)) {
	    call ms_free_smooth ()
	}
	call imunmap (im)
	call history (ms, "Fit model")
	call msunmap (ms)
	call sfree (sp)
end
