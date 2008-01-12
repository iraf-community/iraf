include <fset.h>
include "ms.h"

# T_MS_LIST -- Print general MULTISPEC database information.

procedure t_ms_list ()

char	image[SZ_FNAME]
char	keyword[SZ_LINE]
bool	titles

int	ms_id
pointer	ms

bool	clgetb(), streq()
int	ms_db_id()
pointer	msmap()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get task parameters.
	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_ONLY, 0)
	call clgstr ("keyword", keyword, SZ_LINE)
	titles = clgetb ("titles")

	# Check for special keywords.
	if (streq (keyword, "gauss5")) {
	    call g5_list (ms, keyword, titles)

	# Keyword is one of the database record names.  Convert to a
	# MULTISPEC id and switch to appropriate listing routine.
	} else {
	    ms_id = ms_db_id (ms, keyword)
	    switch (ms_id) {
	    case HDR:
	        call hdr_list (ms, keyword, titles)
	    case COMMENTS:
	        call com_list (ms, keyword, titles)
	    case SAMPLE:
		call sam_list (ms, keyword, titles)
	    case I0, X0, S0, S1, S2:
	        call par_list (ms, ms_id, keyword, titles)
	    case X0_FIT, S0_FIT, S1_FIT, S2_FIT:
	        call fit_list(ms, ms_id, keyword, titles)
	    }
	}

	call msunmap (ms)
end


# HDR_LIST - List the contents of the MULTISPEC database header

procedure hdr_list (ms, keyword, titles)

pointer	ms					# MULTISPEC data structure
char	keyword[ARB]				# List keyword
bool	titles					# Print titles?

begin
	call printf ("Image: %s\n")
	    call pargstr (MS_IMAGE(ms))
	call printf ("Keyword:  %s\n")
	    call pargstr (keyword)
	call printf ("Title: %s\n")
	    call pargstr (MS_TITLE(ms))
	call printf ("Number of spectra: %d\n")
	    call pargi (MS_NSPECTRA(ms))
	call printf ("Number of sample image lines: %d\n")
	    call pargi (MS_NSAMPLES(ms))
	call printf ("Image size: %d x %d\n")
	    call pargi (MS_LEN(ms, 1))
	    call pargi (MS_LEN(ms, 2))
end

procedure com_list (ms, keyword, titles)

pointer	ms					# MULTISPEC data structure
char	keyword[ARB]				# List keyword
bool	titles					# Print titles?
int	i

begin
	if (titles) {
	    call printf ("Image: %s\n")
		call pargstr (MS_IMAGE(ms))
	    call printf ("Keyword:  %s\n")
		call pargstr (keyword)
	    call printf ("Comments:\n")
	}

	for (i=1; (i <= SZ_MS_COMMENTS) && (COMMENT(ms, i) != EOS); i=i+1)
	    call putchar (COMMENT(ms, i))
end


# SAM_LIST -- List the sample image lines.

procedure sam_list (ms, keyword, titles)

pointer	ms					# MULTISPEC data structure
char	keyword[ARB]				# List keyword
bool	titles					# Print titles?
int	i

begin
	if (titles) {
	    call printf ("Image: %s\n")
		call pargstr (MS_IMAGE(ms))
	    call printf ("Keyword:  %s\n")
		call pargstr (keyword)
	    call printf ("Sample Image Lines:\n")
	}

	do i = 1, MS_NSAMPLES(ms) {
	    call printf ("%8d\n")
		call pargi (LINE(ms, i))
	}
end


# PAR_LIST -- Print MULTISPEC profile parameters.
#
# This procedure does some CLIO.

procedure par_list (ms, ms_id, keyword, titles)

pointer	ms					# MULTISPEC data structure
int	ms_id					# MULTISPEC parameter id
char	keyword[ARB]				# List keyword
bool	titles					# Print titles?

int	lines[3, MAX_RANGES], spectra[3, MAX_RANGES]
int	i, nsamples, sample, spectrum
pointer	sp, samples

int	clgranges(), get_next_number(), get_sample_lines()

begin
	if ((MS_NSAMPLES(ms) == 0) || (MS_NSPECTRA(ms) == 0))
	    return

	# Get desired image lines and spectra to be listed.
	i = clgranges ("lines", 1, MS_LEN(ms, 2), lines, MAX_RANGES)
	i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra, MAX_RANGES)

	# Convert image lines to sample lines.
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	nsamples = get_sample_lines (ms, lines, Memi[samples])

	# Print header titles if needed.
	if (titles) {
	    call printf ("Image: %s\n")
		call pargstr (MS_IMAGE(ms))
	    call printf ("Keyword:  %s\n")
		call pargstr (keyword)
	    call printf ("%8s %8s %8s\n")
		call pargstr ("Line")
		call pargstr ("Spectrum")
		call pargstr (NAME(ms, ms_id))
	}

	# For each sample line get the parameter values for the selected
	# parameter and list those for the selected spectra.
	do i = 1, nsamples {
	    sample = Memi[samples + i - 1]

	    call msgparam (ms, ms_id, sample)

	    spectrum = 0
	    while (get_next_number (spectra, spectrum) != EOF) {
		call printf ("%8d %8d %8.3g\n")
		    call pargi (LINE(ms, sample))
		    call pargi (spectrum)
		    call pargr (PARAMETER(ms, ms_id, spectrum))
	    }
	}

	call sfree (sp)
end


# FIT_LIST -- Print MULTISPEC fit.
#
# This procedure does CLIO.

procedure fit_list (ms, ms_id, keyword, titles)

pointer	ms					# MULTISPEC data structure
int	ms_id					# MULTISPEC parameter id
char	keyword[ARB]				# List keyword
bool	titles					# Print header titles?

int	lines[3, MAX_RANGES]
int	spectra[3, MAX_RANGES]

int	i, line, spectrum

real	cveval()
int	clgranges(), get_next_number()

begin
	if (MS_NSPECTRA(ms) == 0)
	    return

	# Get the image lines at which to evaluate the function and
	# the spectra to be listed.

	i = clgranges ("lines", 1, MS_LEN(ms, 2), lines, MAX_RANGES)
	i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra, MAX_RANGES)

	# Get the fits.
	call msgfits (ms, ms_id)

	# Print header titles if needed.
	if (titles) {
	    call printf ("Image: %s\n")
		call pargstr (MS_IMAGE(ms))
	    call printf ("Keyword:  %s\n")
		call pargstr (keyword)
	    call printf ("%8s %8s %8s\n")
		call pargstr ("Line")
		call pargstr ("Spectrum")
		call pargstr (NAME(ms, ms_id))
	}

	# For each selected image line evalute the functions for the
	# selected spectra and print the values.

	line = 0
	while (get_next_number (lines, line) != EOF) {
	    spectrum = 0
	    while (get_next_number (spectra, spectrum) != EOF) {
		call printf ("%8d %8d %8.3g\n")
		    call pargi (line)
		    call pargi (spectrum)
		    call pargr (cveval (CV(ms, ms_id, spectrum), real (line)))
	    }
	}
end


# G5_LIST -- Print MULTISPEC model gauss5 profile parameters.
#
# This procedure does CLIO.

procedure g5_list (ms, keyword, titles)

pointer	ms					# MULTISPEC data structure
char	keyword[ARB]				# List keyword
bool	titles					# Print header titles?

int	lines[3, MAX_RANGES], spectra[3, MAX_RANGES]
int	i, nsamples, sample, spectrum
pointer	sp, samples

int	clgranges(), get_next_number(), get_sample_lines()

begin
	if ((MS_NSAMPLES(ms) == 0) || (MS_NSPECTRA(ms) == 0))
	    return

	# Get desired image lines and spectra to be listed.
	i = clgranges ("lines", 1, MS_LEN(ms, 2), lines, MAX_RANGES)
	i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra, MAX_RANGES)

	# Convert image lines to sample lines.
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	nsamples = get_sample_lines (ms, lines, Memi[samples])

	# Print header titles if needed.
	if (titles) {
	    call printf ("Image: %s\n")
		call pargstr (MS_IMAGE(ms))
	    call printf ("Keyword:  %s\n")
		call pargstr (keyword)
	    call printf ("%8s %8s %8s %8s %8s %8s %8s\n")
		call pargstr ("Line")
		call pargstr ("Spectrum")
		call pargstr (NAME (ms, X0))
		call pargstr (NAME (ms, I0))
		call pargstr (NAME (ms, S0))
		call pargstr (NAME (ms, S1))
		call pargstr (NAME (ms, S2))
	}

	# For each sample line get the GAUSS5 values and list for the
	# selected spectra.
	do i = 1, nsamples {
	    sample = Memi[samples + i - 1]

	    call msggauss5 (ms, sample)

	    spectrum = 0
	    while (get_next_number (spectra, spectrum) != EOF) {
		call printf ("%8d %8d %8.3g %8.3g %8.3g %8.3g %8.3g\n")
		    call pargi (LINE(ms, sample))
		    call pargi (spectrum)
		    call pargr (PARAMETER(ms, X0, spectrum))
		    call pargr (PARAMETER(ms, I0, spectrum))
		    call pargr (PARAMETER(ms, S0, spectrum))
		    call pargr (PARAMETER(ms, S1, spectrum))
		    call pargr (PARAMETER(ms, S2, spectrum))
	    }
	}

	call sfree (sp)
end
