include "ms.h"

# T_MS_SET -- Set profile parameters in database.

procedure t_ms_set ()

char	image[SZ_FNAME]
char	keyword[SZ_LINE]

char	comment[SZ_LINE]
int	i, nspectra, ms_id
pointer	ms

bool	streq(), clgetb()
int	clscan(), nscan(), ms_db_id()
pointer	msmap()

begin
	# Get the task parameters and access the database.
	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_WRITE, 0)
	call clgstr ("keyword", keyword, SZ_LINE)

	# Decode the keyword for the desired database quantity.
	if (streq (keyword, "nspectra")) {
	    # Set the value of MS_NSPECTRA in the MULTISPEC header record.
	    if (clgetb ("read_list"))
		i = clscan ("list")
	    else
	        i = clscan ("value") 
	    call gargi (nspectra)
	    if (nscan () != 1)
		call error (MS_ERROR, "Bad parameter value")

	    # It is an error to attempt to change the value previously set.
	    if (MS_NSPECTRA(ms) == 0)
	        MS_NSPECTRA(ms) = nspectra
	    else if (MS_NSPECTRA(ms) != nspectra)
	        call error (MS_ERROR, "Attempt to change number of spectra")
	} else {
	    # Keyword is one of the database record names.  Convert to
	    # a MULTISPEC parameter ID and call the appropriate procedure.

	    ms_id = ms_db_id (ms, keyword)
	    switch (ms_id) {
	    case COMMENTS:
	        call com_set (ms, comment)
	    case I0, X0, S0, S1, S2:
	        call par_set (ms, ms_id, comment)
	    }
	}

	# Finish up.
	call msphdr (ms)
	call msunmap (ms)
end


# COM_SET -- Add a comment to the MULTISPEC database comment block.
#
# This procedure does CLIO.

procedure com_set (ms, comment)

pointer	ms				# MULTISPEC data structure
char	comment[SZ_LINE]		# Input comment buffer.

int	i

bool	clgetb()
int	clscan()

begin
	# Desire whether to use list input or CL parameter input.
	if (clgetb ("read_list")) {
	    # Read a list of comment strings.
	    while (clscan ("list") != EOF) {
	        call gargstr (comment, SZ_LINE)
	        call history (ms, comment)
	    }
	} else {
	    # Read a comment line from the parameter "value".
	    i = clscan ("value")
	    call gargstr (comment, SZ_LINE)
	    call history (ms, comment)
	}
end


# PAR_SET -- Set the values of the model parameters.
#
# This procedure does CLIO.

procedure par_set (ms, ms_id, comment)

pointer	ms			# MULTISPEC data structure
int	ms_id			# MULTISPEC ID
char	comment[SZ_LINE]	# Comment buffer

int	i, line, nsamples, sample, last_sample, spectrum
int	lines[3, MAX_RANGES], spectra[3, MAX_RANGES]
real	value
pointer	sp, samples

int	clscan(), nscan(), clgranges(), get_next_number()
int	get_sample_line(), get_sample_lines()
bool	dbaccess(), clgetb()

begin
	if ((MS_NSAMPLES(ms) == 0) || (MS_NSPECTRA(ms) == 0))
	    return

	# Enter the parameter in the database if necessary.
	if (!dbaccess (MS_DB(ms), NAME(ms, ms_id)))
	    call dbenter (MS_DB(ms), NAME(ms, ms_id),
		MS_NSPECTRA(ms) * SZ_REAL, MS_NSAMPLES(ms))

	# Determine input source.
	if (clgetb ("read_list")) {
	    # Read values from a list.
	    last_sample = 0
	    while (clscan ("list") != EOF) {

		# Get line, spectrum, and value from the list.
	        call gargi (line)
	        call gargi (spectrum)
	        call gargr (value)

		# Check that the data is valid otherwise go to next input.
	        if (nscan () != 3)
		    next
	        if ((spectrum < 1) || (spectrum > MS_NSPECTRA(ms)))
		    next

		# If the last sample is not the same as the previous sample
		# flush the last parameter values if the last sample is not
		# zero and get the next line of parameter values.

		sample = get_sample_line (ms, line)
	        if (sample != last_sample) {
		    if (last_sample != 0)
		        call mspparam (ms, ms_id, last_sample)
	            call msgparam (ms, ms_id, sample)
		    last_sample = sample
	        }

		# Set the parameter value.
	        PARAMETER(ms, ms_id, spectrum) = value
	    }

	    # Flush the last line of parameter values.
	    call mspparam (ms, ms_id, last_sample)

	} else {
	    # Set the parameter values for the selected lines and spectra
	    # to the CL parameter "value".

	    i = clgranges ("lines", 1, MS_LEN(ms, 2), lines, MAX_RANGES)
	    i = clgranges ("spectra", 1, MS_NSPECTRA(ms), spectra, MAX_RANGES)
	    i = clscan ("value")

	    # Convert the image lines to sample lines.
	    call smark (sp)
	    call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	    nsamples = get_sample_lines (ms, lines, Memi[samples])

	    # Check that the parameter value is a real number.
	    call gargr (value)
	    if (nscan () != 1)
		call error (MS_ERROR, "Bad parameter value")

	    # Go through the selected sample lines and spectra setting the
	    # parameter value.

	    do i = 1, nsamples {
		sample = Memi[samples + i - 1]
		call msgparam (ms, ms_id, sample)
		spectrum = 0
		while (get_next_number (spectra, spectrum) != EOF)
		    PARAMETER (ms, ms_id, spectrum) = value
		call mspparam (ms, ms_id, sample)
	    }
	}
		
	# Add a history comment.
	call sprintf (comment, SZ_LINE, "Values of parameter %s set.")
	    call pargstr (NAME(ms, ms_id))
	call history (ms, comment)
end
