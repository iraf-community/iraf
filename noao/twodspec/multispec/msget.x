include <imhdr.h>
include "ms.h"

# MSGET -- Allocate memory and get data from the MULTISPEC database
# and associated image.
#
# MSGHDR	-- Allocate memory and get MULTISPEC header information.
# MSGCOMMENTS	-- Allocate memory and get MULTISPEC comments.
# MSGPARAM	-- Allocate memory and get a line of MULTISPEC parameter data.
# MSGSAMPLE	-- Allocate memory and get SAMPLE line array.
# MSGFIT	-- Get parameter fit for a spectrum.
# MSGFITS	-- Get parameter fit for all spectra.
# MSGGAUSS5	-- Get a line of GAUSS5 parameter data.
# MSGIMAGE	-- Get a line of the image with possible averaging.


# MSGHDR -- Allocate memory and get MULTISPEC header information.

procedure msghdr (ms)

pointer	ms				# MULTISPEC data structure

int	i

int	dbread()

begin
	if (MS_DATA(ms, HDR) == NULL)
	    call calloc (MS_DATA(ms, HDR), LEN_MS_HDR, TY_STRUCT)
	i = dbread (MS_DB(ms), NAME(ms, HDR), HEADER(ms), 1)
end

# MSGCOMMENTS -- Allocate memory and get MULTISPEC comments.

procedure msgcomments (ms)

pointer	ms				# MULTISPEC data structure

int	i

int	dbread()

begin
	if (MS_DATA(ms, COMMENTS) == NULL)
	    call calloc (MS_DATA(ms, COMMENTS), SZ_MS_COMMENTS, TY_CHAR)
	i = dbread (MS_DB(ms), NAME(ms, COMMENTS), COMMENT(ms, 1), 1)
end

# MSGPARAM -- Allocate memory and get a line of MULTISPEC parameter data.

procedure msgparam (ms, parameter, line)

pointer	ms				# MULTISPEC data structure
int	parameter			# Parameter ID
int	line				# Sample line to be obtained

int	i
char	reference[SZ_MS_KEY]

bool	is_param_id()
int	dbread()

begin
	# Check if the the requested parameter is valid.
	if (!is_param_id (parameter))
	    call error (MS_ERROR, "Bad parameter identifier")

	if (MS_DATA(ms, parameter) == NULL)
	    call calloc (MS_DATA(ms, parameter), MS_NSPECTRA(ms), TY_REAL)

	# Make reference to the desired database record.
	call sprintf (reference, SZ_MS_KEY, "%s[%d]")
	    call pargstr (NAME(ms, parameter))
	    call pargi (line)

	i = dbread (MS_DB(ms), reference, PARAMETER(ms, parameter, 1), 1)
end

# MSGSAMPLE -- Allocate memory and get SAMPLE line array.

procedure msgsample (ms)

pointer	ms				# MULTISPEC data structure

int	i

int	dbread()

begin
	if (MS_DATA(ms, SAMPLE) == NULL)
	    call malloc (MS_DATA(ms, SAMPLE), MS_NSAMPLES(ms), TY_INT)
	i = dbread (MS_DB(ms), NAME(ms, SAMPLE), LINE(ms,1), 1)
end


# MSGFIT -- Get parameter fit for a spectrum.

procedure msgfit (ms, parameter, spectrum)

pointer	ms				# MULTISPEC data structure
int	parameter			# Parameter ID for desired fit
int	spectrum			# Spectrum

int	i
char	reference[SZ_MS_KEY]
pointer	sp, fit

bool	is_fit_id()
int	dbread()

errchk	cvrestore

begin
	# Check if for valid parameter id.
	if (!is_fit_id (parameter))
	    call error (MS_ERROR, "Bad fit identifier")

	# Allocate memory for the curfit pointers.
	if (MS_DATA(ms, parameter) == NULL)
	    call malloc (MS_DATA(ms, parameter), MS_NSPECTRA(ms), TY_INT)

	# Allocate memory for the curfit coefficients.
	call smark (sp)
	call salloc (fit, 7 + MS_NSAMPLES(ms), TY_REAL)

	# Reference appropriate data.
	call sprintf (reference, SZ_MS_KEY, "%s[%d]")
	    call pargstr (NAME(ms, parameter))
	    call pargi (spectrum)

	i = dbread (MS_DB(ms), reference, Memr[fit], 1)
	iferr (call cvrestore (CV(ms, parameter, spectrum), Memr[fit]))
	    ;

	call sfree (sp)
end


# MSGFITS -- Get parameter fits.

procedure msgfits (ms, parameter)

pointer	ms				# MULTISPEC data structure
int	parameter			# Parameter ID for desired fit

int	i

begin
	do i = 1, MS_NSPECTRA(ms)
	    call msgfit (ms, parameter, i)
end


# MSGGAUSS5 -- Get a line of GAUSS5 parameter data.

procedure msggauss5 (ms, line)

pointer	ms				# MULTISPEC data structure
int	line				# Sample line to be obtained

begin
	call msgparam (ms, I0, line)
	call msgparam (ms, X0, line)
	call msgparam (ms, S0, line)
	call msgparam (ms, S1, line)
	call msgparam (ms, S2, line)
end


# MSGIMAGE -- Get a line of the image with possible averaging.

procedure msgimage (im, line, naverage, data)

pointer	im				# Image descriptor
int	line				# Line to be gotten from the image
int	naverage			# Number of line to use in average
real	data[ARB]			# The output data array

int	i, line_start, line_end
real	nlines
pointer	buf

pointer	imgl2r()

begin
	# If naverage is <= 1 copy the image line to the data array
	# Else average the several lines.

	if (naverage <= 1) {
	    call amovr (Memr[imgl2r (im, line)], data, IM_LEN(im,1))
	} else {
	    # Determine starting and ending lines for the average.
	    line_start = max (1, line - naverage / 2)
	    line_end = min (IM_LEN(im, 2), line_start + naverage - 1)

	    # Clear data array for accumulating sum and then vector
	    # add the image lines.
	    call aclrr (data, IM_LEN(im, 1))
	    do i = line_start, line_end {
	        buf = imgl2r (im, i)
	        call aaddr (Memr[buf], data, data, IM_LEN(im, 1))
	    }

	    # Vector divide by the number of lines to form average.
	    nlines = line_end - line_start + 1
	    call adivkr (data, nlines, data, IM_LEN(im, 1))
	}
end
