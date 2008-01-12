include "ms.h"

# MSPUT -- Put information in the MULTISPEC database.
#
# MSPHDR	-- Put MULTISPEC header record in the database.
# MSPCOMMENTS	-- Put MULTISPEC comment record into the database.
# MSPSAMPLE	-- Put MULTISPEC sample record into the database.
# MSPPARAM	-- Put a line of MULTISPEC parameter data.
# MSPGAUSS5	-- Put a line of GAUSS5 parameter data.
# MSPFIT	-- Put fit coefficients for a spectrum.
# MSPFITS	-- Put fit coefficients for all spectra.


# MSPHDR -- Put MULTISPEC header record in the database.

procedure msphdr (ms)

pointer	ms				# MSIO descriptor

begin
	call dbwrite (MS_DB(ms), NAME(ms, HDR), HEADER(ms), 1)
end


# MSPCOMMENTS -- Put MULTISPEC comment record into the database.

procedure mspcomments (ms)

pointer	ms				# MSIO descriptor

begin
	call dbwrite (MS_DB(ms), NAME(ms, COMMENTS), COMMENT(ms, 1), 1)
end


# MSPSAMPLE -- Put MULTISPEC sample record into the database.

procedure mspsample (ms)

pointer	ms				# MSIO descriptor

begin
	call dbwrite (MS_DB(ms), NAME(ms, SAMPLE), LINE(ms,1), 1)
end

# MSPPARAM -- Put a line of MULTISPEC parameter data.

procedure mspparam (ms, parameter, line)

pointer	ms				# MSIO descriptor
int	parameter			# Index to parameter array
int	line				# Line to be read

char	reference[SZ_MS_KEY]

bool	is_param_id()

begin
	if (!is_param_id (parameter))
	    call error (MS_ERROR, "Bad parameter identifier")

	call sprintf (reference, SZ_MS_KEY, "%s[%d]")
	    call pargstr (NAME(ms, parameter))
	    call pargi (line)

	call dbwrite (MS_DB(ms), reference, PARAMETER(ms,parameter,1), 1)
end


# MSPGAUSS5 -- Put a line of GAUSS5 parameter data.

procedure mspgauss5 (ms, line)

pointer	ms
int	line

begin
	call mspparam (ms, I0, line)
	call mspparam (ms, X0, line)
	call mspparam (ms, S0, line)
	call mspparam (ms, S1, line)
	call mspparam (ms, S2, line)
end

# MSPFIT -- Put parameter fit data.

procedure mspfit (ms, parameter, spectrum)

pointer	ms				# MSIO descriptor
int	parameter			# Parameter to be put
int	spectrum			# Spectrum to be put

char	reference[SZ_MS_KEY]
pointer	sp, fit

begin
	call smark (sp)
	call salloc (fit, 7 + MS_NSAMPLES(ms), TY_REAL)

	call sprintf (reference, SZ_MS_KEY, "%s[%d]")
	    call pargstr (NAME(ms, parameter))
	    call pargi (spectrum)

	call cvsave (CV(ms, parameter, spectrum), Memr[fit])
	call dbwrite (MS_DB(ms), reference, Memr[fit], 1)

	call sfree (sp)
end


# MSPFITS -- Put parameter fits.

procedure mspfits (ms, parameter)

pointer	ms				# MULTISPEC data structure
int	parameter			# Parameter ID for desired fit

int	i

begin
	do i = 1, MS_NSPECTRA(ms)
	    call mspfit (ms, parameter, i)
end
