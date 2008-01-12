include	"ms.h"

# The procedures in this file deal with the mapping of the
# database names to the MULTISPEC identifiers and relations between the
# identifiers and their meaning.
#
# MSNAMES -- Allocate memory and set name array in MULTISPEC data structure.
# MS_DB_ID -- Associate a database name to the MULTISPEC identifier.
# IS_PARAM_ID -- Test if an identifier refers to a model parameter.
# IS_FIT_ID -- Test if an identifier refers to a curfit parameter fit.
# MS_FIT_ID -- Return fit identifier for specified parameter identifier.
# MS_MODEL_ID -- CL get a model name and map to a MULTISPEC identifier.

# MSNAMES -- Allocate memory and set the name array in MULTISPEC data structure.
#
# The name array maps the integer identifiers with the names in the
# database.  The name array is also allocated if necessary.
# This is the only place where the database names are explicitly known.

procedure msnames (ms)

pointer	ms

begin
	if (MS_NAMES(ms) == NULL)
	    call calloc (MS_NAMES(ms), MS_DB_ENTRIES * (SZ_MS_KEY + 1), TY_CHAR)

	# Set name array mapping the MULTISPEC IDs to the database names.
	call sprintf (NAME(ms, HDR), SZ_MS_KEY, "header")
	call sprintf (NAME(ms, COMMENTS), SZ_MS_KEY, "comments")
	call sprintf (NAME(ms, SAMPLE), SZ_MS_KEY, "samples")
	call sprintf (NAME(ms, I0), SZ_MS_KEY, "i0")
	call sprintf (NAME(ms, X0), SZ_MS_KEY, "x0")
	call sprintf (NAME(ms, X0_FIT), SZ_MS_KEY, "x0 fit")
	call sprintf (NAME(ms, S0), SZ_MS_KEY, "s0")
	call sprintf (NAME(ms, S1), SZ_MS_KEY, "s1")
	call sprintf (NAME(ms, S2), SZ_MS_KEY, "s2")
	call sprintf (NAME(ms, S0_FIT), SZ_MS_KEY, "s0 fit")
	call sprintf (NAME(ms, S1_FIT), SZ_MS_KEY, "s1 fit")
	call sprintf (NAME(ms, S2_FIT), SZ_MS_KEY, "s2 fit")
end


# MS_DB_ID -- Associate a database name to the MULTISPEC identifier.
#
# The input entry name is matched with a database name and the
# MULTISPEC identifier is returned.

int procedure ms_db_id (ms, entry)

pointer	ms
char	entry[ARB]

int	i

bool	streq()

begin
	do i = 1, MS_DB_ENTRIES
	    if (streq (entry, NAME(ms, i)))
		return (i)

	return (0)
end


# IS_PARAM_ID -- Test if an identifier refers to a model parameter.

bool procedure is_param_id (param_id)

int	param_id

begin
	switch (param_id) {
	case X0, I0, S0, S1, S2:
	    return (TRUE)
	default:
	    return (FALSE)
	}
end


# IS_FIT_ID -- Test if an identifier refers to a parameter fit.

bool procedure is_fit_id (fit_id)

int	fit_id

begin
	switch (fit_id) {
	case X0_FIT, S0_FIT, S1_FIT, S2_FIT:
	    return (TRUE)
	default:
	    return (FALSE)
	}
end


# MS_FIT_ID -- Return fit identifier for specified parameter identifier.

int procedure ms_fit_id (param_id)

int	param_id

begin
	switch (param_id) {
	case X0:
	    return (X0_FIT)
	case S0:
	    return (S0_FIT)
	case S1:
	    return (S1_FIT)
	case S2:
	    return (S2_FIT)
	default:
	    return (ERR)
	}
end

# MS_MODEL_ID -- CL get a model name and map to a MULTISPEC identifier.
#
# This procedure isolates the model definitions to protect against
# changes in the model names or the order and choice of identifiers
# in ms.h.

int procedure ms_model_id (param)

char	param[ARB]		# CL parameter name
char	str[SZ_LINE]
int	i, clgwrd()

begin
	i = clgwrd (param, str, SZ_LINE, ",gauss5,smooth,")
	switch (i) {
	case 1:
	    return (GAUSS5)
	case 2:
	    return (SMOOTH)
	}
end
