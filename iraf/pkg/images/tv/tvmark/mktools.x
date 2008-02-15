include <ctype.h>
include "tvmark.h"

#  MK_INIT -- Procedure to initialize the image marking code.

procedure mk_init (mk)

pointer	mk		# pointer to immark structure

begin
	call malloc (mk, LEN_MARKSTRUCT, TY_STRUCT)

	# Initialize the mark type parameters.
	MK_MARK(mk) = EOS
	MK_CSTRING(mk) = EOS
	MK_RSTRING(mk) = EOS
	MK_MKTYPE(mk) = 0
	MK_NCIRCLES(mk) = 0
	MK_NELLIPSES(mk) = 0
	MK_NSQUARES(mk) = 0
	MK_NRECTANGLES(mk) = 0
	MK_NXOFFSET(mk) = 0
	MK_NYOFFSET(mk) = 0

	# Initialize the mark shape parameters.
	MK_RATIO(mk) = 1.0
	MK_ELLIPTICITY(mk) = 0.0
	MK_RTHETA(mk) = 0.0
	MK_ETHETA(mk) = 0.0

	# Initialize the pointers.
	MK_RADII(mk) = NULL
	MK_AXES(mk) = NULL
	MK_SLENGTHS(mk) = NULL
	MK_RLENGTHS(mk) = NULL

	MK_X1(mk) = INDEFI
	MK_Y1(mk) = INDEFI
	MK_X2(mk) = INDEFI
	MK_Y2(mk) = INDEFI

	# Initialize actual drawing parameters.
	MK_NUMBER(mk) = NO
	MK_LABEL(mk) = NO
	MK_FONT(mk) = EOS
	MK_GRAYLEVEL(mk) = 0
	MK_SIZE(mk) = 1
	MK_SZPOINT(mk) = 1

	# Initialize file parameters strings.
	MK_IMAGE(mk) = EOS
	MK_OUTIMAGE(mk) = EOS
	MK_COORDS(mk) = EOS
	MK_DELETIONS(mk) = EOS
	MK_LOGFILE(mk) = EOS
	MK_AUTOLOG(mk) = NO

	# Initilize the display command parameters.
	MK_FRAME(mk) = 1
	MK_TOLERANCE(mk) = 1.0

	# Initialize the buffers.
	call mk_rinit (mk)
end


#  MK_RINIT -- Procedure to initialize the immark structure.

procedure mk_rinit (mk)

pointer	mk		# pointer to immark structure

begin
	call mk_rfree (mk)
	call malloc (MK_RADII(mk), MAX_NMARKS, TY_REAL)
	call malloc (MK_AXES(mk), MAX_NMARKS, TY_REAL)
	call malloc (MK_SLENGTHS(mk), MAX_NMARKS, TY_REAL)
	call malloc (MK_RLENGTHS(mk), MAX_NMARKS, TY_REAL)
end


# MK_INDEFR -- Procedure to reinitialize the size dependent buffers.

procedure mk_indefr (mk)

pointer	mk		# pointer to immark

int	ncircles, nsquares, nellipses, nrectangles
int	mk_stati()

begin
	ncircles = mk_stati (mk, NCIRCLES)
	nellipses = mk_stati (mk, NELLIPSES)
	nsquares = mk_stati (mk, NSQUARES)
	nrectangles = mk_stati (mk, NRECTANGLES)

	if (ncircles > 0)
	    call amovkr (INDEFR, Memr[MK_RADII(mk)], ncircles)
	if (nellipses > 0)
	    call amovkr (INDEFR, Memr[MK_AXES(mk)], nellipses)
	if (nsquares > 0)
	    call amovkr (INDEFR, Memr[MK_SLENGTHS(mk)], nsquares)
	if (nrectangles > 0)
	    call amovkr (INDEFR, Memr[MK_RLENGTHS(mk)], nrectangles)

end


#  MK_REALLOC -- Procedure to reallocate regions buffers.

procedure mk_realloc (mk, ncircles, nellipses, nsquares, nrectangles)

pointer	mk		# pointer to immark structure
int	ncircles	# number of circles
int	nellipses 	# number of ellipses
int	nsquares	# number of squares
int	nrectangles	# number of rectangles

int	nc, ne, ns, nr
int	mk_stati()

begin
	if (ncircles > 0)
	    call realloc (MK_RADII(mk), ncircles, TY_REAL)
	else {
	    call mfree (MK_RADII(mk), TY_REAL)
	    MK_RADII(mk) = NULL
	}

	if (nellipses > 0) 
	    call realloc (MK_AXES(mk), nellipses, TY_REAL)
	else {
	    call mfree (MK_AXES(mk), TY_REAL)
	    MK_AXES(mk) = NULL
	}

	if (nsquares > 0)
	    call realloc (MK_SLENGTHS(mk), nsquares, TY_REAL)
	else {
	    call mfree (MK_SLENGTHS(mk), TY_REAL)
	    MK_SLENGTHS(mk) = NULL
	}

	if (nrectangles > 0)
	    call realloc (MK_RLENGTHS(mk), nrectangles, TY_REAL)
	else {
	    call mfree (MK_RLENGTHS(mk), TY_REAL)
	    MK_RLENGTHS(mk) = NULL
	}

	nc = mk_stati (mk, NCIRCLES)
	ne = mk_stati (mk, NELLIPSES)
	ns = mk_stati (mk, NSQUARES)
	nr = mk_stati (mk, NRECTANGLES)

	if (ncircles > nc)
	    call amovkr (INDEFR, Memr[MK_RADII(mk)+nc], ncircles - nc)
	if (nellipses > ne)
	    call amovkr (INDEFR, Memr[MK_AXES(mk)+ne], nellipses - ne)
	if (nsquares > ns)
	    call amovkr (INDEFR, Memr[MK_SLENGTHS(mk)+ns], nsquares - ns)
	if (nrectangles > nr)
	    call amovkr (INDEFR, Memr[MK_RLENGTHS(mk)+nr], nrectangles - nr)
end


# MK_FREE -- Procedure to free the immark structure.

procedure mk_free (mk)

pointer	mk		# pointer to immark structure

begin
	call mk_rfree (mk)
	call mfree (mk, TY_STRUCT)
end


# MK_RFREE -- Procedure to free the regions portion of the immark structure.

procedure mk_rfree (mk)

pointer	mk		# pointer to immark structure

begin
	if (MK_RADII(mk) != NULL)
	    call mfree (MK_RADII(mk), TY_REAL)
	MK_RADII(mk) = NULL
	if (MK_AXES(mk) != NULL)
	    call mfree (MK_AXES(mk), TY_REAL)
	MK_AXES(mk) = NULL
	if (MK_SLENGTHS(mk) != NULL)
	    call mfree (MK_SLENGTHS(mk), TY_REAL)
	MK_SLENGTHS(mk) = NULL
	if (MK_RLENGTHS(mk) != NULL)
	    call mfree (MK_RLENGTHS(mk), TY_REAL)
	MK_RLENGTHS(mk) = NULL
end


# MK_STATI -- Procedure to fetch the value of an immark integer parameter.

int procedure mk_stati (mk, param)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case AUTOLOG:
	    return (MK_AUTOLOG(mk))
	case NUMBER:
	    return (MK_NUMBER(mk))
	case LABEL:
	    return (MK_LABEL(mk))
	case GRAYLEVEL:
	    return (MK_GRAYLEVEL(mk))
	case SIZE:
	    return (MK_SIZE(mk))
	case SZPOINT:
	    return (MK_SZPOINT(mk))
	case FRAME:
	    return (MK_FRAME(mk))
	case NCIRCLES:
	    return (MK_NCIRCLES(mk))
	case NELLIPSES:
	    return (MK_NELLIPSES(mk))
	case NSQUARES:
	    return (MK_NSQUARES(mk))
	case NRECTANGLES:
	    return (MK_NRECTANGLES(mk))
	case MKTYPE:
	    return (MK_MKTYPE(mk))
	case X1:
	    return (MK_X1(mk))
	case Y1:
	    return (MK_Y1(mk))
	case X2:
	    return (MK_X2(mk))
	case Y2:
	    return (MK_Y2(mk))
	case NXOFFSET:
	    return (MK_NXOFFSET(mk))
	case NYOFFSET:
	    return (MK_NYOFFSET(mk))
	default:
	    call error (0, "MK_STATI: Unknown integer parameter.")
	}
end


# MK_STATP -- Procedure to fetch the value of a pointer parameter.

pointer procedure mk_statp (mk, param)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case RADII:
	    return (MK_RADII(mk))
	case AXES:
	    return (MK_AXES(mk))
	case SLENGTHS:
	    return (MK_SLENGTHS(mk))
	case RLENGTHS:
	    return (MK_RLENGTHS(mk))
	default:
	    call error (0, "MK_STATP: Unknown pointer parameter.")
	}
end


# MK_STATR -- Procedure to fetch the value of a real parameter.

real procedure mk_statr (mk, param)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case RATIO:
	    return (MK_RATIO(mk))
	case ELLIPTICITY:
	    return (MK_ELLIPTICITY(mk))
	case RTHETA:
	    return (MK_RTHETA(mk))
	case ETHETA:
	    return (MK_ETHETA(mk))
	case TOLERANCE:
	    return (MK_TOLERANCE(mk))
	default:
	    call error (0, "MK_STATR: Unknown real parameter.")
	}
end


# MK_STATS -- Procedure to fetch the value of a string parameter.

procedure mk_stats (mk, param, str, maxch)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched
char	str[ARB]	# output string
int	maxch		# maximum number of characters

begin
	switch (param) {
	case IMAGE:
	    call strcpy (MK_IMAGE(mk), str, maxch)
	case OUTIMAGE:
	    call strcpy (MK_OUTIMAGE(mk), str, maxch)
	case COORDS:
	    call strcpy (MK_COORDS(mk), str, maxch)
	case DELETIONS:
	    call strcpy (MK_DELETIONS(mk), str, maxch)
	case LOGFILE:
	    call strcpy (MK_LOGFILE(mk), str, maxch)
	case FONT:
	    call strcpy (MK_FONT(mk), str, maxch)
	case MARK:
	    call strcpy (MK_MARK(mk), str, maxch)
	case CSTRING:
	    call strcpy (MK_CSTRING(mk), str, maxch)
	case RSTRING:
	    call strcpy (MK_RSTRING(mk), str, maxch)
	default:
	    call error (0, "MK_STATS: Unknown string parameter.")
	}
end


# MK_SETI -- Procedure to set the value of an integer parameter.

procedure mk_seti (mk, param, value)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched
int	value		# value of the integer parameter

begin
	switch (param) {
	case AUTOLOG:
	    MK_AUTOLOG(mk) = value
	case NUMBER:
	    MK_NUMBER(mk) = value
	case LABEL:
	    MK_LABEL(mk) = value
	case GRAYLEVEL:
	    MK_GRAYLEVEL(mk) = value
	case SIZE:
	    MK_SIZE(mk) = value
	case SZPOINT:
	    MK_SZPOINT(mk) = value
	case FRAME:
	    MK_FRAME(mk) = value
	case NCIRCLES:
	    MK_NCIRCLES(mk) = value
	case NELLIPSES:
	    MK_NELLIPSES(mk) = value
	case NSQUARES:
	    MK_NSQUARES(mk) = value
	case NRECTANGLES:
	    MK_NRECTANGLES(mk) = value
	case MKTYPE:
	    MK_MKTYPE(mk) = value
	case X1:
	    MK_X1(mk) = value
	case Y1:
	    MK_Y1(mk) = value
	case X2:
	    MK_X2(mk) = value
	case Y2:
	    MK_Y2(mk) = value
	case NXOFFSET:
	    MK_NXOFFSET(mk) = value
	case NYOFFSET:
	    MK_NYOFFSET(mk) = value
	default:
	    call error (0, "MK_SETI: Unknown integer parameter.")
	}
end


# MK_SETP -- Procedure to set the value of a pointer parameter.

procedure mk_setp (mk, param, value)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched
pointer value		# value of the pointer parameter

begin
	switch (param) {
	case RADII:
	    MK_RADII(mk) = value
	case AXES:
	    MK_AXES(mk) = value
	case SLENGTHS:
	    MK_SLENGTHS(mk) = value
	case RLENGTHS:
	    MK_RLENGTHS(mk) = value
	default:
	    call error (0, "MK_SETP: Unknown pointer parameter.")
	}
end


# MK_SETR -- Procedure to set the value of a real parameter.

procedure mk_setr (mk, param, value)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched
real	value		# real parameter

begin
	switch (param) {
	case RATIO:
	    MK_RATIO(mk) = value
	case ELLIPTICITY:
	    MK_ELLIPTICITY(mk) = value
	case RTHETA:
	    MK_RTHETA(mk) = value
	case ETHETA:
	    MK_ETHETA(mk) = value
	case TOLERANCE:
	    MK_TOLERANCE(mk) = value
	default:
	    call error (0, "MK_SETR: Unknown real parameter.")
	}
end


# MK_SETS -- Procedure to set the value of a string parameter.

procedure mk_sets (mk, param, str)

pointer	mk		# pointer to immark structure
int	param		# parameter to be fetched
char	str[ARB]	# output string

int	rp, ntemp
pointer	sp, rtemp
int	fnldir(), mk_gmarks()

begin
	switch (param) {
	case IMAGE:
	    call strcpy (str, MK_IMAGE(mk), SZ_FNAME)

	case OUTIMAGE:
	    call strcpy (str, MK_OUTIMAGE(mk), SZ_FNAME)

	case COORDS:
	    rp = fnldir (str, MK_COORDS(mk), SZ_FNAME)
	    call strcpy (str[rp+1], MK_COORDS(mk), SZ_FNAME)

	case DELETIONS:
	    rp = fnldir (str, MK_DELETIONS(mk), SZ_FNAME)
	    call strcpy (str[rp+1], MK_DELETIONS(mk), SZ_FNAME)

	case LOGFILE:
	    rp = fnldir (str, MK_LOGFILE(mk), SZ_FNAME)
	    call strcpy (str[rp+1], MK_LOGFILE(mk), SZ_FNAME)

	case FONT:
	    rp = fnldir (str, MK_FONT(mk), SZ_FNAME)
	    call strcpy (str[rp+1], MK_FONT(mk), SZ_FNAME)

	case MARK:
	    call strcpy (str, MK_MARK(mk), SZ_FNAME)

	case CSTRING:
	    call smark (sp)
	    call salloc (rtemp, MAX_NMARKS, TY_REAL)
	    ntemp = mk_gmarks (str, Memr[rtemp], MAX_NMARKS)
	    if (ntemp > 0) {
	        call strcpy (str, MK_CSTRING(mk), SZ_FNAME)
		MK_NCIRCLES(mk) = ntemp
		call realloc (MK_RADII(mk), ntemp, TY_REAL)
		call amovr (Memr[rtemp], Memr[MK_RADII(mk)], ntemp)
		call asrtr (Memr[MK_RADII(mk)], Memr[MK_RADII(mk)], ntemp)
	    }
	    call sfree (sp)

	case RSTRING:
	    call smark (sp)
	    call salloc (rtemp, MAX_NMARKS, TY_REAL)
	    ntemp = mk_gmarks (str, Memr[rtemp], MAX_NMARKS)
	    if (ntemp > 0) {
	        call strcpy (str, MK_RSTRING(mk), SZ_FNAME)
		MK_NRECTANGLES(mk) = ntemp
		call realloc (MK_RLENGTHS(mk), ntemp, TY_REAL)
		call amovr (Memr[rtemp], Memr[MK_RLENGTHS(mk)], ntemp)
		call asrtr (Memr[MK_RLENGTHS(mk)], Memr[MK_RLENGTHS(mk)], ntemp)
	    }
	    call sfree (sp)

	default:
	    call error (0, "MK_SETS: Unknown string parameter.")
	}
end
