include	<ctype.h>
include	<error.h>
include	<units.h>


# UN_OPEN -- Open units package
# It is allowed to open an unknown unit type

pointer procedure un_open (units)

char	units[ARB]		# Units string
pointer	un			# Units pointer returned

begin
	call calloc (un, UN_LEN, TY_STRUCT)
	iferr (call un_decode (un, units)) {
	    call un_close (un)
	    call erract (EA_ERROR)
	}
	return (un)
end


# UN_CLOSE -- Close units package

procedure un_close (un)

pointer	un			# Units pointer

begin
	call mfree (un, TY_STRUCT)
end


# UN_COPY -- Copy units pointer

procedure un_copy (un1, un2)

pointer	un1, un2		# Units pointers

begin
	if (un2 == NULL)
	    call malloc (un2, UN_LEN, TY_STRUCT)
	call amovi (Memi[un1], Memi[un2], UN_LEN)
end


# UN_DECODE -- Decode units string and set up units structure.
# The main work is done in UN_DECODE1 so that the units string may
# be recursive; i.e. the units string may contain other units strings.
# In particular, this is required for the velocity units to specify
# a reference wavelength.

procedure un_decode (un, units)

pointer	un			# Units pointer
char	units[ARB]		# Units string

bool	streq()
pointer	sp, units1, temp, un1, un2
errchk	un_decode1, un_ctranr

begin
	if (streq (units, UN_USER(un)))
	    return

	call smark (sp)
	call salloc (units1, SZ_LINE, TY_CHAR)
	call salloc (temp, UN_LEN, TY_STRUCT)

	# Save a copy to restore in case of an error.
	call un_copy (un, temp)

	iferr {
	    # Decode the primary units
	    call un_decode1 (un, units, Memc[units1], SZ_LINE)

	    # Decode velocity reference wavelength if necessary.
	    if (UN_CLASS(un) == UN_VEL || UN_CLASS(un) == UN_DOP) {
		call salloc (un1, UN_LEN, TY_STRUCT)
		call un_decode1 (un1, Memc[units1], Memc[units1], SZ_LINE)
		if (UN_CLASS(un1) == UN_VEL || UN_CLASS(un1) == UN_DOP)
		    call error (1,
			"Velocity reference units may not be velocity")
		call salloc (un2, UN_LEN, TY_STRUCT)
		call un_decode1 (un2, "angstroms", Memc[units1], SZ_LINE)
		call un_ctranr (un1, un2, UN_VREF(un), UN_VREF(un), 1)
	    }
	} then {
	    call un_copy (temp, un)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end


# UN_DECODE1 -- Decode units string and set up units structure.
# Return any secondary units string.  Unknown unit strings are allowed.

procedure un_decode1 (un, units, units1, sz_units1)

pointer	un			# Units pointer
char	units[ARB]		# Units string
char	units1[sz_units1]	# Secondary units string to return
int	sz_units1		# Size of secondary units string

int	unlog, uninv, untype
int	i, j, k, nscan(), strdic(), strlen()
pointer	sp, str
pointer	stp, sym, stfind(), strefsbuf()

int	class[UN_NUNITS]
real	scale[UN_NUNITS]
data	stp/NULL/
data	class /UN_WAVE,UN_WAVE,UN_WAVE,UN_WAVE,UN_WAVE,UN_WAVE,UN_WAVE,
		UN_FREQ,UN_FREQ,UN_FREQ,UN_FREQ,UN_VEL,UN_VEL,
		UN_ENERGY,UN_ENERGY,UN_ENERGY,UN_DOP/
data	scale /UN_ANG,UN_NM,UN_MMIC,UN_MIC,UN_MM,UN_CM,UN_M,UN_HZ,UN_KHZ,
		UN_MHZ,UN_GHZ,UN_MPS,UN_KPS,UN_EV,UN_KEV,UN_MEV,UN_Z/

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	iferr (call un_abbr (stp))
	    ;

	call strcpy (units, Memc[str], SZ_FNAME)
	if (stp != NULL) {
	    sym = stfind (stp, Memc[str])
	    if (sym != NULL)
		call strcpy (Memc[strefsbuf(stp,Memi[sym])],
		    Memc[str], SZ_FNAME)
	}
	call strlwr (Memc[str])
	call sscan (Memc[str])
	untype = 0
	unlog = NO
	uninv = NO
	do i = 1, 3 {
	    call gargwrd (Memc[str], SZ_FNAME)
	    if (nscan() != i)
		break

	    j = strdic (Memc[str], Memc[str], SZ_FNAME, UN_DIC)
	    for (k=strlen(Memc[str]); k>0 &&
		(IS_WHITE(Memc[str+k-1]) || Memc[str+k-1]=='\n'); k=k-1)
		Memc[str+k-1] = EOS

	    if (j > UN_NUNITS) {
	        j = j - UN_NUNITS
	        if (j == 1) {
		    if (unlog == YES)
			break
		    unlog = YES
		} else if (j == 2) {
		    if (uninv == YES)
			break
		    uninv = YES
		}
	    } else {
		if (class[j] == UN_VEL || class[j] == UN_DOP) {
		    call gargr (UN_VREF(un))
		    call gargstr (units1, sz_units1)
		    if (nscan() != i+2)
			call error (1, "Error in velocity reference wavelength")
		} else
		    UN_VREF(un) = 0.
		untype = j
		break
	    }
	}

	if (untype == 0) {
	    UN_TYPE(un) = 0
	    UN_CLASS(un) = UN_UNKNOWN
	    UN_LABEL(un) = EOS
	    call strcpy (units, UN_UNITS(un), SZ_UNITS)
	} else {
	    UN_TYPE(un) = untype
	    UN_CLASS(un) = class[untype]
	    UN_LOG(un) = unlog
	    UN_INV(un) = uninv
	    UN_SCALE(un) = scale[untype]
	    UN_LABEL(un) = EOS
	    UN_UNITS(un) = EOS
	    call strcpy (units, UN_USER(un), SZ_UNITS)

	    if (unlog == YES)
		call strcat ("Log ", UN_LABEL(un), SZ_UNITS)
	    if (uninv == YES)
		call strcat ("inverse ", UN_UNITS(un), SZ_UNITS)
	    call strcat (Memc[str], UN_UNITS(un), SZ_UNITS)
	    switch (class[j]) {
	    case UN_WAVE:
		if (uninv == NO)
		    call strcat ("Wavelength", UN_LABEL(un), SZ_UNITS)
		else
		    call strcat ("Wavenumber", UN_LABEL(un), SZ_UNITS)
	    case UN_FREQ:
		call strcat ("Frequency", UN_LABEL(un), SZ_UNITS)
	    case UN_VEL:
		call strcat ("Velocity", UN_LABEL(un), SZ_UNITS)
	    case UN_ENERGY:
		call strcat ("Energy", UN_LABEL(un), SZ_UNITS)
	    case UN_DOP:
		call strcat ("Redshift", UN_LABEL(un), SZ_UNITS)
	    }
	}

	call sfree (sp)
end


# UN_COMPARE -- Compare two units

bool procedure un_compare (un1, un2)

pointer	un1, un2		# Units pointers to compare
bool	strne()

begin
	if (strne (UN_UNITS(un1), UN_UNITS(un2)))
	    return (false)
	if (strne (UN_LABEL(un1), UN_LABEL(un2)))
	    return (false)
	if (UN_VREF(un1) != UN_VREF(un2))
	    return (false)
	return (true)
end


# UN_CTRANR -- Transform units
# Error is returned if the transform cannot be made

procedure un_ctranr (un1, un2, val1, val2, nvals)

pointer	un1			# Input units pointer
pointer	un2			# Output units pointer
real	val1[nvals]		# Input values
real	val2[nvals]		# Output values
int	nvals			# Number of values

int	i
real	s, v, z
bool	un_compare()

begin
	if (un_compare (un1, un2)) {
	    call amovr (val1, val2, nvals)
	    return
	}

	if (UN_CLASS(un1) == UN_UNKNOWN || UN_CLASS(un2) == UN_UNKNOWN)
	    call error (1, "Cannot convert between selected units")

	call amovr (val1, val2, nvals)

	s = UN_SCALE(un1)
	if (UN_LOG(un1) == YES)
	    do i = 1, nvals
		val2[i] = 10. ** val2[i]
	if (UN_INV(un1) == YES)
	    do i = 1, nvals
		val2[i] = 1. / val2[i]
	switch (UN_CLASS(un1)) {
	case UN_WAVE:
	    do i = 1, nvals
		val2[i] = val2[i] / s
	case UN_FREQ:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_VEL:
	    v = UN_VREF(un1)
	    do i = 1, nvals {
		z = val2[i] / s
		val2[i] = sqrt ((1 + z) / (1 - z)) * v
	    }
	case UN_ENERGY:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_DOP:
	    v = UN_VREF(un1)
	    do i = 1, nvals
		val2[i] = (val2[i] / s + 1) * v
	}

	s = UN_SCALE(un2)
	switch (UN_CLASS(un2)) {
	case UN_WAVE:
	    do i = 1, nvals
		val2[i] = val2[i] * s
	case UN_FREQ:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_VEL:
	    v = UN_VREF(un2)
	    do i = 1, nvals {
		z = (val2[i] / v) ** 2
		val2[i] = (z - 1) / (z + 1) * s
	    }
	case UN_ENERGY:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_DOP:
	    v = UN_VREF(un2)
	    do i = 1, nvals
		val2[i] = (val2[i] / v - 1) * s
	}
	if (UN_INV(un2) == YES)
	    do i = 1, nvals
		val2[i] = 1. / val2[i]
	if (UN_LOG(un2) == YES)
	    do i = 1, nvals
		val2[i] = log10 (val2[i])
end


# UN_CHANGER -- Change units
# Error is returned if the conversion cannot be made

procedure un_changer (un, units, vals, nvals, update)

pointer	un			# Units pointer (may be changed)
char	units[ARB]		# Desired units
real	vals[nvals]		# Values
int	nvals			# Number of values
int	update			# Update units pointer?

bool	streq(), un_compare()
pointer	un1, un_open()
errchk	un_open, un_ctranr

begin

	# Check for same unit string
	if (streq (units, UN_USER(un)))
	    return

	# Check for error in units string, or the same units.
	un1 = un_open (units)
	if (un_compare (un1, un)) {
	    call strcpy (units, UN_USER(un), SZ_UNITS)
	    call un_close (un1)
	    return
	}

	iferr {
	    call un_ctranr (un, un1, vals, vals, nvals)
	    if (update == YES)
		call un_copy (un1, un)
	    call un_close(un1)
	} then {
	    call un_close(un1)
	    call erract (EA_ERROR)
	}
end


# UN_CTRAND -- Transform units
# Error is returned if the transform cannot be made

procedure un_ctrand (un1, un2, val1, val2, nvals)

pointer	un1			# Input units pointer
pointer	un2			# Output units pointer
double	val1[nvals]		# Input values
double	val2[nvals]		# Output values
int	nvals			# Number of values

int	i
double	s, v, z
bool	un_compare()

begin
	if (un_compare (un1, un2)) {
	    call amovd (val1, val2, nvals)
	    return
	}

	if (UN_CLASS(un1) == UN_UNKNOWN || UN_CLASS(un2) == UN_UNKNOWN)
	    call error (1, "Cannot convert between selected units")

	call amovd (val1, val2, nvals)

	s = UN_SCALE(un1)
	if (UN_LOG(un1) == YES)
	    do i = 1, nvals
		val2[i] = 10. ** val2[i]
	if (UN_INV(un1) == YES)
	    do i = 1, nvals
		val2[i] = 1. / val2[i]
	switch (UN_CLASS(un1)) {
	case UN_WAVE:
	    do i = 1, nvals
		val2[i] = val2[i] / s
	case UN_FREQ:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_VEL:
	    v = UN_VREF(un1)
	    do i = 1, nvals {
		z = val2[i] / s
		val2[i] = sqrt ((1 + z) / (1 - z)) * v
	    }
	case UN_ENERGY:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_DOP:
	    v = UN_VREF(un1)
	    do i = 1, nvals
		val2[i] = (val2[i] / s + 1) * v
	}

	s = UN_SCALE(un2)
	switch (UN_CLASS(un2)) {
	case UN_WAVE:
	    do i = 1, nvals
		val2[i] = val2[i] * s
	case UN_FREQ:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_VEL:
	    v = UN_VREF(un2)
	    do i = 1, nvals {
		z = (val2[i] / v) ** 2
		val2[i] = (z - 1) / (z + 1) * s
	    }
	case UN_ENERGY:
	    do i = 1, nvals
		val2[i] = s / val2[i]
	case UN_DOP:
	    v = UN_VREF(un2)
	    do i = 1, nvals
		val2[i] = (val2[i] / v - 1) * s
	}
	if (UN_INV(un2) == YES)
	    do i = 1, nvals
		val2[i] = 1. / val2[i]
	if (UN_LOG(un2) == YES)
	    do i = 1, nvals
		val2[i] = log10 (val2[i])
end


# UN_CHANGED -- Change units
# Error is returned if the conversion cannot be made

procedure un_changed (un, units, vals, nvals, update)

pointer	un			# Units pointer (may be changed)
char	units[ARB]		# Desired units
double	vals[nvals]		# Values
int	nvals			# Number of values
int	update			# Update units pointer?

bool	streq(), un_compare()
pointer	un1, un_open()
errchk	un_open, un_ctrand

begin

	# Check for same unit string
	if (streq (units, UN_USER(un)))
	    return

	# Check for error in units string, or the same units.
	un1 = un_open (units)
	if (un_compare (un1, un)) {
	    call strcpy (units, UN_USER(un), SZ_UNITS)
	    call un_close (un1)
	    return
	}

	iferr {
	    call un_ctrand (un, un1, vals, vals, nvals)
	    if (update == YES)
		call un_copy (un1, un)
	    call un_close(un1)
	} then {
	    call un_close(un1)
	    call erract (EA_ERROR)
	}
end


# UN_ABBR -- Load abbreviations into a symbol table.

procedure un_abbr (stp)

pointer	stp		#U Symbol table

int	fd, open(), fscan(), nscan(), stpstr()
pointer	sp, key, val
pointer	sym, stopen(), stfind(), stenter(), strefsbuf()
errchk	open

begin
	if (stp != NULL)
	    return

	fd = open (ABBREVIATIONS, READ_ONLY, TEXT_FILE)
	stp = stopen ("unabbr", 20, 20, 40*SZ_LINE)

	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)

	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[key], SZ_LINE)
	    call gargwrd (Memc[val], SZ_LINE)
	    if (nscan() != 2)
		next
	    if (Memc[key] == '#')
		next

	    sym = stfind (stp, Memc[key])
	    if (sym == NULL) {
		sym = stenter (stp, Memc[key], 1)
		Memi[sym] = stpstr (stp, Memc[val], SZ_LINE)
	    } else
		call strcpy (Memc[val], Memc[strefsbuf(stp,Memi[sym])], SZ_LINE)
	}

	call close (fd)
	call sfree (sp)
end
