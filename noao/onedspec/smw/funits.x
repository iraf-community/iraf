include	<ctype.h>
include	<error.h>
include	<funits.h>


# FUN_OPEN -- Open funits package
# It is allowed to open an unknown funit type

pointer procedure fun_open (funits)

char	funits[ARB]		# Units string
pointer	fun			# Units pointer returned

begin
	call calloc (fun, FUN_LEN, TY_STRUCT)
	iferr (call fun_decode (fun, funits)) {
	    call fun_close (fun)
	    call erract (EA_ERROR)
	}
	return (fun)
end


# FUN_CLOSE -- Close funits package

procedure fun_close (fun)

pointer	fun			# Units pointer

begin
	call mfree (fun, TY_STRUCT)
end


# FUN_COPY -- Copy funits pointer

procedure fun_copy (fun1, fun2)

pointer	fun1, fun2		# Units pointers

begin
	if (fun2 == NULL)
	    call malloc (fun2, FUN_LEN, TY_STRUCT)
	call amovi (Memi[fun1], Memi[fun2], FUN_LEN)
end


# FUN_DECODE -- Decode funits string and set up funits structure.
# The main work is done in FUN_DECODE1 so that the funits string may
# be recursive; i.e. the funits string may contain other funits strings.

procedure fun_decode (fun, funits)

pointer	fun			# Units pointer
char	funits[ARB]		# Units string

bool	streq()
pointer	sp, funits1, temp
errchk	fun_decode1, fun_ctranr

begin
	if (streq (funits, FUN_USER(fun)))
	    return

	call smark (sp)
	call salloc (funits1, SZ_LINE, TY_CHAR)
	call salloc (temp, FUN_LEN, TY_STRUCT)

	# Save a copy to restore in case of an error.
	call fun_copy (fun, temp)

	iferr (call fun_decode1 (fun, funits, Memc[funits1], SZ_LINE)) {
	    call fun_copy (temp, fun)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end


# FUN_DECODE1 -- Decode funits string and set up funits structure.
# Unknown funit strings are allowed.

procedure fun_decode1 (fun, funits, funits1, sz_funits1)

pointer	fun			# Units pointer
char	funits[ARB]		# Units string
char	funits1[sz_funits1]	# Secondary funits string to return
int	sz_funits1		# Size of secondary funits string

int	funmod, funtype
int	i, j, k, nscan(), strdic(), strlen()
real	funscale
pointer	sp, str

int	class[FUN_NUNITS]
real	scale[FUN_NUNITS]
data	class /FUN_FREQ,FUN_FREQ,FUN_FREQ,FUN_WAVE/
data	scale /FUN_J,FUN_FU,FUN_CGSH,FUN_CGSA/

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call strcpy (funits, Memc[str], SZ_FNAME)
	call strlwr (Memc[str])
	call sscan (Memc[str])
	funtype = 0
	funmod = 0
	do i = 1, 2 {
	    call gargwrd (Memc[str], SZ_FNAME)
	    if (nscan() != i)
		break

	    j = strdic (Memc[str], Memc[str], SZ_FNAME, FUN_DIC)
	    for (k=strlen(Memc[str]); k>0 &&
		(IS_WHITE(Memc[str+k-1]) || Memc[str+k-1]=='\n'); k=k-1)
		Memc[str+k-1] = EOS

	    if (j > FUN_NUNITS) {
		if (funmod != 0)
		    break
		funmod = j - FUN_NUNITS
	    } else {
		funtype = j
		break
	    }
	}
	i = nscan()
	call gargr (funscale)
	if (nscan() != i+1)
	    funscale = 1

	if (funtype == 0) {
	    FUN_TYPE(fun) = 0
	    FUN_CLASS(fun) = FUN_UNKNOWN
	    FUN_LABEL(fun) = EOS
	    call strcpy (funits, FUN_UNITS(fun), SZ_UNITS)
	} else {
	    FUN_TYPE(fun) = funtype
	    FUN_CLASS(fun) = class[funtype]
	    FUN_MOD(fun) = funmod
	    FUN_SCALE(fun) = scale[funtype] * funscale
	    FUN_LABEL(fun) = EOS
	    FUN_UNITS(fun) = EOS
	    call strcpy (funits, FUN_USER(fun), SZ_UNITS)
	    switch (funmod) {
	    case FUN_LOG:
		call strcat ("Log ", FUN_LABEL(fun), SZ_UNITS)
	    case FUN_MAG:
		call strcat ("Mag ", FUN_LABEL(fun), SZ_UNITS)
	    }
	    call strcat ("Flux", FUN_LABEL(fun), SZ_UNITS)
	    if (funscale != 1) {
		call sprintf (FUN_UNITS(fun), SZ_UNITS, "%sx%.1g")
		    call pargstr (Memc[str])
		    call pargr (funscale)
	    } else {
		call sprintf (FUN_UNITS(fun), SZ_UNITS, "%s")
		    call pargstr (Memc[str])
	    }
	}

	call sfree (sp)
end


# FUN_COMPARE -- Compare two funits

bool procedure fun_compare (fun1, fun2)

pointer	fun1, fun2		# Units pointers to compare
bool	strne()

begin
	if (strne (FUN_UNITS(fun1), FUN_UNITS(fun2)))
	    return (false)
	if (strne (FUN_LABEL(fun1), FUN_LABEL(fun2)))
	    return (false)
	return (true)
end


# FUN_CTRANR -- Transform funits
# Error is returned if the transform cannot be made

procedure fun_ctranr (fun1, fun2, dun, dval, fval1, fval2, nvals)

pointer	fun1			# Input funits pointer
pointer	fun2			# Output funits pointer
pointer	dun			# Input units pointer
real	dval[nvals]		# Input dispersion values
real	fval1[nvals]		# Input flux values
real	fval2[nvals]		# Output flux values
int	nvals			# Number of values

int	i
real	s, lambda
pointer	ang, un_open()
bool	fun_compare()
errchk	un_open, un_ctranr

begin
	if (fun_compare (fun1, fun2)) {
	    call amovr (fval1, fval2, nvals)
	    return
	}

	if (FUN_CLASS(fun1) == FUN_UNKNOWN || FUN_CLASS(fun2) == FUN_UNKNOWN)
	    call error (1, "Cannot convert between selected funits")

	call amovr (fval1, fval2, nvals)

	s = FUN_SCALE(fun1)
	switch (FUN_MOD(fun1)) {
	case FUN_LOG:
	    do i = 1, nvals
		fval2[i] = 10. ** fval2[i]
	case FUN_MAG:
	    do i = 1, nvals
		fval2[i] = 10. ** (-0.4 * fval2[i])
	}
	switch (FUN_CLASS(fun1)) {
	case FUN_FREQ:
	    do i = 1, nvals
		fval2[i] = fval2[i] / s
	case FUN_WAVE:
	    if (FUN_CLASS(fun2) != FUN_WAVE) {
		s = s * FUN_VLIGHT
		ang = un_open ("angstroms")
		do i = 1, nvals {
		    call un_ctranr (dun, ang, dval[i], lambda, 1)
		    fval2[i] = fval2[i] / s * lambda**2
		}
		call un_close (ang)
	    } else {
		do i = 1, nvals
		    fval2[i] = fval2[i] / s
	    }
	}

	s = FUN_SCALE(fun2)
	switch (FUN_CLASS(fun2)) {
	case FUN_FREQ:
	    do i = 1, nvals
		fval2[i] = fval2[i] * s
	case FUN_WAVE:
	    if (FUN_CLASS(fun1) != FUN_WAVE) {
		s = s * FUN_VLIGHT
		ang = un_open ("angstroms")
		do i = 1, nvals {
		    call un_ctranr (dun, ang, dval[i], lambda, 1)
		    fval2[i] = fval2[i] * s / lambda**2
		}
		call un_close (ang)
	    } else {
		do i = 1, nvals
		    fval2[i] = fval2[i] * s
	    }
	}
	switch (FUN_MOD(fun2)) {
	case FUN_LOG:
	    do i = 1, nvals
		fval2[i] = log10 (fval2[i])
	case FUN_MAG:
	    do i = 1, nvals
		fval2[i] = -2.5 * log10 (fval2[i])
	}
end


# FUN_CHANGER -- Change funits
# Error is returned if the conversion cannot be made

procedure fun_changer (fun, funits, dun, dvals, fvals, nvals, update)

pointer	fun			# Units pointer (may be changed)
char	funits[ARB]		# Desired funits
pointer	dun			# Dispersion units pointer
real	dvals[nvals]		# Dispersion values
real	fvals[nvals]		# Flux Values
int	nvals			# Number of values
int	update			# Update funits pointer?

bool	streq(), fun_compare()
pointer	fun1, fun_open()
errchk	fun_open, fun_ctranr

begin

	# Check for same funit string
	if (streq (funits, FUN_USER(fun)))
	    return

	# Check for error in funits string, or the same funits.
	fun1 = fun_open (funits)
	if (fun_compare (fun1, fun)) {
	    call strcpy (funits, FUN_USER(fun), SZ_UNITS)
	    call fun_close (fun1)
	    return
	}

	iferr {
	    call fun_ctranr (fun, fun1, dun, dvals, fvals, fvals, nvals)
	    if (update == YES)
		call fun_copy (fun1, fun)
	    call fun_close(fun1)
	} then {
	    call fun_close(fun1)
	    call erract (EA_ERROR)
	}
end


# FUN_CTRAND -- Transform funits
# Error is returned if the transform cannot be made

procedure fun_ctrand (fun1, fun2, dun, dval, fval1, fval2, nvals)

pointer	fun1			# Input funits pointer
pointer	fun2			# Output funits pointer
pointer	dun			# Input dispersion units pointer
double	dval[nvals]		# Input dispersion values
double	fval1[nvals]		# Input flux values
double	fval2[nvals]		# Output flux values
int	nvals			# Number of values

int	i
double	s, lambda
pointer	ang, un_open()
bool	fun_compare()
errchk	un_open, un_ctrand

begin
	if (fun_compare (fun1, fun2)) {
	    call amovd (fval1, fval2, nvals)
	    return
	}

	if (FUN_CLASS(fun1) == FUN_UNKNOWN || FUN_CLASS(fun2) == FUN_UNKNOWN)
	    call error (1, "Cannot convert between selected funits")

	call amovd (fval1, fval2, nvals)

	s = FUN_SCALE(fun1)
	switch (FUN_MOD(fun1)) {
	case FUN_LOG:
	    do i = 1, nvals
		fval2[i] = 10. ** fval2[i]
	case FUN_MAG:
	    do i = 1, nvals
		fval2[i] = 10. ** (-0.4 * fval2[i])
	}
	switch (FUN_CLASS(fun1)) {
	case FUN_FREQ:
	    do i = 1, nvals
		fval2[i] = fval2[i] / s
	case FUN_WAVE:
	    if (FUN_CLASS(fun2) != FUN_WAVE) {
		s = s * FUN_VLIGHT
		ang = un_open ("angstroms")
		do i = 1, nvals {
		    call un_ctrand (dun, ang, dval[i], lambda, 1)
		    fval2[i] = fval2[i] / s * lambda**2
		}
		call un_close (ang)
	    } else {
		do i = 1, nvals
		    fval2[i] = fval2[i] / s
	    }
	}

	s = FUN_SCALE(fun2)
	switch (FUN_CLASS(fun2)) {
	case FUN_FREQ:
	    do i = 1, nvals
		fval2[i] = fval2[i] * s
	case FUN_WAVE:
	    if (FUN_CLASS(fun1) != FUN_WAVE) {
		s = s * FUN_VLIGHT
		ang = un_open ("angstroms")
		do i = 1, nvals {
		    call un_ctrand (dun, ang, dval[i], lambda, 1)
		    fval2[i] = fval2[i] * s / lambda**2
		}
		call un_close (ang)
	    } else {
		do i = 1, nvals
		    fval2[i] = fval2[i] * s
	    }
	}
	switch (FUN_MOD(fun2)) {
	case FUN_LOG:
	    do i = 1, nvals
		fval2[i] = log10 (fval2[i])
	case FUN_MAG:
	    do i = 1, nvals
		fval2[i] = -2.5 * log10 (fval2[i])
	}

end


# FUN_CHANGED -- Change funits
# Error is returned if the conversion cannot be made

procedure fun_changed (fun, funits, dun, dvals, fvals, nvals, update)

pointer	fun			# Units pointer (may be changed)
char	funits[ARB]		# Desired funits
pointer	dun			# Input dispersion pointer
double	dvals[nvals]		# Input dispersion values
double	fvals[nvals]		# Flux values
int	nvals			# Number of values
int	update			# Update funits pointer?

bool	streq(), fun_compare()
pointer	fun1, fun_open()
errchk	fun_open, fun_ctrand

begin

	# Check for same funit string
	if (streq (funits, FUN_USER(fun)))
	    return

	# Check for error in funits string, or the same funits.
	fun1 = fun_open (funits)
	if (fun_compare (fun1, fun)) {
	    call strcpy (funits, FUN_USER(fun), SZ_UNITS)
	    call fun_close (fun1)
	    return
	}

	iferr {
	    call fun_ctrand (fun, fun1, dun, dvals, fvals, fvals, nvals)
	    if (update == YES)
		call fun_copy (fun1, fun)
	    call fun_close(fun1)
	} then {
	    call fun_close(fun1)
	    call erract (EA_ERROR)
	}
end
