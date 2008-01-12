include "tunits.h"

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# ABREV_UNSTR -- Replace units string with its abbreviation

procedure abrev_unstr (ab, instr, outstr, maxch)

pointer	ab		# i: abbreviation hash descriptor
char	instr[ARB]	# i: string to be abbreviated
char	outstr[ARB]	# o: abbreviated string
int	maxch		# i: max length of abbreviated string
#--
int	nc
pointer	sp, temp

int	gstrcpy(), find_abrev ()

begin
	if (find_abrev (ab, instr, outstr, maxch) == YES)
	    return

	call smark (sp)
	call salloc (temp, LEN_UNIT, TY_CHAR)

	nc = gstrcpy (instr, Memc[temp], LEN_UNIT)
	if (nc == 1 || instr[nc] != 's') {
	    call strcpy (instr, outstr, maxch)

	} else {
	    Memc[temp+nc-1] = EOS
	    if (find_abrev (ab, Memc[temp], outstr, maxch) == NO)
		call strcpy (Memc[temp], outstr, maxch)
	}

	call sfree (sp)
end

# COPY_UNSTR -- Copy a units descriptor

pointer procedure copy_unstr (punit1)

pointer	punit1		# i: units descriptor to be copied
#--
int	idx
pointer	punit2

begin
	# Allocate structure to hold units

	call calloc (punit2, LEN_TUNSTRUCT, TY_INT)

	# Copy numeric factor

	TUN_FACTOR(punit2) = TUN_FACTOR(punit1)

	# Copy units and their powers

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit1,idx) == NULL)
		break

	    call malloc (TUN_UNPTR(punit2,idx), LEN_UNIT, TY_CHAR)
	    call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit2,idx), 
			 LEN_UNIT)

	    TUN_POWER(punit2,idx) = TUN_POWER(punit1,idx)
	}

	return (punit2)
end
# DIV_UNSTR -- Divide one set of units by another

pointer procedure div_unstr (punit1, punit2)

pointer	punit1		# i: descriptor for first set of units
pointer	punit2		# i: descriptor for second set of units
#--
int	idx, jdx, kdx, power
pointer	punit3

int	find_unstr()

begin
	# Allocate structure to hold units

	call calloc (punit3, LEN_TUNSTRUCT, TY_INT)

	# Compute the new factor

	TUN_FACTOR(punit3) = TUN_FACTOR(punit1) / TUN_FACTOR(punit2)

	# Find units in both descriptors and subtract their powers

	jdx = 1
	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit1,idx) == NULL)
		break

	    kdx = find_unstr (punit2, TUN_UNITS(punit1, idx))
	    if (kdx == 0)
		next

	    power = TUN_POWER(punit1,idx) - TUN_POWER(punit2,kdx)
	    if (power == 0)
		next

	    call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
	    call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit3,jdx), 
			 LEN_UNIT)

	    TUN_POWER(punit3,jdx) = power
	    jdx = jdx + 1
	    
	}

	# Find units only in a single descriptor and add them to the
	# new descriptor

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit1,idx) == NULL)
		break

	    if (find_unstr (punit2, TUN_UNITS(punit1, idx)) == 0) {
		call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
		call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit3,jdx), 
			     LEN_UNIT)

		TUN_POWER(punit3,jdx) = TUN_POWER(punit1,idx)
		jdx = jdx + 1
	    }
	}

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit2,idx) == NULL)
		break

	    if (find_unstr (punit1, TUN_UNITS(punit2, idx)) == 0) {
		call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
		call strcpy (TUN_UNITS(punit2,idx), TUN_UNITS(punit3,jdx), 
			     LEN_UNIT)

		TUN_POWER(punit3,jdx) = - TUN_POWER(punit2,idx)
		jdx = jdx + 1
	    }
	}

	return (punit3)
end

# FIND_UNSTR -- Find location of units string in descriptor

int procedure find_unstr (punit, units)

pointer	punit		# i: units descriptor
char	units[ARB]	# i: units string to search for
#--
int	idx
bool	streq()

begin
	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit,idx) == NULL)
		break

	    if (streq (TUN_UNITS(punit,idx), units))
		return (idx)
	}

	return (0)
end

# FREE_UNSTR -- Release memory used by a units descriptor

procedure free_unstr (punit)

pointer	punit		# i: units descriptor
#--
int	idx

begin
	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit,idx) == NULL)
		break

	    call mfree (TUN_UNPTR(punit,idx), TY_CHAR)
	}

	call mfree (punit, TY_INT)
end

# MUL_UNSTR -- Multiply two sets of units together

pointer procedure mul_unstr (punit1, punit2)

pointer	punit1		# i: descriptor for first set of units
pointer	punit2		# i: descriptor for second set of units
#--
int	idx, jdx, kdx, power
pointer	punit3

int	find_unstr()

begin
	# Allocate structure to hold units

	call calloc (punit3, LEN_TUNSTRUCT, TY_INT)

	# Compute the new factor

	TUN_FACTOR(punit3) = TUN_FACTOR(punit1) * TUN_FACTOR(punit2)

	# Find units in both descriptors and add their powers

	jdx = 1
	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit1,idx) == NULL)
		break

	    kdx = find_unstr (punit2, TUN_UNITS(punit1, idx))
	    if (kdx == 0)
		next

	    power = TUN_POWER(punit1,idx) + TUN_POWER(punit2,kdx)
	    if (power == 0)
		next

	    call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
	    call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit3,jdx), 
			 LEN_UNIT)

	    TUN_POWER(punit3,jdx) = power
	    jdx = jdx + 1
	    
	}

	# Find units only in a single descriptor and add them to the
	# new descriptor

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit1,idx) == NULL)
		break

	    if (find_unstr (punit2, TUN_UNITS(punit1, idx)) == 0) {
		call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
		call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit3,jdx), 
			     LEN_UNIT)

		TUN_POWER(punit3,jdx) = TUN_POWER(punit1,idx)
		jdx = jdx + 1
	    }
	}

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit2,idx) == NULL)
		break

	    if (find_unstr (punit1, TUN_UNITS(punit2, idx)) == 0) {
		call malloc (TUN_UNPTR(punit3,jdx), LEN_UNIT, TY_CHAR)
		call strcpy (TUN_UNITS(punit2,idx), TUN_UNITS(punit3,jdx), 
			     LEN_UNIT)

		TUN_POWER(punit3,jdx) = TUN_POWER(punit2,idx)
		jdx = jdx + 1
	    }
	}

	return (punit3)
end

# NUM_UNSTR -- Convert a token to an integer

int procedure num_unstr (value)

char	value[ARB]	# i: string containing token value
#--
int	ic, nc, num

int	ctoi()

begin
	ic = 1
	nc = ctoi (value, ic, num)
	return (num)
end

# POW_UNSTR -- Raise a set of units to an integer power

pointer procedure pow_unstr (punit1, power)

pointer	punit1		# i: units descriptor to be raised to power
int	power
#--
int	idx
pointer	punit2

begin
	# Allocate structure to hold units

	call calloc (punit2, LEN_TUNSTRUCT, TY_INT)

	# Compute the new factor

	TUN_FACTOR(punit2) = TUN_FACTOR(punit1) ** power

	# Find units in both descriptors and add their powers

	if (power != 0) {
	    for (idx = 1; idx <= MAXUNIT; idx = idx + 1) {
		if (TUN_UNPTR(punit1,idx) == NULL)
		    break

		call malloc (TUN_UNPTR(punit2,idx), LEN_UNIT, TY_CHAR)
		call strcpy (TUN_UNITS(punit1,idx), TUN_UNITS(punit2,idx), 
			     LEN_UNIT)

		TUN_POWER(punit2,idx) = TUN_POWER(punit1,idx) * power
	    }
	}

	return (punit2)
end

# SET_UNSTR -- Make a new units description from a units string and its power

pointer procedure set_unstr (ab, units, power)

pointer	ab		# i: hash of units abbreviations
char	units[ARB]	# i: units string
int	power		# i: power of the units 
#--
pointer	punit

begin
	# Allocate structure to hold units

	call calloc (punit, LEN_TUNSTRUCT, TY_INT)
	call malloc (TUN_UNPTR(punit,1), LEN_UNIT, TY_CHAR)	

	# Set the first slot in the structure  to hold the string
	# and power passed to this procedure

	TUN_FACTOR(punit) = 1.0
	TUN_POWER(punit,1) = power
	call abrev_unstr (ab, units, TUN_UNITS(punit,1), LEN_UNIT)

	return (punit)
end

# STR_UNSTR -- Convert units structure into a string

procedure str_unstr (punit, str, maxch)

pointer	punit		# i: units descriptor
char	str[ARB]	# o: string representation of units
int	maxch		# i: max length of string
#--
int	ic, idx

int	strlen(), gstrcpy(), itoc()

begin
	call sprintf (str, maxch, "%g")
	call pargd (TUN_FACTOR(punit))

	ic = strlen (str) + 1

	do idx = 1, MAXUNIT {
	    if (TUN_UNPTR(punit,idx) == NULL)
		break

	    ic = ic + gstrcpy ("*", str[ic], maxch-ic+1)
	    ic = ic + gstrcpy (TUN_UNITS(punit,idx), str[ic], maxch+ic-1)

	    if (TUN_POWER(punit,idx) != 1) {
		ic = ic + gstrcpy ("^", str[ic], maxch-ic+1)
		ic = ic + itoc (TUN_POWER(punit,idx), str[ic], maxch-ic+1)
	    }
	}
end
