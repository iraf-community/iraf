# A set of procedures that implement a generic hash table. The hash table 
# stores the key, plus a pointer to the value structure. It should be 
# wrapped  in a more specific set of calls that can read the value structure

define	LEN_UNHSTRUCT	5

define	UNH_SIZE	Memi[$1]
define	UNH_NEXT	Memi[$1+1]
define	UNH_KEYBUF	Memi[$1+2]
define	UNH_VALBUF	Memi[$1+3]
define	UNH_STRBUF	Memi[$1+4]

define	UNH_KEY		Memi[UNH_KEYBUF($1)+$2]
define	UNH_VALUE	Memi[UNH_VALBUF($1)+$2]

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# ADD_UNHASH -- Add a new keyword and value to the hash table

procedure add_unhash (hash, keyword, value)

pointer	hash		# i: Descriptor of hash table
char	keyword[ARB]	# i: Keyword to add to hash table
pointer	value		# i: Value descriptor
#--
int	index, nc

string	duplicate  "Cannot add duplicate keyword to hash table"

int	gstrcpy(), loc_unhash()

begin
	# Find where keyword should be inserted

	index = loc_unhash (hash, keyword)

	# Adding duplicate keywords is not allowed

	if (UNH_KEY(hash,index) != NULL) {
	    call tuniterr (duplicate, keyword)

	} else {
	    UNH_KEY(hash,index) = UNH_NEXT(hash)
	    UNH_VALUE(hash,index) = value

	    nc = gstrcpy (keyword, Memc[UNH_NEXT(hash)], ARB)
	    UNH_NEXT(hash) = UNH_NEXT(hash) + nc + 1
	}
end

# CALC_UNHASH -- Calculate hash index and step size from keyword

procedure calc_unhash (hash, keyword, index, step)

pointer	hash		# i: Descriptor of hash table
char	keyword[ARB]	# i: Keyword to search for in hash table
int	index		# o: Location to place keyword at in hash
int	step		# o: Step size in case location is filled
#--
int	ic

begin
	# Standard hash table function based on munging characters
 
	index = 0
	step = 0

	for (ic = 1; keyword[ic] != EOS; ic = ic + 1) {
	    index = 2 * index + keyword[ic]
	    step = step + keyword[ic]
	}

	# This line ensures the step size is odd

	step = step - mod (step, 2) + 1
end

# EACH_UNHASH -- Retrieve values from hash table serially

int procedure each_unhash (hash, index, keyword, value, maxch)

pointer	hash		# i: Descriptor of hash table
int	index		# u: Index of next slot in hash table to examine
char	keyword[ARB]	# o: Keyword name
pointer	value		# o: Keyword value
int	maxch		# i: Maximum length of keyword
#--

begin
	while (index < UNH_SIZE(hash)) {
	    if (UNH_KEY(hash,index) != NULL) {
		call strcpy (Memc[UNH_KEY(hash,index)], keyword, maxch)
		value = UNH_VALUE(hash,index)
		index = index + 1
		return (OK)
	    }

	    index = index + 1
	}

	return (EOF)
end

# FREE_UNHASH -- Free a hash table

procedure free_unhash (hash)

pointer	hash		# i: hash table descriptor
#--

begin
	# This code assumes that all memory associated 
	# with the values has already been freed

	call mfree (UNH_STRBUF(hash), TY_CHAR)
	call mfree (UNH_VALBUF(hash), TY_INT)
	call mfree (UNH_KEYBUF(hash), TY_INT)
	call mfree (hash, TY_INT)
end

# GET_UNHASH -- Return a keyword's value from a hash

int procedure get_unhash (hash, keyword, value)

pointer	hash		# i: Descriptor of hash table
char	keyword[ARB]	# i: Keyword to add to hash table
pointer	value		# o: pointer to hash table value
#--
int	index, status

int	loc_unhash ()

begin
	# The keyword is found if its slot is not null

	index = loc_unhash (hash, keyword)

	if (UNH_KEY(hash,index) == NULL) {
	    value = NULL
	    status = NO
	} else {
	    value = UNH_VALUE(hash,index)
	    status = YES
	}

	return (status)
end

# LOC_UNHASH -- Return index of location where a key should be inserted

int procedure loc_unhash (hash, keyword)

pointer	hash		# i: Descriptor of hash table
char	keyword[ARB]	# i: Keyword to add to hash table
#--
int	index, step

bool	streq()

begin
	# Calculate initial guess at position in hash table
	# and step size in case guessed position is already filled

	call calc_unhash (hash, keyword, index, step)
	index = mod (index, UNH_SIZE(hash))

	# Loop until an empty slot is found or the keyword is matched

	repeat {
	    if (UNH_KEY(hash,index) == NULL) {
		break

	    } else if (streq (Memc[UNH_KEY(hash,index)], keyword)) {
		break
	    }

	    index = mod (index + step, UNH_SIZE(hash))
	}

	return (index)
end

# NEW_UNHASH -- Create a new hash table

pointer procedure new_unhash (nkey, keysize)

int	nkey		# i: number of keywords in the hash
int	keysize		# i: maximum length of a key
#--
int	size
pointer	hash

begin
	# Find a power of two greater than the number of keywords

	for (size = 1; size < 2 * nkey; size = 2 * size)
	    ;

	# Allocate structure for hash and set initial values

	call malloc (hash, LEN_UNHSTRUCT, TY_INT)
	call calloc (UNH_KEYBUF(hash), size, TY_INT)
	call calloc (UNH_VALBUF(hash), size, TY_INT)
	call malloc (UNH_STRBUF(hash), size*(keysize+1), TY_CHAR)

	UNH_SIZE(hash) = size
	UNH_NEXT(hash) = UNH_STRBUF(hash)

	return (hash)
end

