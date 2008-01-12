include <imhdr.h>
include "gf.h"

define	HASH_STRUCTLEN		4
define	HASH_MINSIZE		50		# minimum hash size

define	HASH_SIZE		Memi[$1] 	# length of hash arrays
define	HASH_KEYPTR		Memi[$1+1]	# pointer to array of keys
define	HASH_VALPTR		Memi[$1+2]	# pointer to array of names
define	HASH_MARKPTR		Memi[$1+3]	# pointer to mark field

define	HASH_KEY		Memi[HASH_KEYPTR($1)+$2]
define	HASH_VAL		Memc[HASH_VALPTR($1)+($2)*(SZ_KEYWORD+1)]
define	HASH_MARK		Memi[HASH_MARKPTR($1)+$2]

#* HISTORY *
#* B.Simon	9-Nov-00	Original code
#* B.Simon	6-Dec-00	Added mark field

# GF_ADDHASH -- Add a header keyword to the hash

procedure gf_addhash (hash, keyword)

pointer	hash		# i: hash descriptor
char	keyword[ARB]	# i: keyword name
#--
int	idx, key, step

int	gf_idxhash()

begin
	if (keyword[1] == EOS)
	    return

	if (gf_idxhash (hash, keyword, idx) == NOT_FOUND) {
	    call gf_keyhash (keyword, key, step)
	    HASH_KEY(hash, idx) = key

	    call strcpy (keyword, HASH_VAL(hash,idx), SZ_KEYWORD)
	}
end

# GF_DELHASH -- Delete a header keyword from the hash

procedure gf_delhash (hash, keyword)

pointer	hash		# i: hash descriptor
char	keyword[ARB]	# i: keyword name
#--
int	idx

int	gf_idxhash()

begin
	if (keyword[1] == EOS)
	    return

	if (gf_idxhash (hash, keyword, idx) == IS_FOUND) {
	    HASH_KEY(hash, idx) = 0
	    HASH_VAL(hash,idx) = EOS
	    HASH_MARK(hash,idx) = 0
	}
end

# GF_DUMPHASH -- Dump the hash values to a file

procedure gf_dumphash (hash, fd)

pointer	hash		# i: hash descriptor
int	fd		# i: file descriptor
#--
int	idx

begin
	do idx = 0, HASH_SIZE(hash)-1 {
	    if (HASH_VAL(hash,idx) != EOS) {
		call fprintf (fd, "%d\t%d\t%s\t%d\n")
		call pargi (idx)
		call pargi (HASH_KEY(hash,idx))
		call pargstr (HASH_VAL(hash,idx))
		call pargi (HASH_MARK(hash,idx))
	    }
	}
end

# GF_FINDHASH -- Search a hash to see if a keyword is already present

int procedure gf_findhash (hash, record)

pointer	hash		# i: hash descriptor
char	record[ARB]	# i: record or keyword
#--
char	keyword[SZ_KEYWORD]
int	idx, status

bool	streq()
int	strlen(), strncmp(), gf_idxhash()

begin
	if (strlen (record) <= SZ_KEYWORD) {
	    # Check for history and comment records

	    if (streq (record, "HISTORY") || streq (record, "COMMENT"))
		return (HIST_FOUND)

	    # Short records are actually keywords

	    call strcpy (record, keyword, SZ_KEYWORD)

	} else {
	    # Check for history and comment records
	    
	    if (strncmp (record, "HISTORY ", 8) == 0 ||
		strncmp (record, "COMMENT ", 8) == 0) 
		return (HIST_FOUND)

	    # Search the record for an equals sign. If not found, the 
	    # keyword is a comment. 

	    if (record[SZ_KEYWORD+1] != '=')
		return (CMT_FOUND)

	    # Get keyword from record

	    call gf_trimhash (record, keyword, SZ_KEYWORD)
	}

	# Search for it in hash table

	status = gf_idxhash (hash, keyword, idx)
	return (status)

end

# GF_FREEHASH -- Free the header keyword hash

procedure gf_freehash (hash)

pointer	hash		# i: hash descriptor
#--

begin
	if (hash == NULL)
	    return

	call mfree (HASH_VALPTR(hash), TY_CHAR)
	call mfree (HASH_KEYPTR(hash), TY_INT)
	call mfree (HASH_MARKPTR(hash), TY_INT)

	call mfree (hash, TY_INT)
end

# GF_GETMKHASH -- Set the mark field of a hash

int procedure gf_getmkhash (hash, keyword)

pointer	hash		# i: hash descriptor
char	keyword[ARB]	# i: keyword name
#--
int	idx, value

int	gf_idxhash()

begin
	value = 0

	if (keyword[1] != EOS)
	    if (gf_idxhash (hash, keyword, idx) == IS_FOUND)
		value = HASH_MARK(hash,idx)

	return (value)
end

# GF_IDXHASH -- Get the location of a keyword in a hash table

int procedure gf_idxhash (hash, keyword, idx)

pointer	hash		# i: hash descriptor
char	keyword[ARB]	# i: keyword name
int	idx		# o: location of keyword or empty slot
#--
int	key, step

bool	streq()

begin
	# Compute hash key and step size from keyword name

	call gf_keyhash (keyword, key, step)

	# Search hash for key

	idx = mod (key, HASH_SIZE(hash))

	repeat {
	    if (HASH_VAL(hash,idx) == EOS) {
		return (NOT_FOUND)

	    } else if (HASH_KEY(hash,idx) == key) {
		if (streq (keyword, HASH_VAL(hash,idx)))
		    return (IS_FOUND)
	    }

	    idx = mod (idx + step, HASH_SIZE(hash))
	}

end

# GF_INITHASH -- Create a hash to store header keyword names

pointer procedure gf_inithash (len)

int	len		# i: maximum hash length
#--
int	maxlen, hash_len
pointer	hash

begin
	# Calculate size of hash

	hash_len = 1
	maxlen = max (len, HASH_MINSIZE)

	while (hash_len < 2 * maxlen)
	    hash_len = 2 * hash_len

	# Allocate hash structure

	call malloc (hash, HASH_STRUCTLEN, TY_INT)

	HASH_SIZE(hash) = hash_len

	call calloc (HASH_KEYPTR(hash), hash_len, TY_INT)
	call calloc (HASH_VALPTR(hash), hash_len*(SZ_KEYWORD+1), TY_CHAR)
	call calloc (HASH_MARKPTR(hash), hash_len, TY_INT)

	return (hash)
end

# GF_KEYHASH -- Compute hash key and step size from keyword name

procedure gf_keyhash (keyword, key, step)

char	keyword[ARB]	# i: keyword name
int	key		# o: key value
int	step		# o: step size
#--
int	ic

begin
	# Compute hash key and step size

	key = 0
	step = 0
	for (ic = 1; keyword[ic] != EOS; ic = ic + 1) {
	    key = 2 * key + keyword[ic]
	    step = step + keyword[ic]
	}

	# Ensure step size is odd so we hit all spots in the table

	step = step - mod (step, 2) + 1
end

# GF_NEXTHASH -- Step through hash table and return next keyword

int procedure gf_nexthash (hash, idx, keyword, maxch)

pointer	hash		# i: hash descriptor
int	idx		# u: index to next slot in hash table to check
char	keyword[ARB]	# o: keyword returned from search
int	maxch		# i: maximum length of keyword
#--
int	nc

int	gstrcpy()

begin
	while (idx < HASH_SIZE(hash)) {
	    if (HASH_VAL(hash,idx) != EOS) {
		nc = gstrcpy (HASH_VAL(hash,idx), keyword, maxch)
		idx = idx + 1
		return (nc)
	    }
		
	    idx = idx + 1
	}

	return (0)
end

# GF_SETMKHASH -- Set the mark field of a hash

procedure gf_setmkhash (hash, keyword, value)

pointer	hash		# i: hash descriptor
char	keyword[ARB]	# i: keyword name
int	value		# i: mark value
#--
int	idx

int	gf_idxhash()

begin
	if (keyword[1] == EOS)
	    return

	if (gf_idxhash (hash, keyword, idx) == IS_FOUND)
		HASH_MARK(hash,idx) = value

end

# GF_SIZEHASH -- Calculate inital size of hash

int procedure gf_sizehash (im)

pointer	im		# i: image descriptor
#--
char	nl
int	len, ua_len, line_len
pointer	ua

data	nl / '\n' /
int	strlen(), stridx()

begin
	ua = IM_USERAREA(im)
	ua_len = strlen (Memc[ua])

	line_len = stridx (nl, Memc[ua])
	len =  (ua_len / line_len) + 1

	return (len)
end

# GF_TRIMHASH -- Trim a record to its initial keyword

procedure gf_trimhash (record, keyword, maxch)

char	record[ARB]	# i: record containing keyword
char	keyword[ARB]	# o: keyword name
int	maxch		# o: max length of keyword string
#--
int	ic, jc, nc

begin
	if (record[SZ_KEYWORD+1] != '=') {
	    keyword[1] = EOS

	} else {
	    nc = min (maxch, SZ_KEYWORD)

	    jc = 0
	    do ic = 1, nc {
		if (record[ic] != ' ')
		    jc = ic

		keyword[ic] = record[ic]
	    }

	    keyword[jc+1] = EOS
	}
	    
end

