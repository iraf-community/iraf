# Definition of delimeters used in parsing words

define	IS_DELIM	(($1) <= ' ' || ($1) == ',')
define	NOT_DELIM	(($1) > ' ' && ($1) != ',')

.help
.nf_________________________________________________________________________

The procedures in this file perform simple processing on lists of
words.  These procedures count the number of words in a list, fetch
the next word in a list, find the n-th word in a list, check for an
exact match between a word and a list of words.  A word is any group
of contiguous characters which are neither whitespace or commas. The
definition of whitespace is anomalous, it includes any character whose
integer value is less than or equal to a blank. Note that words cannot
be delimeted by quotes and that escape processing is not done.

.endhelp____________________________________________________________________

#______________________________HISTORY______________________________________
#
# B.Simon	20-Apr-1990	Modified versions of CDBS routines adb_*tok.x
#
#___________________________________________________________________________

# WORD_COUNT -- Return the number of words in a list of words

int procedure word_count (list)

char	list[ARB]	# i: List of words
#--
char	ch
int	count, ic

begin

	# The absolute value of count is the number of the current
	# word of the list, count is negative if we are currently
	# between words.

	count = 0

	# Loop over all characters in the list

	for (ic = 1 ; list[ic] != EOS; ic = ic + 1) {
	    ch = list[ic]

	    if (count > 0) {
		if (IS_DELIM(ch))
		    count = - count

	    } else if (NOT_DELIM(ch)) {
		count = - count + 1
	    }
	}

	return (abs(count))
end

# WORD_FETCH -- Retrieve next word from string

int procedure word_fetch (str, ic, word, maxch)

char	str[ARB]	#  i: String containing words
int	ic		# io: Index of starting character
char	word[ARB]	#  o: Word string
int	maxch		#  i: Declared length of output string
#--
char	ch
int	jc

begin
	# Skip leading whitespace or commas. Don't go past string terminator.

	for (ch = str[ic]; IS_DELIM(ch); ch = str[ic]) {
	    if (ch == EOS)
		break
	    ic = ic + 1
	}

	# Copy characters to word. End when maxch is reached, or
	# when commas, whitespace, or EOS is found

	for (jc = 1; jc <= maxch; jc = jc + 1) {
	    if (IS_DELIM(ch))
		break

	    word[jc] = ch
	    ic = ic + 1
	    ch = str[ic]
	}
	word[jc] = EOS

	# If loop is terminated because of maxch, eat remaining characters
	# in field

	while (NOT_DELIM(ch)) {
	    ic = ic + 1
	    ch = str[ic]
	}

	# Return number of characters in word

	return (jc - 1)

end

# WORD_FIND -- Find the i-th word in a list of words

int procedure word_find (index, list, word, maxch)

int	index		# i: Index to word within list
char	list[ARB]	# i: List of words
char	word[ARB]	# o: Word returned by this procedure
int	maxch		# i: Declared length of output string
#--
char	ch
int	count, ic, jc

begin
	# The absolute value of count is the number of the current
	# word of the list, count is negative if we are currently
	# between words

	count = 0

	# Loop until i-th word is reached in list

	for (ic = 1 ; count < index && list[ic] != EOS; ic = ic + 1) {
	    ch = list[ic]

	    if (count > 0) {
		if (IS_DELIM(ch))
		    count = - count

	    } else if (NOT_DELIM(ch)) {
		count = - count + 1
	    }
	}

	# If index is out of bounds, return zero

	if (index < 0 || index > count)
	    return (0)

	jc = 1
	for (ic = ic - 1; NOT_DELIM(list[ic]); ic = ic + 1) {
	    if (jc > maxch)
		break

	    word[jc] = list[ic]
	    jc = jc + 1
	}
	word[jc] = EOS

	# Return number of characters in word

	return (jc - 1)
end

# WORD_MATCH -- Return number of the word in the list which matches the word

int procedure word_match (word, list)

char	word[ARB]	# i: Word to be matched
char	list[ARB]	# i: List of words
#--
char	ch
int	match, inword, ic, jc

begin
	# The absolute value of inword is the number of the current
	# word of the list, inword is negative if we are currently
	# between words in the list

	jc = 1
	match = 0
	inword = 0

	# Loop over all characters in the list

	for (ic = 1 ; list[ic] != EOS; ic = ic + 1) {
	    ch = list[ic]

	    # First case: current character is within a word

	    if (inword > 0) {

		# Check for conversion to second case

		if (IS_DELIM(ch)) {
		    inword = - inword

		    # Simultaneous end of word in list and word 
		    # means a match has been found

		    if (match != 0 && word[jc] == EOS)
			break
		    else
			match = 0

		} else if (match != 0) {

		    # Check for match between list and word

		    if (ch == word[jc])
			jc = jc + 1
		    else 
			match = 0
		}

	    # Second case: current character is between words
	    # Check for conversion to first case

	    } else if (NOT_DELIM(ch)) {
		jc = 1
		ic = ic - 1
		inword = - inword + 1
		match = inword
	    }
	}

	# If list ended before word, there was no match

	if (word[jc] != EOS)
	    match = 0

	return (match)
end
