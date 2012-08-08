# SPAN -- Copy characters while they match a set
#
# B.Simon	25-Apr-88	Original

int procedure span (set, str, ic, outstr, maxch)

char	set[ARB]	#  i: Set of characters used in matching
char	str[ARB]	#  i: Input string
int	ic		# io: Index to input string character
char	outstr[ARB]	#  o: Output string
int	maxch		#  i: Maximum length of output string
#--
bool	match
int	jc, kc, setlen

int	strlen()

begin
	# Loop over characters in the input string

	setlen = strlen (set)
	for (jc = 1; str[ic] != EOS && jc <= maxch; ic = ic + 1) {

	    # See if the current character in the input string
	    # matches the characters in the set

	    match = false
	    do kc = 1, setlen {
		if (str[ic] == set[kc]) {
		    match = true
		    break
		}
	    }

	    # Copy character to the output string if it matches

	    if (! match)
		break

	    outstr[jc] = str[ic]
	    jc = jc + 1

	}

	# Return number of characters in output string

	outstr[jc] = EOS
	return (jc - 1)
end

# NOSPAN -- Copy characters while they do not match a set

int procedure nospan (set, str, ic, outstr, maxch)

char	set[ARB]	#  i: Set of characters used in matching
char	str[ARB]	#  i: Input string
int	ic		# io: Index to input string character
char	outstr[ARB]	#  o: Output string
int	maxch		#  i: Maximum length of output string
#--
bool	match
int	jc, kc, setlen

int	strlen()

begin
	# Loop over characters in the input string

	setlen = strlen (set)
	for (jc = 1; str[ic] != EOS && jc <= maxch; ic = ic + 1) {

	    # See if the current character in the input string
	    # matches the characters in the set

	    match = false
	    do kc = 1, setlen {
		if (str[ic] == set[kc]) {
		    match = true
		    break
		}
	    }

	    # Copy character to the output string if it does not match

	    if (match)
		break

	    outstr[jc] = str[ic]
	    jc = jc + 1

	}

	# Return number of characters in output string

	outstr[jc] = EOS
	return (jc - 1)
end
