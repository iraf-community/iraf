define	BLANK		' '

#* HISTORY *
#* B.Simon	24-Jul-92	Original
#* B.Simon	17-Dec-97	Copied from old cdbs for use in row selector

# This procedure removes leading and trailing whitespace and compresses 
# interior whitepace to a single blank. Whitespace is defined to be any 
# character with an ascii value less than or equal to that of the blank.
# 

# TRSTRIM -- Remove non-significant whitespace from string

int procedure trstrim (str)

char	str[ARB]	# u: String to be modified
#--
bool	flag
int	ic, jc

begin
	# Initialize flag to true so that leading blanks are skipped

	jc = 1
	flag = true

	# Convert control characters to blanks, skip multiple blanks

	for (ic = 1; str[ic] != EOS; ic = ic + 1) {

	    if (str[ic] > BLANK) {
		flag = false
		if (jc < ic)
		    str[jc] = str[ic]
		jc = jc + 1

	    } else {
		if (! flag) {
		    flag = true
		    str[jc] = ' '
		    jc = jc + 1
		}
	    }
	}

	# Remove trailing blanks

	if (flag && jc > 1)
	    jc = jc - 1

	str[jc] = EOS
	return (jc - 1)

end
