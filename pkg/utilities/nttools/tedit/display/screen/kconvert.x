include	<ctype.h>

# K_CONVERT -- Convert a string with coded escape sequences to the real thing
#
# B.Simon	23-Jan-89	Original

procedure k_convert (escstr, escseq, maxch)

char	escstr[ARB]	# i: Escape sequence string
char	escseq[ARB]	# o: Escape key sequence
int	maxch		# i: Declared length of key sequence
#--
int	ic, jc, index, num

string	echars  "befnrt"
string	ecodes  "\010\033\f\n\r\t"

int	stridx()

begin
	ic = 1
	for (jc = 1; jc <= maxch; jc = jc + 1) {

	    # Exit when all characters in escape string have been processed

	    if (escstr[ic] == EOS)
		break

	    # Convert escape sequence

	    if (escstr[ic] == '\\') {
		ic = ic + 1
		index = stridx (escstr[ic], echars)
		if (index > 0) {
		    escseq[jc] = ecodes[index]
		} else if (IS_DIGIT(escstr[ic])) {
		    for (num = 0; IS_DIGIT(escstr[ic]); ic = ic + 1)
			num = 8 * num + TO_DIGIT(escstr[ic])
		    ic = ic - 1
		    escseq[jc] = num
		} else {
		    escseq[jc] = escstr[ic]
		}

	    # Convert control sequence

	    } else if (escstr[ic] == '^') {
		ic = ic + 1
		escseq[jc] = mod (int(escstr[ic]), 32)

	    # Copy ordinary character

	    } else {
		escseq[jc] = escstr[ic]
	    }

	    ic = ic + 1
	}

	escseq[jc] = EOS
end
