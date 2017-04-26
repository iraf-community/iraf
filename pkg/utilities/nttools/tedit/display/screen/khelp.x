include	<fset.h>

# K_HELP -- Retrieve help text for function key sequences

procedure k_help (text)

pointer	text		# o: Help text
#--
include	"screen.com"

begin
	text = htext
end

procedure k_eseq (name, eseq, maxch)

char	name[ARB]	# i: Name bound to escape sequence
char	eseq[ARB]	# o: String representation of escape sequence
int	maxch		# i: Maximum length of escape sequence
#--
include "screen.com"

bool	match, in_name
int	ic
pointer	ch

begin
	ic = 1
	match = true
	in_name = true

	for (ch = htext; Memc[ch] != EOS; ch = ch + 1) {
	    if (in_name) {
		if (Memc[ch] == '=') {
		    match = match && name[ic] == EOS
		    in_name = false
		    ic = 1
		} else if (match) {
		    if (Memc[ch] == name[ic]) {	
			ic = ic + 1
		    } else {
			match = false
		    }
		}

	    } else {
		if (Memc[ch] == '\n') {
		    if (match) 
			break
		    ic = 1
		    match = true
		    in_name = true
		} else if (match && ic <= maxch) {
		    eseq[ic] = Memc[ch]
		    ic = ic + 1
		}
	    }
	}
	eseq[ic] = EOS

end
