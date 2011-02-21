# MVWORD -- Move one word over in a string
#
# B.Simon	20-Mar-91	Original

procedure mvword_next (str, ic, jc)

char	str[ARB]	# i: String containing words
int	ic		# i: Starting character (0 to strlen(str))
int	jc		# o: Character before start of next word
#--
int	nc
int	strlen()

begin
	# Find next blank

	nc = strlen (str)
	for (jc = min (ic+1, nc); jc < nc; jc = jc + 1) {
	    if (str[jc] <= ' ')
		break
	}

	# Find first non-blank character after blank

	for ( ; jc < nc; jc = jc + 1) {
	    if (str[jc] > ' ') {
		jc = jc - 1	# back up to previous blank
		break
	    }
	}

end

procedure mvword_prev (str, ic, jc)

char	str[ARB]	# i: String containing words
int	ic		# i: Starting character (0 to strlen(str))
int	jc		# o: Character before start of next word
#--

begin
	# Find previous nonblank character

	for (jc = max (ic-1, 0); jc > 0; jc = jc - 1) {
	    if (str[jc] > ' ')
		break
	}

	# Find blank preceding non-blank character

	for ( ; jc > 0; jc = jc - 1) {
	    if (str[jc] <= ' ')
		break
	}

end
