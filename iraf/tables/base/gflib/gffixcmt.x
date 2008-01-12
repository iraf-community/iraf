#* HISTORY *
#* B.Simon	 6-Dec-00	Original code

# GF_FIX_CMT -- Modify comment of card in user area by adding asterisk

procedure gf_fix_cmt (record)

char	record[ARB]	# u: record from user area
#--
char	slash, squote, blank, nl, star
int	ic, jc, kc, nc

data	slash	/ '/' /
data	squote  / '\'' /
data	blank   / ' ' /
data	nl      / '\n' /
data	star    / '*' /

int	strlen(), stridx()

begin
	# Find location of slash that starts comment

	nc = strlen (record)
	if (record[11] != squote) {
	    jc = 11

	} else {
	    do ic = 12, nc {
		if (record[ic] == squote && record[ic+1] != squote) {
		    jc = ic
		    break
		}
	    }
	}

	jc = stridx (slash, record[jc]) + jc - 1

	# Add a slash to start the comment if not present

	if (jc == 0) {
	    record[32] = '/'
	    jc = 32
	}

	# Find the last nonblank character

	kc = jc + 1
	do ic = jc+2, nc {
	    if (record[ic] != blank && record[ic] != nl)
		kc = ic
	}

	# Convert blank to asterisk

	if (kc <= nc && record[kc] != star) 
	    record[kc+1] = star

end
