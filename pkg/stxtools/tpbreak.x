# TP_BREAK -- Break an image name into bracket delimeted substrings
#
# B.Simon	02-Jun-89	Original

procedure tp_break (imname, part, npart, maxch)

char	imname[ARB]		# i: Image name
char	part[maxch,ARB]		# o: Array of image name parts
int	npart			# i: Maximum number of parts
int	maxch			# i: Maximum length of part
#--
bool	inside
char	ch
int	ic, jc, ipart
pointer	sp, errmsg

string	syntax  "Syntax error in image name (%s)"

begin
	# Allocate memory for error message

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Initialize output to null string

	do ipart = 1, npart
	    part[1,ipart] = EOS

	# Break image name into bracket delimeted components
	# The variable inside is used as a check that brackets are paired

	jc = 1
	ipart = 1
	inside = false

	for (ic = 1; ipart <= npart && imname[ic] != EOS; ic = ic + 1) {

	    ch = imname[ic]
	    if (ch == '\\') {
		ic = ic + 1

	    } else if (ch == '[') {
		if (inside) {
		    call sprintf (Memc[errmsg], SZ_LINE, syntax)
		    call pargstr (imname)
		    call error (1, Memc[errmsg])
		}
		part[jc,ipart] = EOS
		ipart = ipart + 1
		inside = true
		jc = 1

	    } else if (ch == ']') {
		if (! inside) {
		    call sprintf (Memc[errmsg], SZ_LINE, syntax)
		    call pargstr (imname)
		    call error (1, Memc[errmsg])
		}
		inside = false

	    } else if (ipart > 1 && ! inside) {
		call sprintf (Memc[errmsg], SZ_LINE, syntax)
		call pargstr (imname)
		call error (1, Memc[errmsg])
	    }

	    part[jc,ipart] = imname[ic]
	    jc = jc + 1

	    if (jc > maxch) {
		call sprintf (Memc[errmsg], SZ_LINE, syntax)
		call pargstr (imname)
		call error (1, Memc[errmsg])
	    }
	}

	part[jc,ipart] = EOS
	call sfree (sp)
end
