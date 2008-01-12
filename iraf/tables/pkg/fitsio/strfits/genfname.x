include	<ctype.h>

# B.Simon	05-Jun-97	original code

# GEN_FNAME -- Generate output filename from info in the fits header

procedure gen_fname (irafname, extname, root, extn, maxch)

char	irafname[ARB]	# i: Filename as stored in primary header
char	extname[ARB]	# i: Filename as stored in first extension
char	root[ARB]	# o: Root name of the generated filename
char	extn[ARB]	# o: Extension of the generated filename
int	maxch		# i: Max length of output strings
#--
char	uscore
int	nc

data	uscore / '_' /

bool	streq()
int	fnroot(), fnextn(), strldx(), strncmp()

begin
	root[1] = EOS
	extn[1] = EOS

	# If the filename is "null_image", the true filename is 
	# to be found in the extension

	if (streq (irafname, "null_image")) {
	    if (extname[1] != EOS) {
		call pesc_dash (extname)
		nc = fnroot (extname, root, maxch)
		nc = fnextn (extname, extn, maxch)
		call cesc_dash (extname)

	    } else {
		call strcpy (irafname, root, maxch)
		extn[1] = EOS
	    }

	} else {
	    if (irafname[1] != EOS) {
		call pesc_dash (irafname)
		nc = fnroot (irafname, root, maxch)
		nc = fnextn (irafname, extn, maxch)
		call cesc_dash (irafname)
	    }
	}

	# If the filename extension is "fit" or "fits", the
	# real extension is buried inside the filename root
	# But first we check to see it's not a numeric suffix

	if (strncmp (extn, "fit", 3) == 0) {
	    nc = strldx (uscore, root)
	    if (nc > 0 && IS_ALPHA (root[nc+1])) {
		root[nc] = EOS
		call strcpy (root[nc+1], extn, maxch)
	    }
	}

end
