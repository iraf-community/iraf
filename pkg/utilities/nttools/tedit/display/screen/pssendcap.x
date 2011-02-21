# PS_SENDCAP -- Send a termcap command to the terminal
#
# B.Simon	18-Jan-89	Original

procedure ps_sendcap (cap, affcnt)

char	cap[ARB]	# i: Termcap capability name
int	affcnt		# i: Number of lines affected by the command
#--
include	"screen.com"

int	nchar
pointer	sp, capstr

int	ttygets()

begin
	call smark (sp)
	call salloc (capstr, SZ_FNAME, TY_CHAR)

	# Retrieve the termcap capability string given its name
	# If it is found, write it to the terminal

	nchar = ttygets (term, cap, Memc[capstr], SZ_FNAME)
	if (nchar > 0) {
	    call ttywrite (ttyout, term, Memc[capstr], nchar, affcnt)
	    ##  call ps_debugcap (cap, Memc[capstr], nchar)
	}

	call sfree (sp)
end

# PS_DEBUGCAP -- Print a termcap string for debugging purposes

procedure ps_debugcap (capname, capstr, nchar)

char	capname[ARB]	# i: Termcap capability name
char	capstr[ARB]	# i: Termcap capability string
int	nchar		# i: Number of characters in string
#--
include	"screen.com"

char	ch
int	ic, jc
pointer	sp, out

begin
	# Allocate memory for strings

	call smark (sp)
	call salloc (out, SZ_LINE, TY_CHAR)

	jc = 0
	do ic = 1, nchar {
	    ch = capstr[ic]
	    if (ch < ' ') {
		Memc[out+jc] = '^'
		jc = jc + 1
		ch = ch + '@'
	    }
	    Memc[out+jc] = ch
	    jc = jc + 1
	}
	Memc[out+jc] = EOS
	
	# Write string to STDOUT and flush

	call fprintf (ttyout, "%s = %s\n")
	    call pargstr (capname)
	    call pargstr (Memc[out])
	call flush (ttyout)

	call sfree (sp)
end
