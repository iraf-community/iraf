include	<fset.h>

# K_END -- Terminate keyboard
#
# B.Simon	23-Jan-89	Original

procedure k_end ()

#--
include	"screen.com"

int	klen
int	strlen()

begin
	# Restore keyboard to original state and end raw mode

	if (ttyin != NULL) {
	    if (term != NULL) {
		klen = strlen (ke)
		if (klen > 0)
		    call ttywrite (ttyout, term, ke, klen, 1)
	    }
	    call fseti (ttyin, F_RAW, NO)
	}

	# Release dynamic memory

	if (keytab != NULL)
	    call mfree (keytab, TY_INT)

	if (htext != NULL)
	    call mfree (htext, TY_CHAR)
	
end

# K_ERROR -- Procedure called on error exit

procedure k_error (status)

int	status		# i: Error status
#--

begin
	if (status != OK) {
	    call k_end
	    call ps_end
	}
end
