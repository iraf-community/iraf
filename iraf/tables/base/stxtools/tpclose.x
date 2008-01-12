include	"template.h"

# TP_CLOSE -- Close the group template expansion routines

# Free the dynamic memory used to store the group template structure
#
# B.Simon	28-Feb-89	Original
# B.Simon	21-Aug-91	Changed template structure

procedure tp_close (ptr)

pointer	ptr		# u: Pointer to list of file names
#--
errchk mfree

begin
	call mfree (TP_ROOTPTR(ptr), TY_CHAR)
	call mfree (TP_SECTPTR(ptr), TY_CHAR)
	call mfree (ptr, TY_STRUCT)
	ptr = NULL
end
