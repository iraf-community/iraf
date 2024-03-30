include	"vex.h"

# VEX_FREE -- Free the pseudocode structure
#
# This procedure frees the structure created by vex_compile() and evaluated
# by vex_eval()
#
# B.Simon	21-May-90	Original
# B.Simon	19-Apr-91	Revised to handle multiple types

procedure vex_free (code)

pointer	code		# i: Pointer to pseudocode structure
#--

begin
	call stk_free (VEX_STACK(code))

	call mfree (VEX_CODE(code), TY_INT)
	call mfree (code, TY_STRUCT)
end

