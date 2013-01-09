include	"trs.h"

#* HISTORY *
#* B.Simon	04-Nov-94	original

# TRSCLOSE  - Free table row selector code buffer

procedure trsclose (trs)

pointer	trs		# i: Pseudocode structure
#--
string	notcode "trsclose: not pointer to code"

begin
	if (TRS_IDENT(trs) != TRS_MAGIC)
	    call error (1, notcode)

	call rst_free (TRS_ROWS(trs))

	call mfree (TRS_VALUE(trs), TY_DOUBLE)
	call mfree (TRS_CODE(trs), TY_INT)
	call mfree (trs, TY_INT)
end


