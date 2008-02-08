include	"tjoin.h"

# B.Simon	16-Apr-99	first code

# CLOSE_IOTAB -- Close table and release data structure describing it

procedure close_iotab (tj)

pointer	tj		# i: Data structure describing table
#--

begin
	call tbtclo (TJ_TAB(tj))

	if (TJ_JPTR(tj) != NULL)
	    call mfree (TJ_JPTR(tj), TY_POINTER)

	if (TJ_DPTR(tj) != NULL)
	    call mfree (TJ_DPTR(tj), TY_POINTER)

	call mfree (tj, TY_STRUCT)
end
