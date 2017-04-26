include	"tjoin.h"

# B.Simon	16-Apr-99	first code

# FREE_TOL -- Free the structure containing tolerance values

procedure free_tol (tol)

pointer	tol		# i: Vector of tolerance values
#--

begin
	call mfree (TOL_PTR(tol), TY_DOUBLE)
	call mfree (tol, TY_INT)
end
