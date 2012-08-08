include	"tjoin.h"
define	SZ_VALUE	30

# B.Simon	16-Apr-99	first code

# READ_TOL -- Parse the string containing the vector of tolerance values

pointer procedure read_tol (tolerance)

char	tolerance[ARB]	# i: Comma separated string of tolerance values
#--
int	ic, jc, nc, ival
pointer	sp, value, errtxt, tol

string	badvalue  "Invalid value in tolerance (%s)"
string	negvalue  "Negative value in tolerance (%g)"

bool	is_number()
int	word_count(), word_fetch(), ctod()

begin
	call smark (sp)
	call salloc (value, SZ_VALUE, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	call malloc (tol, LEN_TOLSTRUCT, TY_INT)

	TOL_NUM(tol) = word_count (tolerance)
	call malloc (TOL_PTR(tol), TOL_NUM(tol), TY_DOUBLE)

	ic = 1
	ival = 1
	while (word_fetch (tolerance, ic, Memc[value], SZ_VALUE) > 0) {
	    if (! is_number (Memc[value])) {
		call sprintf (Memc[errtxt], SZ_LINE, badvalue)
		call pargstr (Memc[value])
		call error (1, Memc[errtxt])
	    }

	    jc = 1
	    nc = ctod (Memc[value], jc, TOL_VAL(tol,ival))

	    if (TOL_VAL(tol,ival) < 0.0) {
		call sprintf (Memc[errtxt], SZ_LINE, negvalue)
		call pargd (TOL_VAL(tol,ival))
		call error (1, Memc[errtxt])
	    }

	    ival = ival + 1
	}

	call sfree (sp)
	return (tol)
end
 
