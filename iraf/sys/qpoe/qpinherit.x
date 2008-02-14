# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"qpoe.h"

# QP_INHERIT -- Copy all the inheritable parameters from one datafile to
# another.

procedure qp_inherit (n_qp, o_qp, out)

pointer	n_qp			#I QPOE descriptor of new datafile
pointer	o_qp			#I QPOE descriptor of old datafile
int	out			#I output stream for verbose messages, or NULL

int	nsyms, i
pointer	sp, n_st, o_st, sym, op, pname, syms
pointer	sthead(), stnext(), stname()
int	qp_accessf()

begin
	call smark (sp)

	n_st = QP_ST(n_qp)
	o_st = QP_ST(o_qp)

	# Count the symbols to be copied.
	nsyms = 0
	for (sym=sthead(o_st);  sym != NULL;  sym=stnext(o_st,sym))
	    if (and (S_FLAGS(sym), SF_DELETED) == 0)
		if (and (S_FLAGS(sym), SF_INHERIT) != 0)
		    nsyms = nsyms + 1

	# Construct a reversed array of symbol pointers.
	call salloc (syms, nsyms, TY_POINTER) 
	op = syms + nsyms - 1
	for (sym=sthead(o_st);  sym != NULL;  sym=stnext(o_st,sym))
	    if (and (S_FLAGS(sym), SF_DELETED) == 0)
		if (and (S_FLAGS(sym), SF_INHERIT) != 0) {
		    Memi[op] = sym
		    op = op - 1
		}

	# Copy each symbol.
	do i = 1, nsyms {
	    pname = stname (o_st, Memi[syms+i-1])
	    if (qp_accessf (n_qp, Memc[pname]) == YES) {
		if (out != NULL) {
		    call fprintf (out,
			"parameter `%s' already exists, not copied\n")
			call pargstr (Memc[pname])
		}
	    } else iferr (call qp_copyf (o_qp, Memc[pname], n_qp, Memc[pname]))
		call erract (EA_WARN)
	}

	call sfree (sp)
end
