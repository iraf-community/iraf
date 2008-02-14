# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_DSYM -- Dump the symbol table of a QPOE file.

procedure qp_dsym (qp, out)

pointer	qp			#I QPOE descriptor
int	out			#I output file

int	nsyms, i
pointer	sp, st, sym, op, pname, syms
pointer	sthead(), stnext(), stname()

begin
	call smark (sp)
	st = QP_ST(qp)

	# Count the symbols.
	nsyms = 0
	for (sym=sthead(st);  sym != NULL;  sym=stnext(st,sym))
	    nsyms = nsyms + 1

	# Construct a reversed array of symbol pointers.
	call salloc (syms, nsyms, TY_POINTER) 
	op = syms + nsyms - 1
	for (sym=sthead(st);  sym != NULL;  sym=stnext(st,sym)) {
	    Memi[op] = sym
	    op = op - 1
	}

	# Output the symbols.
	if (nsyms > 0)
	    call fprintf (out,
"          SYMBOL FLAGS DTYPE DSYM NELEM MAXELEM SZELEM COMMENT LFILE OFFSET\n")

	do i = 1, nsyms {
	    sym = Memi[syms+i-1]
	    pname = stname (st, sym)

	    call fprintf (out, "%16s %5o %5d %4d %5d %7d %6d %7x %5d %6d\n")
		call pargstr (Memc[pname])
		call pargi (and (S_FLAGS(sym), 77777B))
		call pargi (S_DTYPE(sym))
		call pargi (S_DSYM(sym))
		call pargi (S_NELEM(sym))
		call pargi (S_MAXELEM(sym))
		call pargi (S_SZELEM(sym))
		call pargi (S_COMMENT(sym))
		call pargi (S_LFILE(sym))
		call pargi (S_OFFSET(sym))
	}

	call sfree (sp)
end
