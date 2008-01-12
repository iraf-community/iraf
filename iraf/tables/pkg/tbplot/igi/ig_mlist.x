include "igi.h"

procedure ig_mlist (igs)

pointer	igs		# igi parameters structure

pointer	symtabd		# Command symbol table descriptor
pointer	sym
pointer	sp, key

int	stnsymbols()
pointer	sthead(), stnext(), stname()
bool	strne()

begin
	call lcmdcat (igs, YES)
#	call cmdcat  (igs, NO)
	symtabd = SYM_TABLE(igs)

	if (stnsymbols (symtabd, 0) == 0) {
	    call eprintf ("No macros defined ")
	    return
	}

	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	Memc[key] = EOS

	call gdeactivate (GIO_GP(igs), 0)
	call printf ("Macro Name\tNumber of Arguments\n")
	sym = sthead (symtabd)

	repeat {
	    if (strne (Memc[stname (symtabd, sym)], Memc[key])) {
		# One version only
		call printf ("%s\t\t%d\n")
		    call pargstr (Memc[stname (symtabd, sym)])
		    call pargi (SYM_NMACARG(sym))
	    }
	    call strcpy (Memc[stname (symtabd, sym)], Memc[key], SZ_LINE)
	    sym = stnext (symtabd, sym)
	} until (sym == NULL)

	call flush (STDOUT)
	call greactivate (GIO_GP(igs), 0)

	call sfree (sp)
end
