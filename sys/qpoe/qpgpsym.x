# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_GPSYM -- Lookup the named parameter in the symbol table and return
# a pointer to the symstruct describing the parameter as the function value.
# NULL is returned if the parameter is not defined, or if the named symbol is
# not a parameter.  Global or datafile local parameter aliases are recursively
# expanded.

pointer procedure qp_gpsym (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

int	fd, nchars, n
pointer	sp, pname, sym, st, sm, ip

int	fm_getfd(), read()
pointer	stfind(), strefsbuf(), qm_symtab()
errchk	syserrs, fm_getfd, read, seek

begin
	call smark (sp)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	st = QP_ST(qp)
	sm = qm_symtab (QP_QM(qp))
	call strcpy (param, Memc[pname], SZ_FNAME)

	# First expand any aliases in the global macro symbol table.
	sym = stfind (sm, param)
	for (n=1;  sym != NULL;  n=n+1) {
	    if (and (S_FLAGS(sym), SF_DELETED) != 0)
		break
	    call strcpy (strefsbuf(sm,S_OFFSET(sym)), Memc[pname], SZ_FNAME)
	    sym = stfind (sm, Memc[pname])
	    if (n > MAX_INDIR)
		call syserrs (SYS_QPMRECUR, param)
	}

	# Lookup the symbol in the datafile local symbol table, recursively
	# expanding any macro defines.  Macros are stored in the datafile
	# symbol table as string parameters of type TY_MACRO.

	sym = stfind (st, Memc[pname])
	if (sym != NULL) {
	    for (n=0;  S_DTYPE(sym) == TY_MACRO;  n=n+1) {
		if (and (S_FLAGS(sym), SF_DELETED) != 0) {
		    break

		} else if (S_LFILE(sym) > 0) {
		    # Macro value stored as data.
		    fd = fm_getfd (QP_FM(qp), S_LFILE(sym), READ_ONLY, 0)

		    call seek (fd, S_OFFSET(sym))
		    nchars = max (0, read (fd, Memc[pname], S_NELEM(sym)))
		    Memc[pname+nchars] = EOS
		    ip = pname

		    call fm_retfd (QP_FM(qp), S_LFILE(sym))

		} else {
		    # Macro value stored in symbol table.
		    ip = strefsbuf (st, S_OFFSET(sym))
		}

		# Macro recursion.
		if (n > MAX_INDIR)
		    call syserrs (SYS_QPMRECUR, param)
	    }
	}

	# Don't "find" the symbol if it has been deleted.
	if (sym != NULL)
	    if (and (S_FLAGS(sym), SF_DELETED) != 0)
		sym = NULL

	call sfree (sp)
	return (sym)
end
