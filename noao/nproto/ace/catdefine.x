include	"ace.h"
include	"cat.h"
include	"objs.h"


define	CATDEF		"ace$lib/catdef.dat"

# CATDEF -- Read catalog definition file and create symbol table.

procedure catdefine (tbl, mode, catdef)

pointer	tbl			#I Table pointer
int	mode			#I Table access mode
char	catdef[ARB]		#I Catalog definition file

int	i, n, fd, args, func, ncols
pointer	sp, fname, name, label, str, entry, sym
pointer	stp1, stp2, tp

bool	strne()
int	open(), fscan(), nscan(), strncmp(), ctoi(), ctor()
int	stridxs(), strldxs(), strdic()
pointer	stopen(), stenter(), stfind(), sthead(), stnext(), stname()
errchk	open, stopen, tbcdef1, tbcfnd1

define	err_	10

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (entry, ENTRY_LEN, TY_STRUCT)
	call aclri (Memi[entry], ENTRY_LEN)

	# Build a symbol table from ace$objs.h.
	fd = open ("ace$src/objs.h", READ_ONLY, TEXT_FILE)
	stp1 = stopen ("catdefine", 100, ENTRY_LEN, SZ_LINE)
	while (fscan(fd) != EOF) {
	    Memc[fname] = EOS
	    call gargwrd (Memc[fname], SZ_FNAME)
	    if (strne (Memc[fname], "define"))
		next
	    call gargwrd (Memc[name], SZ_FNAME)
	    if (strncmp (Memc[name], "ID_", 3) != 0)
		next
	    call gargi (ENTRY_ID(entry))
	    call gargwrd (Memc[label], SZ_LINE)
	    if (Memc[label] != '#')
		next
	    call gargwrd (Memc[label], SZ_LINE)
	    call gargwrd (ENTRY_UNITS(entry), ENTRY_ULEN)
	    call gargwrd (ENTRY_FORMAT(entry), ENTRY_FLEN)
	    call gargstr (ENTRY_DESC(entry), ENTRY_DLEN)
	    if (nscan() < 7)
		next
	    switch (Memc[label]) {
	    case 'i':
		ENTRY_TYPE(entry) = TY_INT
	    case 'r':
		ENTRY_TYPE(entry) = TY_REAL
	    case 'd':
		ENTRY_TYPE(entry) = TY_DOUBLE
	    default:
		i = 1
		if (ctoi (Memc[label], i, ENTRY_TYPE(entry)) == 0)
		    next
		ENTRY_TYPE(entry) = -ENTRY_TYPE(entry)
	    }
	    ENTRY_CTYPE(entry) = ENTRY_TYPE(entry)
	    sym = stenter (stp1, Memc[name+3], ENTRY_LEN)
	    call amovi (Memi[entry], Memi[sym], ENTRY_LEN)
	}
	call close (fd)

	if (tbl != NULL)
	    tp = TBL_TP(tbl)

	# Read the definition file.
	if (catdef[1] == EOS)
	    call strcpy (CATDEF, Memc[fname], SZ_FNAME)
	else
	    call strcpy (catdef, Memc[fname], SZ_FNAME)
	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	stp2 = stopen ("catdefine", 100, ENTRY_LEN, SZ_LINE)
	ncols = 0
	while (fscan(fd) != EOF) {
	    call gargwrd (Memc[name], SZ_FNAME)
	    call gargwrd (Memc[label], SZ_LINE)
	    n = nscan()
	    if (n == 0)
		next
	    if (Memc[name] == '#')
		next

	    # Parse the name.
	    call strcpy (Memc[name], Memc[str], SZ_LINE)
	    call strupr (Memc[str])
	    args = stridxs ("(", Memc[str]) + 1
	    if (args > 1) {
		i = strldxs (")", Memc[str])
		Memc[str+args-2] = EOS
		Memc[str+i-1] = EOS
		func = strdic (Memc[str], Memc[fname], SZ_FNAME, FUNCS)
		if (func == 0) {
		    call strcpy (Memc[name], Memc[str], SZ_LINE)
		    call strupr (Memc[str])
		} else
		    call strcpy (Memc[str+args-1], Memc[str], SZ_LINE)

		args = stridxs ("(", Memc[str]) + 1
		if (args > 1) {
		    i = strldxs (")", Memc[str])
		    Memc[str+args-2] = EOS
		    Memc[str+i-1] = EOS
		    sym = stfind (stp1, Memc[str])
		} else
		    sym = stfind (stp1, Memc[str])
	    } else {
		sym = stfind (stp1, Memc[str])
		func = 0
	    }

	    if (sym == NULL) {
err_
		call stclose (stp1)
		call stclose (stp2)
		call close (fd)
		call sprintf (Memc[label], SZ_LINE,
	"Unknown or ambiguous catalog quantity `%s' in definition file `%s'")
		    call pargstr (Memc[name])
		    call pargstr (Memc[fname])
		call error (1, Memc[label])
	    }
	    ncols = ncols + 1
	    if (tbl == NULL)
		next

	    if (n == 1)
		call strcpy (Memc[name], Memc[label], SZ_LINE)

	    entry = stenter (stp2, Memc[label], ENTRY_LEN)
	    call amovi (Memi[sym], Memi[entry], ENTRY_LEN)
	    ENTRY_FUNC(entry) = func

	    switch (ENTRY_FUNC(entry)) {
	    case FUNC_MAG:
		ENTRY_CTYPE(entry) = TY_REAL
		call strcpy ("magnitudes", ENTRY_UNITS(entry), ENTRY_ULEN)
		ENTRY_FORMAT(entry) = EOS
	    }
	    
	    if (mode == NEW_FILE)
		call tbcdef1 (tp, ENTRY_CDEF(entry), Memc[label],
		    ENTRY_UNITS(sym), ENTRY_FORMAT(sym), ENTRY_CTYPE(sym), 1)
	    else
		call tbcfnd1 (tp, Memc[label], ENTRY_CDEF(entry))

	    # Get arguments.
	    switch (ENTRY_ID(entry)) {
	    case ID_APFLUX:
		if (ctor (Memc[name], args, ENTRY_RAP(entry)) == 0)
		    goto err_
	    }
	}
	call close (fd)
	call stclose (stp1)

	if (tbl == NULL)
	    return

	if (ncols == 0) {
	    call stclose (stp2)
	    call sprintf (Memc[label], SZ_LINE,
		"No catalog quantity definitions in file `%s'")
		call pargstr (Memc[fname])
	    call error (1, Memc[label])
	}

	# Reverse order of symbol table.
	stp1 = stopen ("catdef", ncols, ENTRY_LEN, SZ_LINE)
	for (sym=sthead(stp2); sym!=NULL; sym=stnext(stp2,sym)) {
	    entry = stenter (stp1, Memc[stname(stp2,sym)], ENTRY_LEN)
	    call amovi (Memi[sym], Memi[entry], ENTRY_LEN)
	}
	call stclose (stp2)

	TBL_STP(tbl) = stp1

	call sfree (sp)
end
