include "../../lib/astrom.h"
include "../../lib/acatalog.h"
include "../../lib/aimpars.h"


# AT_GRCPSET -- Read in the region definition parameters. 

procedure at_grcpset (psetname, at)

char	psetname[ARB]		#I the input pset name 
pointer at                      #I the pointer to the main astrom structure

double	dval
pointer	sp, str, pp
int	ival, nchars
double	clgpsetd()
pointer	clopset()
int	ctod(), strlen(), strdic()

begin
        call smark (sp)
        call salloc (str, SZ_FNAME, TY_CHAR)

	pp = clopset (psetname)

	# Get the field center and width.
	call clgpset (pp, "rcra", Memc[str], SZ_FNAME)
	nchars = strlen (Memc[str])
	ival = 1
	if (nchars != 0 && (ctod (Memc[str], ival, dval) == nchars))
	    call at_setd (at, RCRA, dval)
	else
	    call error (0, "Parameter not a legal number (aregpars.rcra)")

	call clgpset (pp, "rcdec", Memc[str], SZ_FNAME)
	nchars = strlen (Memc[str])
	ival = 1
	if (nchars != 0 && (ctod (Memc[str], ival, dval) == nchars))
	    call at_setd (at, RCDEC, dval)
	else
	    call error (0, "Parameter not a legal number (aregpars.rcdec)")

	call at_setd (at, RCRAWIDTH, clgpsetd (pp, "rrawidth"))
	call at_setd (at, RCDECWIDTH, clgpsetd (pp, "rdecwidth"))

	# Get the field center units.
	call clgpset (pp, "rcraunits", Memc[str], SZ_FNAME)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_RA_UNITS) 
	if (ival <= 0)
	    call at_seti (at, RCRAUNITS, 0)
	else
	    call at_seti (at, RCRAUNITS, ival)
	call clgpset (pp, "rcdecunits", Memc[str], SZ_FNAME)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_DEC_UNITS) 
	if (ival <= 0)
	    call at_seti (at, RCDECUNITS, 0)
	else
	    call at_seti (at, RCDECUNITS, ival)

	# Get the field center celestial coordinate system.
	call clgpset (pp, "rcsystem", Memc[str], SZ_FNAME)
	call at_sets (at, RCSYSTEM, Memc[str])

	call clcpset (pp)

        call sfree (sp)
end


# AT_GFSPSET -- Read in the input catalog filtering / selection parameters.

procedure at_gfspset (psetname, at)

char	psetname[ARB]		#I the input pset name 
pointer at                      #I the pointer to the main astrom structure

pointer	sp, str, pp
int	ival
pointer	clopset()
int	btoi(), strdic()
bool	clgpsetb()

begin
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

	pp = clopset (psetname)

	call clgpset (pp, "fields", Memc[str], SZ_LINE)
	call at_sets (at, FIELDS, Memc[str])
	call clgpset (pp, "fexpr", Memc[str], SZ_LINE)
	call at_sets (at, FEXPR, Memc[str])
	call clgpset (pp, "fnames", Memc[str], SZ_LINE)
	call at_sets (at, FNAMES, Memc[str])
	call clgpset (pp, "fntypes", Memc[str], SZ_LINE)
	call at_sets (at, FNTYPES, Memc[str])
	call clgpset (pp, "fnunits", Memc[str], SZ_LINE)
	call at_sets (at, FNUNITS, Memc[str])
	call clgpset (pp, "fnformats", Memc[str], SZ_LINE)
	call at_sets (at, FNFORMATS, Memc[str])

	call clgpset (pp, "fsort", Memc[str], SZ_LINE)
	call at_sets (at, FSORT, Memc[str])
	call at_seti (at, FREVERSE, btoi (clgpsetb (pp, "freverse")))

	#call at_seti (at, FREPLACE, btoi (clgpsetb (pp, "freplace")))
	call clgpset (pp, "fosystem", Memc[str], SZ_LINE)
	call at_sets (at, FOSYSTEM, Memc[str])

	call clgpset (pp, "fira", Memc[str], SZ_LINE)
	call at_sets (at, FIRA, Memc[str])
	call clgpset (pp, "fidec", Memc[str], SZ_LINE)
	call at_sets (at, FIDEC, Memc[str])
	call clgpset (pp, "foraunits", Memc[str], SZ_LINE)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_RA_UNITS) 
	if (ival <= 0)
	    call at_seti (at, FORAUNITS, 0)
	else
	    call at_seti (at, FORAUNITS, ival)
	call clgpset (pp, "fodecunits", Memc[str], SZ_LINE)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_DEC_UNITS) 
	if (ival <= 0)
	    call at_seti (at, FODECUNITS, 0)
	else
	    call at_seti (at, FODECUNITS, ival)
	call clgpset (pp, "foraformat", Memc[str], SZ_LINE)
	call at_sets (at, FORAFORMAT, Memc[str])
	call clgpset (pp, "fodecformat", Memc[str], SZ_LINE)
	call at_sets (at, FODECFORMAT, Memc[str])

	call clgpset (pp, "fixp", Memc[str], SZ_LINE)
	call at_sets (at, FIXP, Memc[str])
	call clgpset (pp, "fiyp", Memc[str], SZ_LINE)
	call at_sets (at, FIYP, Memc[str])
	call clgpset (pp, "fixc", Memc[str], SZ_LINE)
	call at_sets (at, FIXC, Memc[str])
	call clgpset (pp, "fiyc", Memc[str], SZ_LINE)
	call at_sets (at, FIYC, Memc[str])

	call clgpset (pp, "foxformat", Memc[str], SZ_LINE)
	call at_sets (at, FOXFORMAT, Memc[str])
	call clgpset (pp, "foyformat", Memc[str], SZ_LINE)
	call at_sets (at, FOYFORMAT, Memc[str])

	call clcpset (pp)

        call sfree (sp)
end


# AT_GWCPSET -- Read in the default image wcs parameters.

procedure at_gwcpset (psetname, at)

char	psetname[ARB]		#I the input pset name 
pointer at                      #I the pointer to the main astrom structure

double	dval
pointer	sp, str, pp, st, sym
int	ip, ival
pointer clopset(), stopen(), stenter()
int	ctod(), strdic()
bool	streq()

begin
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

	st = stopen ("wcslist", 2 * DEF_LEN_WCST, DEF_LEN_WCST,
	    10 * DEF_LEN_WCST)
	call at_setp (at, WCST, st)

	pp = clopset (psetname)

	call clgpset (pp, "wxref", Memc[str], SZ_LINE)
	sym = stenter (st, "wxref", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WXREF, dval)

	call clgpset (pp, "wyref", Memc[str], SZ_LINE)
	sym = stenter (st, "wyref", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WYREF, dval)

	call clgpset (pp, "wxmag", Memc[str], SZ_LINE)
	sym = stenter (st, "wxmag", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WXMAG, dval)

	call clgpset (pp, "wymag", Memc[str], SZ_LINE)
	sym = stenter (st, "wymag", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WYMAG, dval)

	call clgpset (pp, "wxrot", Memc[str], SZ_LINE)
	sym = stenter (st, "wxrot", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WXROT, dval)

	call clgpset (pp, "wyrot", Memc[str], SZ_LINE)
	sym = stenter (st, "wyrot", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WYROT, dval)

	call clgpset (pp, "wraref", Memc[str], SZ_LINE)
	sym = stenter (st, "wraref", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WRAREF, dval)

	call clgpset (pp, "wdecref", Memc[str], SZ_LINE)
	sym = stenter (st, "wdecref", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, WDECREF, dval)

	call clgpset (pp, "wraunits", Memc[str], SZ_LINE)
	sym = stenter (st, "wraunits", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_RA_UNITS) 
	if (ival <= 0)
	    call at_seti (at, WRAUNITS, 0)
	else
	    call at_seti (at, WRAUNITS, ival)

	call clgpset (pp, "wdecunits", Memc[str], SZ_LINE)
	sym = stenter (st, "wdecunits", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	ival = strdic (Memc[str], Memc[str], SZ_FNAME, AT_DEC_UNITS) 
	if (ival <= 0)
	    call at_seti (at, WDECUNITS, 0)
	else
	    call at_seti (at, WDECUNITS, ival)

	call clgpset (pp, "wproj", Memc[str], SZ_LINE)
	sym = stenter (st, "wproj", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	if (streq (Memc[str], "INDEF"))
	    call at_sets (at, WPROJ, "tan")
	else
	    call at_sets (at, WPROJ, Memc[str])

	call clgpset (pp, "wsystem", Memc[str], SZ_LINE)
	sym = stenter (st, "wsystem", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	if (streq (Memc[str], "INDEF"))
	    call at_sets (at, WSYSTEM, "J2000")
	else
	    call at_sets (at, WSYSTEM, Memc[str])

	call clcpset (pp)

        call sfree (sp)
end



# AT_GIMPSET -- Read in the default image data parameters.

procedure at_gimpset (psetname, at)

char	psetname[ARB]		#I the input pset name 
pointer at                      #I the pointer to the main astrom structure

double	dval
real	rval
pointer	sp, str, pp, st, sym
int	ip
pointer clopset(), stopen(), stenter()
int	ctod(), ctor()

begin
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

	pp = clopset (psetname)
	st = stopen ("imlist", 2 * DEF_LEN_IMST, DEF_LEN_IMST,
	    10 * DEF_LEN_IMST)
	call at_setp (at, IMST, st)

	call clgpset (pp, "esitelng", Memc[str], SZ_LINE)
	sym = stenter (st, "esitelng", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, ESITELNG, dval)

	call clgpset (pp, "esitelat", Memc[str], SZ_LINE)
	sym = stenter (st, "esitelat", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, ESITELAT, dval)

	call clgpset (pp, "esitealt", Memc[str], SZ_LINE)
	sym = stenter (st, "esitealt", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, ESITEALT, rval)

	call clgpset (pp, "esitetz", Memc[str], SZ_LINE)
	sym = stenter (st, "esitetz", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, ESITETZ, rval)

	call clgpset (pp, "emjdobs", Memc[str], SZ_LINE)
	sym = stenter (st, "emjdobs", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctod (Memc[str], ip, dval) <= 0)
	    dval = INDEFD
	call at_setd (at, EMJDOBS, dval)

	call clgpset (pp, "edatamin", Memc[str], SZ_LINE)
	sym = stenter (st, "edatamin", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, EDATAMIN, rval)

	call clgpset (pp, "edatamax", Memc[str], SZ_LINE)
	sym = stenter (st, "edatamax", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, EDATAMAX, rval)

	call clgpset (pp, "egain", Memc[str], SZ_LINE)
	sym = stenter (st, "egain", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, EGAIN, rval)

	call clgpset (pp, "erdnoise", Memc[str], SZ_LINE)
	sym = stenter (st, "erdnoise", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, ERDNOISE, rval)

	call clgpset (pp, "ewavlen", Memc[str], SZ_LINE)
	sym = stenter (st, "ewavlen", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, EWAVLEN, rval)

	call clgpset (pp, "etemp", Memc[str], SZ_LINE)
	sym = stenter (st, "etemp", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, ETEMP, rval)

	call clgpset (pp, "epress", Memc[str], SZ_LINE)
	sym = stenter (st, "epress", LEN_IMST_STRUCT)
	call strcpy (Memc[str], AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	ip = 1
	if (ctor (Memc[str], ip, rval) <= 0)
	    rval = INDEFR
	call at_setr (at, EPRESS, rval)

	call clgpset (pp, "observat", Memc[str], SZ_LINE)
	sym = stenter (st, "observat", LEN_WCST_STRUCT)
	call strcpy (Memc[str], AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	call at_sets (at, OBSERVAT, Memc[str])


	call clcpset (pp)

        call sfree (sp)
end
