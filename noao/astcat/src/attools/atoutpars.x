include "../../lib/astrom.h"
include "../../lib/acatalog.h"
include "../../lib/aimpars.h"


# AT_PRCPSET -- Write the current parameter values out to the region
# parameters set. 

procedure at_prcpset (psetname, at)

char    psetname[ARB]           #I the parameter set name
pointer at                      #I the pointer to the main astrom structure

pointer	sp, str, pp
int	ival
double	at_statd()
pointer	clopset()
int	at_wrdstr(), at_stati()

begin
        call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	pp = clopset ("psetname")

	# Update the field center and field width parameters.
	call clppsetd (pp, "rcra", at_statd (at, RCRA))
	call clppsetd (pp, "rcdec", at_statd (at, RCDEC))
	call clppsetd (pp, "rrawidth", at_statd (at, RCRAWIDTH))
	call clppsetd (pp, "rdecwidth", at_statd (at, RCDECWIDTH))

	# Update the units parameters.
	ival = at_stati (at, RCRAUNITS)
	if (ival <= 0)
	    Memc[str] = EOS
	else if (at_wrdstr (ival,  Memc[str], SZ_FNAME, AT_RA_UNITS) <= 0)
	    Memc[str] = EOS
	call clppset (pp, "rcraunits", Memc[str])

	ival = at_stati (at, RCDECUNITS)
	if (ival <= 0)
	    Memc[str] = EOS
	else if (at_wrdstr (ival,  Memc[str], SZ_FNAME, AT_DEC_UNITS) <= 0)
	    Memc[str] = EOS
	call clppset (pp, "rcdecunits", Memc[str])

	# Update the celestial coordinate system.
	call at_stats (at, RCSYSTEM, Memc[str], SZ_FNAME)
	call clppset (pp, "rcsystem", Memc[str])

	call clcpset (pp)

	call sfree (sp)
end


# AT_PFSPSET -- Write the current parameter values out to the filtering 
# parameters parameter set. 

procedure at_pfspset (psetname, at)

char    psetname[ARB]           #I the parameter set name
pointer at                      #I the pointer to the main astrom structure

pointer	sp, str, pp
int	ival
pointer	clopset()
int	at_stati(), at_wrdstr()
bool	itob()

begin
        call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	pp = clopset ("psetname")

	call at_stats (pp, FIELDS, Memc[str], SZ_LINE)
	call clppset (pp, "fields", Memc[str])
	call at_stats (pp, FEXPR, Memc[str], SZ_LINE)
	call clppset (pp, "fexpr", Memc[str])
	call at_stats (pp, FNAMES, Memc[str], SZ_LINE)
	call clppset (pp, "fnames", Memc[str])
	call at_stats (pp, FNTYPES, Memc[str], SZ_LINE)
	call clppset (pp, "fntypes", Memc[str])
	call at_stats (pp, FNUNITS, Memc[str], SZ_LINE)
	call clppset (pp, "fnunits", Memc[str])
	call at_stats (pp, FNFORMATS, Memc[str], SZ_LINE)
	call clppset (pp, "fnformats", Memc[str])

	call at_stats (pp, FSORT, Memc[str], SZ_LINE)
	call clppset (pp, "fsort", Memc[str])
	call clppsetb (pp, "freverse", itob(at_stati(at,FREVERSE)))

	#call clppsetb (pp, "freplace", itob(at_stati(at,FREPLACE)))
	call at_stats (pp, FOSYSTEM, Memc[str], SZ_LINE)
	call clppset (pp, "fosystem", Memc[str])
	call at_stats (pp, FIRA, Memc[str], SZ_LINE)
	call clppset (pp, "fira", Memc[str])
	call at_stats (pp, FIDEC, Memc[str], SZ_LINE)
	call clppset (pp, "fidec", Memc[str])
	ival = at_wrdstr (at_stati(at,FORAUNITS),  Memc[str], SZ_FNAME,
	    AT_RA_UNITS)
	if (ival <= 0)
	    call clppset (pp, "foraunits", "") 
	else
	    call clppset (pp, "foraunits", Memc[str])
	ival = at_wrdstr (at_stati(at,FODECUNITS),  Memc[str], SZ_FNAME,
	    AT_DEC_UNITS)
	if (ival <= 0)
	    call clppset (pp, "fodecunits", "") 
	else
	    call clppset (pp, "fodecunits", Memc[str])
	call at_stats (pp, FORAFORMAT, Memc[str], SZ_LINE)
	call clppset (pp, "foraformats", Memc[str])
	call at_stats (pp, FODECFORMAT, Memc[str], SZ_LINE)
	call clppset (pp, "fodecformats", Memc[str])

	call at_stats (pp, FIXP, Memc[str], SZ_LINE)
	call clppset (pp, "fixp", Memc[str])
	call at_stats (pp, FIYP, Memc[str], SZ_LINE)
	call clppset (pp, "fiyp", Memc[str])
	call at_stats (pp, FIXC, Memc[str], SZ_LINE)
	call clppset (pp, "fixc", Memc[str])
	call at_stats (pp, FIYC, Memc[str], SZ_LINE)
	call clppset (pp, "fiyc", Memc[str])
	call at_stats (pp, FOXFORMAT, Memc[str], SZ_LINE)
	call clppset (pp, "foxformat", Memc[str])
	call at_stats (pp, FOYFORMAT, Memc[str], SZ_LINE)
	call clppset (pp, "foyformat", Memc[str])

	call clcpset (pp)

	call sfree (sp)
end


# AT_PWCPSET -- Write the current parameter values out to the default WCS
# parameters parameter set. 

procedure at_pwcpset (psetname, at)

char    psetname[ARB]           #I the parameter set name
pointer at                      #I the pointer to the main astrom structure

pointer	pp, st, sym
pointer	clopset(), at_statp(), stfind()


begin
	pp = clopset ("psetname")
	st = at_statp (at, WCST)

	sym = stfind (st, "wxref")
	call clppset (pp, "wxref", AT_WCSTKVAL(sym))

	sym = stfind (st, "wyref")
	call clppset (pp, "wyref", AT_WCSTKVAL(sym))

	sym = stfind (st, "wxmag")
	call clppset (pp, "wxmag", AT_WCSTKVAL(sym))

	sym = stfind (st, "wymag")
	call clppset (pp, "wymag", AT_WCSTKVAL(sym))

	sym = stfind (st, "wxrot")
	call clppset (pp, "wxrot", AT_WCSTKVAL(sym))

	sym = stfind (st, "wyrot")
	call clppset (pp, "wyrot", AT_WCSTKVAL(sym))

	sym = stfind (st, "wraref")
	call clppset (pp, "wraref", AT_WCSTKVAL(sym))

	sym = stfind (st, "wdecref")
	call clppset (pp, "wdecref", AT_WCSTKVAL(sym))

	sym = stfind (st, "wraunits")
	call clppset (pp, "wraunits", AT_WCSTKVAL(sym))

	sym = stfind (st, "wdecunits")
	call clppset (pp, "wdecunits", AT_WCSTKVAL(sym))

	sym = stfind (st, "wproj")
	call clppset (pp, "wproj", AT_WCSTKVAL(sym))

	sym = stfind (st, "wsystem")
	call clppset (pp, "wsystem", AT_WCSTKVAL(sym))

	call clcpset (pp)
end


# AT_PIMPSET -- Write the current parameter values out to the default WCS
# parameters parameter set. 

procedure at_pimpset (psetname, at)

char    psetname[ARB]           #I the parameter set name
pointer at                      #I the pointer to the main astrom structure

pointer	pp, st, sym
pointer	clopset(), at_statp(), stfind()


begin
	pp = clopset ("psetname")
	st = at_statp (at, IMST)

	sym = stfind (st, "esitelng")
	call clppset (pp, "esitelng", AT_IMSTKVAL(sym))

	sym = stfind (st, "esitelat")
	call clppset (pp, "esitelat", AT_IMSTKVAL(sym))

	sym = stfind (st, "emjdobs")
	call clppset (pp, "emjdobs", AT_IMSTKVAL(sym))

	#sym = stfind (st, "ut")
	#call clppset (pp, "ut", AT_IMSTKVAL(sym))

	sym = stfind (st, "esitealt")
	call clppset (pp, "esitealt", AT_IMSTKVAL(sym))

	sym = stfind (st, "esitetz")
	call clppset (pp, "esitetz", AT_IMSTKVAL(sym))

	#sym = stfind (st, "exposure")
	#call clppset (pp, "exposure", AT_IMSTKVAL(sym))

	sym = stfind (st, "edatamin")
	call clppset (pp, "edatamin", AT_IMSTKVAL(sym))

	sym = stfind (st, "edatamax")
	call clppset (pp, "edatamax", AT_IMSTKVAL(sym))

	sym = stfind (st, "egain")
	call clppset (pp, "egain", AT_IMSTKVAL(sym))

	sym = stfind (st, "erdnoise")
	call clppset (pp, "erdnoise", AT_IMSTKVAL(sym))

	sym = stfind (st, "ewavlen")
	call clppset (pp, "ewavlen", AT_IMSTKVAL(sym))

	sym = stfind (st, "etemp")
	call clppset (pp, "etemp", AT_IMSTKVAL(sym))

	sym = stfind (st, "epress")
	call clppset (pp, "epress", AT_IMSTKVAL(sym))

	sym = stfind (st, "observat")
	call clppset (pp, "observat", AT_IMSTKVAL(sym))

	#sym = stfind (st, "dateobs")
	#call clppset (pp, "dateobs", AT_IMSTKVAL(sym))

	call clcpset (pp)
end
