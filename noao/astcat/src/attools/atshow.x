include "../../lib/astrom.h"
include "../../lib/acatalog.h"
include "../../lib/aimpars.h"

# AT_IMSHOW -- Print the current default WCS parameters.

procedure at_imshow (at)

pointer	at			#I the astrometry package descriptor

pointer	sp, str1, str2
double	at_statd()
real	at_statr()
pointer	at_statp()

begin
	if (at_statp(at, PIMPARS) == NULL)
	    return

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call printf ("\nDefault Image Data Parameters\n")
	call at_stats (at, OBSERVAT, Memc[str1], SZ_FNAME)
	call printf ("    The observatory id: %s  MJD: %0.5f\n")
	    call pargstr (Memc[str1])
	    call pargd (at_statd (at, EMJDOBS))
	call printf (
	"    The site longitude and latitude: %0.2h %0.2h (degrees degrees)\n")
	    call pargd (at_statd(at, ESITELNG))
	    call pargd (at_statd(at, ESITELAT))
	call printf (
	    "    The site altitude and time zone: %0.1f %0.1f (m hours)\n")
	    call pargr (at_statr(at, ESITEALT))
	    call pargr (at_statr(at, ESITETZ))
	call printf (
	    "    Effective wavelength: %0.2f (microns)\n")
	    call pargr (at_statr (at, EWAVLEN))
	call printf (
	    "    Effective tempaerature and pressure: %0.2f %0.2f (K mbars)\n")
	    call pargr (at_statr (at, ETEMP))
	    call pargr (at_statr (at, EPRESS))
	call printf (
	    "    Effective gain and readout noise: %0.2f %0.2f (e-/ADU  e-)\n")
	    call pargr (at_statr (at, EGAIN))
	    call pargr (at_statr (at, ERDNOISE))
	call printf (
	    "    Low and high good data limits: %0.2f %0.2f (ADU ADU)\n")
	    call pargr (at_statr (at, EDATAMIN))
	    call pargr (at_statr (at, EDATAMAX))

	call sfree (sp)
end


# AT_WCSHOW -- Print the current default WCS parameters.

procedure at_wcshow (at)

pointer	at			#I the astrometry package descriptor

pointer	sp, str1, str2
int	ival
double	at_statd()
pointer	at_statp()
int	at_stati(), at_wrdstr()

begin
	if (at_statp(at, PWCS) == NULL)
	    return

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call printf ("\nDefault WCS Parameters\n")
	call at_stats (at, WPROJ, Memc[str1], SZ_FNAME)
	call printf ("    Sky projection geometry: %s\n")
	    call pargstr (Memc[str1])
	ival = at_stati (at, WRAUNITS)
	if (ival <= 0)
	    #Memc[str1] = EOS
	    call strcpy ("default", Memc[str1], SZ_FNAME)
	else if (at_wrdstr (ival, Memc[str1], SZ_FNAME, AT_RA_UNITS) <= 0)
	    #Memc[str1] = EOS
	    call strcpy ("default", Memc[str1], SZ_FNAME)
	ival = at_stati (at, WDECUNITS)
	if (ival <= 0)
	    #Memc[str2] = EOS
	    call strcpy ("default", Memc[str2], SZ_FNAME)
	else if (at_wrdstr (ival, Memc[str2], SZ_FNAME, AT_DEC_UNITS) <= 0)
	    #Memc[str2] = EOS
	    call strcpy ("default", Memc[str2], SZ_FNAME)
	call printf ("    Reference point: %0.3h %0.2h (%s %s)\n")
	    call pargd (at_statd(at, WRAREF))
	    call pargd (at_statd(at, WDECREF))
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call printf ("    Reference point: %0.3f %0.3f (pixels pixels)\n")
	    call pargd (at_statd(at, WXREF))
	    call pargd (at_statd(at, WYREF))
	call printf (
	    "    X and Y scale: %0.3f %0.3f (arcsec/pixel arsec/pixel)\n")
	    call pargd (at_statd(at, WXMAG))
	    call pargd (at_statd(at, WYMAG))
	call printf (
	    "    X and Y axis rotation: %0.3f %0.3f (degrees degrees)\n")
	    call pargd (at_statd(at, WXROT))
	    call pargd (at_statd(at, WYROT))
	call at_stats (at, WSYSTEM, Memc[str1], SZ_FNAME)
	call printf ("    System: %s\n")
	    call pargstr (Memc[str1])

	call sfree (sp)
end


# AT_IOSHOW -- Print the current i/o parameters.

procedure at_ioshow (at)

pointer	at			#I the astrometry package descriptor

pointer	sp, str1, str2
pointer	at_statp()

begin
	if (at_statp(at, PIO) == NULL)
	    return

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call printf ("\nI/O Parameters\n")

	call at_stats (at, CATDB, Memc[str1], SZ_FNAME)
	call printf ("    catdb = %s\n")
	    call pargstr (Memc[str1])
	call at_stats (at, CATALOGS, Memc[str1], SZ_FNAME)
	call at_stats (at, CATNAME, Memc[str2], SZ_FNAME)
	call printf ("    catalog = %s  catname = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, IMDB, Memc[str1], SZ_FNAME)
	call printf ("    imdb = %s\n")
	    call pargstr (Memc[str1])
	call at_stats (at, SURVEYS, Memc[str1], SZ_FNAME)
	call at_stats (at, SVNAME, Memc[str2], SZ_FNAME)
	call printf ("    survey = %s  svname = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, IMAGES, Memc[str1], SZ_FNAME)
	call at_stats (at, IMNAME, Memc[str2], SZ_FNAME)
	call printf ("    images = %s  imname = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, INPUT, Memc[str1], SZ_FNAME)
	call at_stats (at, INFNAME, Memc[str2], SZ_FNAME)
	call printf ("    input = %s  infname = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, OUTPUT, Memc[str1], SZ_FNAME)
	call at_stats (at, OUTFNAME, Memc[str2], SZ_FNAME)
	call printf ("    output = %s  outfname = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call sfree (sp)
end


# AT_RCSHOW -- Print the current field center parameters.

procedure at_rcshow (at)

pointer	at			#I the astrometry package descriptor

pointer	sp, str1, str2
int	ival
double	at_statd()
pointer	at_statp()
int	at_wrdstr(), at_stati()

begin
	if (at_statp(at, PRCENTER) == NULL)
	    return

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call printf ("\nField Center Parameters\n")
	call at_stats (at, RCSOURCE, Memc[str1], SZ_FNAME)
	call at_stats (at, RCSYSTEM, Memc[str2], SZ_FNAME)
	call printf ("    rcsource = %s  rcsystem = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call printf ("    rcra = %h  rcdec = %h\n")
	    call pargd (at_statd (at, RCRA))
	    call pargd (at_statd (at, RCDEC))
	call printf ("    rrawidth = %0.2f  rdecwidth = %0.2f\n")
	    call pargd (at_statd (at, RCRAWIDTH))
	    call pargd (at_statd (at, RCDECWIDTH))

	ival = at_stati (at, RCRAUNITS)
	if (ival <= 0)
	    #Memc[str1] = EOS
	    call strcpy ("default", Memc[str1], SZ_FNAME)
	else if (at_wrdstr (ival, Memc[str1], SZ_FNAME, AT_RA_UNITS) <= 0)
	    #Memc[str1] = EOS
	    call strcpy ("default", Memc[str1], SZ_FNAME)
	ival = at_stati (at, RCDECUNITS)
	if (ival <= 0)
	    #Memc[str2] = EOS
	    call strcpy ("default", Memc[str2], SZ_FNAME)
	else if (at_wrdstr (ival, Memc[str2], SZ_FNAME, AT_DEC_UNITS) <= 0)
	    #Memc[str2] = EOS
	    call strcpy ("default", Memc[str2], SZ_FNAME)
	call printf ("    rcraunits = %s  rcdecunits = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call printf ("\n")

	call sfree (sp)
end


# AT_FSSHOW -- Print the current filtering parameters.

procedure at_fsshow (at)

pointer	at			#I the astrometry package descriptor

pointer	sp, str1, str2
int	ival
pointer	at_statp()
int	at_stati(), at_wrdstr()
bool	itob()

begin
	if (at_statp(at, PFILTER) == NULL)
	    return

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call printf ("\nRecord Filtering Parameters\n")
	call at_stats (at, FIELDS, Memc[str1], SZ_FNAME)
	call at_stats (at, FEXPR, Memc[str2], SZ_FNAME)
	call printf ("    fields = %s  fexpr = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call at_stats (at, FNAMES, Memc[str1], SZ_FNAME)
	call at_stats (at, FNTYPES, Memc[str2], SZ_FNAME)
	call printf ("    fnames = %s fntypes = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call at_stats (at, FNUNITS, Memc[str1], SZ_FNAME)
	call at_stats (at, FNFORMATS, Memc[str2], SZ_FNAME)
	call printf ("    fnunits = %s fnformats = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, FSORT, Memc[str1], SZ_FNAME)
	call printf ("    fsort = %s  freverse = %b\n")
	    call pargstr (Memc[str1])
	    call pargb (itob(at_stati(at, FREVERSE)))

	call at_stats (at, FOSYSTEM, Memc[str1], SZ_FNAME)
	call printf ("    freplace = %b  fosystem = %s\n")
	    call pargb (itob(at_stati(at, FREPLACE)))
	    call pargstr (Memc[str1])
	call at_stats (at, FIRA, Memc[str1], SZ_FNAME)
	call at_stats (at, FIDEC, Memc[str2], SZ_FNAME)
	call printf ("    fira = %s  fidec = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	ival = at_stati (at, FORAUNITS)
	if (ival <= 0)
	    Memc[str1] = EOS
	else if (at_wrdstr (ival, Memc[str1], SZ_FNAME, AT_RA_UNITS) <= 0)
	    Memc[str1] = EOS
	ival = at_stati (at, FODECUNITS)
	if (ival <= 0)
	    Memc[str2] = EOS
	else if (at_wrdstr (ival, Memc[str2], SZ_FNAME, AT_DEC_UNITS) <= 0)
	    Memc[str2] = EOS
	call printf ("    foraunits = %s  fodecunits = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call at_stats (at, FORAFORMAT, Memc[str1], SZ_FNAME)
	call at_stats (at, FODECFORMAT, Memc[str2], SZ_FNAME)
	call printf ("    foraformat = %s  fodecformat = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call at_stats (at, FIXP, Memc[str1], SZ_FNAME)
	call at_stats (at, FIYP, Memc[str2], SZ_FNAME)
	call printf ("    fixp = %s  fiyp = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call at_stats (at, FIXC, Memc[str1], SZ_FNAME)
	call at_stats (at, FIYC, Memc[str2], SZ_FNAME)
	call printf ("    fixc = %s  fiyc = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call at_stats (at, FOXFORMAT, Memc[str1], SZ_FNAME)
	call at_stats (at, FOYFORMAT, Memc[str2], SZ_FNAME)
	call printf ("    foxformat = %s  foyformat = %s\n")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])

	call printf ("\n")

	call sfree (sp)
end


# AT_STSHOW -- Dump the contents of the field center symbol table

procedure at_stshow (at)

pointer	at			#I the astrometry package descriptor

pointer	st, symlist, symbol
int	i, nfields
pointer	at_statp(), sthead(), stnext()
int	stnsymbols()

begin
	if (at_statp(at, PRCENTER) == NULL)
	    return

	st = at_statp(at, RCST)
	if (st == NULL)
	    return

	nfields = stnsymbols (st, 0)
	if (nfields <= 0)
	    return

	call malloc (symlist, nfields, TY_INT)
	symbol = sthead (st)
	do i = 1, nfields {
	    Memi[symlist+i-1] = symbol
	    symbol = stnext (st, symbol)
	}

	do i = nfields, 1, -1 {
	    symbol = Memi[symlist+i-1]
	    call printf ("%s %s\n")
		call pargstr (AT_RCSTSOURCE(symbol))
		call pargstr (AT_RCSTNAME(symbol))
	    call printf ("    %h %h  %8.3f %8.3f  %d %d  %s\n")
		call pargd (AT_RCSTRA(symbol))
		call pargd (AT_RCSTDEC(symbol))
		call pargd (AT_RCSTRAWIDTH(symbol))
		call pargd (AT_RCSTDECWIDTH(symbol))
		call pargi (AT_RCSTRAUNITS(symbol))
		call pargi (AT_RCSTDECUNITS(symbol))
		call pargstr (AT_RCSTSYSTEM(symbol))
	}

	call mfree (symlist, TY_INT)
end
