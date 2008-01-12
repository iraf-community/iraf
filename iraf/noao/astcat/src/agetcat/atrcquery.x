include <math.h>
include <pkg/cq.h>
include <pkg/skywcs.h>
include "../../lib/astrom.h"
include "../../lib/acatalog.h"

# AT_RCQUERY -- Format the catalog query for the specified field using
# field data supplied by the user and stored in a symbol table and query
# information stored in the catalog database.

int procedure at_rcquery (at, cq, fieldno)

pointer	at			#I the astrometry pacakge descriptor
pointer	cq			#I the database descriptor
int	fieldno			#I the field number descriptor

double	ra, dec, width
pointer	sp, qsystem, fsystem, qpname, qpvalue, qpunits, qpformats, raformats
pointer	decformats, symbol, qcoo, fcoo, mw
int	i, stat, parno, units, nqpars 

pointer	stfind(), at_statp()
int	sk_decwcs(), cq_nqpars(), cq_gqparn(), cq_sqpar(), strdic(), at_wrdstr()
int	sk_stati()
bool	streq()
errchk	cq_fgwrd()

begin
	call smark (sp)

	# Allocate space for the coordinate system descriptions.
	call salloc (qsystem, SZ_FNAME, TY_CHAR)
	call salloc (fsystem, SZ_FNAME, TY_CHAR)

	# Fetch the field center symbol.
	call sprintf (Memc[qsystem], SZ_FNAME, "%s%d")
	    call pargstr (DEF_RCST_ROOTNAME)
	    call pargi (fieldno)
	symbol = stfind (at_statp(at, RCST), Memc[qsystem])
	if (symbol == NULL) {
	    call sfree (sp)
	    return (ERR)
	}

	# Determine the query coordinate system. If the query coordinate system
	# is undefined, set it to the current catalog coordinate system. If
	# the catalog system is undefined set it to the global default.
	iferr (call cq_fgwrd (cq, "qsystem", Memc[qsystem], SZ_FNAME)) {
	    iferr (call cq_fgwrd (cq, "csystem", Memc[qsystem], SZ_FNAME)) 
		call strcpy ("DEF_CATSYSTEM", Memc[qsystem], SZ_FNAME)
	}
	if (Memc[qsystem] == EOS || streq (Memc[qsystem], "INDEF"))
	    call strcpy ("DEF_CATSYSTEM", Memc[qsystem], SZ_FNAME)

	# Open the query coordinate system data structure.
	stat = sk_decwcs (Memc[qsystem], mw, qcoo, NULL)
	if (stat == ERR || mw != NULL) {
	    if (mw != NULL)
		call mw_close (mw)
	    call sk_close (qcoo)
	    call sfree (sp)
	    return (ERR)
	}

	# Determine the field center coordinate system. If the field center
	# coordinate system is undefined, set it to the query coordinate
	# system.
	if (AT_RCSTSYSTEM(symbol) == EOS || streq (AT_RCSTSYSTEM(symbol),
	    "INDEF")) 
	    call strcpy (Memc[qsystem], Memc[fsystem], SZ_FNAME)
	else
	    call strcpy (AT_RCSTSYSTEM(symbol), Memc[fsystem], SZ_FNAME)

	# Open the field center coordinate system data structure.
	stat = sk_decwcs (Memc[fsystem], mw, fcoo, NULL)
	if (stat == ERR || mw != NULL) {
	    if (mw != NULL)
		call mw_close (mw)
	    call sk_close (fcoo)
	    call sk_close (qcoo)
	    call sfree (sp)
	    return (ERR)
	}

	# Allocate space for the query parameter description.
	call salloc (qpname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (qpvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (qpunits, CQ_SZ_QPUNITS, TY_CHAR)
	call salloc (qpformats, CQ_SZ_QPFMTS, TY_CHAR)
	call salloc (raformats, CQ_SZ_QPFMTS, TY_CHAR)
	call salloc (decformats, CQ_SZ_QPFMTS, TY_CHAR)

	# Loop through the query parameter list encoding the non-coordinate
	# system parameters.
	nqpars = cq_nqpars (cq)
	do i = 1, nqpars {

	    # Get the query parameter description.
	    if (cq_gqparn (cq, i, Memc[qpname], CQ_SZ_QPNAME, Memc[qpvalue],
	        CQ_SZ_QPVALUE, Memc[qpunits], CQ_SZ_QPUNITS, Memc[qpformats],
		CQ_SZ_QPFMTS) != i)
		next

	    parno = strdic (Memc[qpname], Memc[qpname], CQ_SZ_QPNAME,
		AT_QRCFIELDS)
	    if (parno <= 0)
		next

	    # Field center right ascension. Set the units and save the format
	    # for later use since we cannot perform the coordinate
	    # transformation until both ra and dec units are decoded.
	    switch (parno) {
	    case AT_QRCRA:
		units = strdic (Memc[qpunits], Memc[qpunits], CQ_SZ_QPUNITS,
		    SKY_LNG_UNITLIST)
		if (units > 0)
		    call sk_seti (qcoo, S_NLNGUNITS, units)
		switch (AT_RCSTRAUNITS(symbol)) {
		case AT_DEGREES:
		    units = SKY_DEGREES
		case AT_RADIANS:
		    units = SKY_RADIANS
		case AT_HOURS:
		    units = SKY_HOURS
		default:
		    units = sk_stati (fcoo, S_NLNGUNITS)
		}
		call sk_seti (fcoo, S_NLNGUNITS, units)

		call strcpy (Memc[qpformats], Memc[raformats], CQ_SZ_QPFMTS)

	    # Field center declination. Set the units and save the format
	    # for later use since we cannot perform the coordinate 
	    # transformation until both ra and dec units are decoded.
	    case AT_QRCDEC:
		units = strdic (Memc[qpunits], Memc[qpunits], CQ_SZ_QPUNITS,
		    SKY_LAT_UNITLIST)
		if (units > 0)
		    call sk_seti (qcoo, S_NLATUNITS, units)
		switch (AT_RCSTDECUNITS(symbol)) {
		case AT_DEGREES:
		    units = SKY_DEGREES
		case AT_RADIANS:
		    units = SKY_RADIANS
		default:
		    units = sk_stati (fcoo, S_NLATUNITS)
		}
		call sk_seti (fcoo, S_NLATUNITS, units)

		call strcpy (Memc[qpformats], Memc[decformats], CQ_SZ_QPFMTS)

	    # Width. Input units are minutes. Output units are minutes or
	    # degrees.
	    case AT_QRCWIDTH:
		width = max (AT_RCSTRAWIDTH(symbol), AT_RCSTDECWIDTH(symbol))
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Radius. Input units are minutes. Output units are minutes or
	    # degrees.
	    case AT_QRCRADIUS:
		width = max (AT_RCSTRAWIDTH(symbol),
		    AT_RCSTDECWIDTH(symbol)) / 2.0d0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Half width. Input units are minutes. Output units are minutes or
	    # degrees.
	    case AT_QRCHWIDTH:
		width = max (AT_RCSTRAWIDTH(symbol),
		    AT_RCSTDECWIDTH(symbol)) / 2.0d0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Ra width. Input units are minutes. Output units are minutes or
	    # degrees.
	    case AT_QRCRAWIDTH:
		width = AT_RCSTRAWIDTH(symbol)
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Dec width. Input units are minutes. Output units are minutes or
	    # degrees.
	    case AT_QRCDECWIDTH:
		width = AT_RCSTDECWIDTH(symbol)
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Ra half width. Input units are minutes. Output units are minutes
	    # or degrees.
	    case AT_QRCRAHWIDTH:
		width = AT_RCSTRAWIDTH(symbol) / 2.0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Dec half width. Input units are minutes. Output units are minutes
	    # or degrees.
	    case AT_QRCDECHWIDTH:
		width = AT_RCSTDECWIDTH(symbol) / 2.0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

            # X width. Input units are minutes. Output units are minutes
            # or degrees.
            case AT_QRCXWIDTH:
                width = AT_RCSTRAWIDTH(symbol)
                if (streq (Memc[qpunits], "degrees"))
                    width = width / 60.0d0
                call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats])
                    call pargd (width)
                if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
                    ;

            # Y width. Input units are minutes. Output units are minutes
            # or degrees.
            case AT_QRCYWIDTH:
                width = AT_RCSTDECWIDTH(symbol)
                if (streq (Memc[qpunits], "degrees"))
                    width = width / 60.0d0
                call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats])
                    call pargd (width)
                if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
                    ;

	    # X half width. Input units are minutes. Output units are minutes
	    # or degrees.
	    case AT_QRCXHWIDTH:
		width = AT_RCSTRAWIDTH(symbol) / 2.0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;

	    # Y half width. Input units are minutes. Output units are minutes
	    # or degrees.
	    case AT_QRCYHWIDTH:
		width = AT_RCSTDECWIDTH(symbol) / 2.0
		if (streq (Memc[qpunits], "degrees"))
		    width = width / 60.0d0
		call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpformats]) 
		    call pargd (width)
		if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) != i)
		    ;


	    }

	}

	# Transform the ra and dec from the field center coordinate system to
	# the query coordinate system and reformat the query.
	call sk_ultran (fcoo, qcoo, AT_RCSTRA(symbol), AT_RCSTDEC(symbol), ra,
	    dec, 1)
	if (at_wrdstr (AT_QRCRA, Memc[qpname], CQ_SZ_QPNAME,
	    AT_QRCFIELDS) > 0) {
	    call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[raformats])
	        call pargd (ra)
	    if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) == 0)
	        ;
	}
	if (at_wrdstr (AT_QRCDEC, Memc[qpname], CQ_SZ_QPNAME,
	    AT_QRCFIELDS) > 0) {
	    call sprintf (Memc[qpvalue], CQ_SZ_QPVALUE, Memc[decformats])
	        call pargd (dec)
	    if (cq_sqpar (cq, Memc[qpname], Memc[qpvalue]) == 0)
	        ;
	}
	
	# Cleanup.
	call sk_close (fcoo)
	call sk_close (qcoo)
	call sfree (sp)

	return (OK)
end


# AT_RCREGION -- Determine the region extraction parameters for the specified
# field using field data supplied by the user and stored in a symbol table and
# information stored in the catalog .

int procedure at_rcregion (at, cres, fieldno, ra, dec, rawidth, decwidth)

pointer	at			#I the astrometry pacakge descriptor
pointer	cres			#I the catalog results descriptor
int	fieldno			#I the field number descriptor
double	ra			#O the field center ra in degrees
double	dec			#O the field center dec in degrees
double	rawidth			#O the field ra width in degrees
double	decwidth		#O the field dec width in degrees

pointer	sp, qsystem, fsystem, raname, decname, raunits, decunits
pointer	symbol, mw, qcoo, fcoo
int	stat, units
pointer	stfind(), at_statp()
int	sk_decwcs(), cq_hinfo(), strdic(), sk_stati()
bool	streq()
errchk	at_stats()

begin
	call smark (sp)

	# Allocate space for the coordinate system descriptions.
	call salloc (qsystem, SZ_FNAME, TY_CHAR)
	call salloc (fsystem, SZ_FNAME, TY_CHAR)
	call salloc (raname, CQ_SZ_FNAME, TY_CHAR)
	call salloc (decname, CQ_SZ_FNAME, TY_CHAR)
	call salloc (raunits, CQ_SZ_FNAME, TY_CHAR)
	call salloc (decunits, CQ_SZ_FNAME, TY_CHAR)

	# Fetch the field center symbol.
	call sprintf (Memc[qsystem], SZ_FNAME, "%s%d")
	    call pargstr (DEF_RCST_ROOTNAME)
	    call pargi (fieldno)
	symbol = stfind (at_statp(at, RCST), Memc[qsystem])
	if (symbol == NULL) {
	    call sfree (sp)
	    return (ERR)
	}

	# Set the query coordinate system to the catalog coordinate system.
	# the catalog system is undefined set it to the global default.
	if (cq_hinfo (cres, "csystem", Memc[qsystem], SZ_FNAME) <= 0)
	    Memc[qsystem] = EOS
	if (Memc[qsystem] == EOS || streq (Memc[qsystem], "INDEF"))
	    call strcpy ("DEF_CATSYSTEM", Memc[qsystem], SZ_FNAME)

	# Open the query coordinate system data structure.
	stat = sk_decwcs (Memc[qsystem], mw, qcoo, NULL)
	if (stat == ERR || mw != NULL) {
	    if (mw != NULL)
		call mw_close (mw)
	    call sk_close (qcoo)
	    call sfree (sp)
	    return (ERR)
	}

	# Determine the field center coordinate system. If the field center
	# coordinate system is undefined, set it to the query coordinate
	# system.
	if (AT_RCSTSYSTEM(symbol) == EOS || streq (AT_RCSTSYSTEM(symbol),
	    "INDEF")) 
	    call strcpy (Memc[qsystem], Memc[fsystem], SZ_FNAME)
	else
	    call strcpy (AT_RCSTSYSTEM(symbol), Memc[fsystem], SZ_FNAME)

	# Open the field center coordinate system data structure.
	stat = sk_decwcs (Memc[fsystem], mw, fcoo, NULL)
	if (stat == ERR || mw != NULL) {
	    if (mw != NULL)
		call mw_close (mw)
	    call sk_close (fcoo)
	    call sk_close (qcoo)
	    call sfree (sp)
	    return (ERR)
	}

	# Get the names of the columns containing ra and dec.
	iferr (call at_stats (at, FIRA, Memc[raname], CQ_SZ_FNAME))
	    call strcpy ("ra", Memc[raname], SZ_FNAME)
	iferr (call at_stats (at, FIDEC, Memc[decname], CQ_SZ_FNAME))
	    call strcpy ("dec", Memc[decname], SZ_FNAME)
	
	# Get the query ra units.
	call cq_funits (cres,  Memc[raname], Memc[raunits], CQ_SZ_FUNITS)
	units = strdic (Memc[raunits], Memc[raunits], CQ_SZ_FUNITS,
	    SKY_LNG_UNITLIST)
	if (units > 0)
	    call sk_seti (qcoo, S_NLNGUNITS, units)
	else
	    units = sk_stati (qcoo, S_NLNGUNITS)
	switch (AT_RCSTRAUNITS(symbol)) {
	case AT_DEGREES:
	    units = SKY_DEGREES
	case AT_RADIANS:
	    units = SKY_RADIANS
	case AT_HOURS:
	    units = SKY_HOURS
	default:
	    ;
	}
	call sk_seti (fcoo, S_NLNGUNITS, units)

	# Get the query dec units.
	call cq_funits (cres,  Memc[decname], Memc[decunits], CQ_SZ_FUNITS)
	units = strdic (Memc[decunits], Memc[decunits], CQ_SZ_FUNITS,
	    SKY_LAT_UNITLIST)
	if (units > 0)
	    call sk_seti (qcoo, S_NLATUNITS, units)
	else
	    units = sk_stati (qcoo, S_NLATUNITS)
	switch (AT_RCSTDECUNITS(symbol)) {
	case AT_DEGREES:
	    units = SKY_DEGREES
	case AT_RADIANS:
	    units = SKY_RADIANS
	case AT_HOURS:
	    units = SKY_HOURS
	default:
	    ;
	}
	call sk_seti (fcoo, S_NLATUNITS, units)

	# Transform the ra and dec from the field center coordinate system to
	# the query coordinate system and convert the units to degrees.
	call sk_ultran (fcoo, qcoo, AT_RCSTRA(symbol), AT_RCSTDEC(symbol), ra,
	    dec, 1)

	# Transform the ra, dec, and width parameters to degrees.
	switch (sk_stati(qcoo, S_NLNGUNITS)) {
	case SKY_HOURS:
	    ra = 15.0d0 * ra
	case SKY_DEGREES:
	    ;
	case SKY_RADIANS:
	    ra = DRADTODEG (ra)
	default:
	    ;
	}
	switch (sk_stati(qcoo, S_NLATUNITS)) {
	case SKY_HOURS:
	    dec = 15.0d0 * dec
	case SKY_DEGREES:
	    ;
	case SKY_RADIANS:
	    dec = DRADTODEG (dec)
	default:
	    ;
	}
	rawidth = AT_RCSTRAWIDTH(symbol) / 60.0d0
	decwidth = AT_RCSTDECWIDTH(symbol) / 60.0d0

	# Cleanup.
	call sk_close (fcoo)
	call sk_close (qcoo)
	call sfree (sp)

	return (OK)
end


# AT_RCLIMITS -- Given the ra, dec, ra width, and dec width of the field
# compute the field corners.

procedure at_rclimits (ra, dec, rawidth, decwidth, ra1, ra2, dec1, dec2)

double	ra			#I the field center ra in degrees
double	dec			#I the field center dec in degrees
double	rawidth			#I the field ra width in degrees
double	decwidth		#I the field dec width in degrees
double	ra1			#O lower ra limit in degrees
double	ra2			#O upper ra limit in degrees
double	dec1			#O lower dec limit in degrees
double	dec2			#O upper dec limit in degrees

double	cosdec, dra

begin
        # Find the field corners.
        dec1 = dec - 0.5d0 * decwidth
        dec2 = dec + 0.5d0 * decwidth
        if (dec1 <= -90.0d0) {
            dec1 = -90.0d0
            dec2 = min  (dec + 0.5d0 * decwidth, 90.0d0)
            ra1 = 0.0d0
            ra2 = 360.0d0
            return
        } else if (dec2 >= 90.0d0) {
            dec2 = 90.0d0
            dec1 = max (dec - 0.5d0 * decwidth, -90.0d0)
            ra1 = 0.0d0
            ra2 = 360.0d0
	} else {
	    if (dec > 0.0d0)
		cosdec = cos (DEGTORAD(dec2))
	    else
		cosdec = cos (DEGTORAD(dec1))
	    dra = 0.50d0 * rawidth / cosdec
	    if (dra >= 180.0d0) {
		ra1 = 0.0d0
		ra2 = 360.0d0
	    } else {
		ra1 = ra - dra
		if (ra1 < 0.0d0)
		    ra1 = ra1 + 360.0d0
		ra2 = ra + dra
		if (ra2 > 360.0d0)
		    ra2 = ra2 - 360.0d0
	    }
	}

end
