include "../../lib/astrom.h"
include "../../lib/acatalog.h"
include "../../lib/aimpars.h"


# AT_DRCPSET -- Reset the region definition parameters to their default values.

procedure at_drcpset (at)

pointer at                      #I the pointer to the main astrom structure

begin
	# Set the default field center and width.
	call at_setd (at, RCRA, 0.0d0)
	call at_setd (at, RCDEC, 0.0d0)
	call at_setd (at, RCRAWIDTH, 10.0d0)
	call at_setd (at, RCDECWIDTH, 10.0d0)

	# Set the default field center units.
	call at_seti (at, RCRAUNITS, 0)
	call at_seti (at, RCDECUNITS, 0)

	# Set the default field center coordinate system.
	call at_sets (at, RCSYSTEM, "")
end


# AT_DFSPSET -- Reset the filtering parameters to their default values.

procedure at_dfspset (at)

pointer at                      #I the pointer to the main astrom structure

begin
	call at_sets (at, FIELDS, "f[*]")
	call at_sets (at, FEXPR, "yes")
	call at_sets (at, FNAMES, "")
	call at_sets (at, FNTYPES, "")
	call at_sets (at, FNUNITS, "")
	call at_sets (at, FNFORMATS, "")
	call at_sets (at, FSORT, "")
	call at_seti (at, FREVERSE, NO)
	call at_sets (at, FOSYSTEM, "")
	call at_sets (at, FIRA, "ra")
	call at_sets (at, FIDEC, "dec")
	call at_seti (at, FORAUNITS, 0)
	call at_seti (at, FODECUNITS, 0)
	call at_sets (at, FORAFORMAT, "")
	call at_sets (at, FODECFORMAT, "")
	call at_sets (at, FIXP, "xp")
	call at_sets (at, FIYP, "yp")
	call at_sets (at, FIXC, "xc")
	call at_sets (at, FIYC, "yc")
	call at_sets (at, FOXFORMAT, "%10.3f")
	call at_sets (at, FOYFORMAT, "%10.3f")
end


# AT_DWCPSET -- Reset the wcs parameters to their default values.

procedure at_dwcpset (at)

pointer at                      #I the pointer to the main astrom structure

double	dval
pointer	st, sym
int	ip
pointer at_statp(), stfind()
int	ctod()

begin
	st = at_statp (at, WCST)
	if (st == NULL)
	    return

	sym = stfind (st, "wxref")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WXREF, dval)
	}

	sym = stfind (st, "wyref")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WYREF, dval)
	}

	sym = stfind (st, "wxmag")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WXMAG, dval)
	}

	sym = stfind (st, "wymag")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WYMAG, dval)
	}

	sym = stfind (st, "wxrot")
	if (sym != NULL) {
	    call strcpy ("180.0", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("180.0", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WXROT, dval)
	}

	sym = stfind (st, "wyrot")
	if (sym != NULL) {
	    call strcpy ("0.0", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("0.0", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WYROT, dval)
	}

	sym = stfind (st, "wraref")
	if (sym != NULL) {
	    call strcpy ("RA", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("RA", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WRAREF, dval)
	}

	sym = stfind (st, "wdecref")
	if (sym != NULL) {
	    call strcpy ("DEC", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    ip = 1
	    if (ctod ("DEC", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, WDECREF, dval)
	}

	sym = stfind (st, "wraunits")
	if (sym != NULL) {
	    call strcpy ("", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    call at_seti (at, WRAUNITS, 0)
	}

	sym = stfind (st, "wdecunits")
	if (sym != NULL) {
	    call strcpy ("", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    call at_seti (at, WDECUNITS, 0)
	}

	sym = stfind (st, "wproj")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    call at_sets (at, WPROJ, "tan")
	}

	sym = stfind (st, "wsystem")
	if (sym != NULL) {
	    call strcpy ("EQUINOX", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    call at_sets (at, WSYSTEM, "EQUINOX")
	}
end


# AT_DIMPSET -- Read in the default image data parameters.

procedure at_dimpset (at)

pointer at                      #I the pointer to the main astrom structure

double	dval
real	rval
pointer	st, sym
int	ip
pointer	at_statp(), stfind()
int	ctod(), ctor()

begin
	st = at_statp (at, IMST)
	if (st == NULL)
	    return

	sym = stfind (st, "esitelng")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, ESITELNG, dval)
	}

	sym = stfind (st, "esitelat")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctod ("INDEF", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, ESITELAT, dval)
	}

	sym = stfind (st, "esitealt")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, ESITEALT, rval)
	}

	sym = stfind (st, "esitetz")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, ESITETZ, rval)
	}

	sym = stfind (st, "emjdobs")
	if (sym != NULL) {
	    call strcpy ("MJD-OBS", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctod ("MJD-OBS", ip, dval) <= 0)
	        dval = INDEFD
	    call at_setd (at, EMJDOBS, dval)
	}

	sym = stfind (st, "edatamin")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, EDATAMIN, rval)
	}

	sym = stfind (st, "edatamax")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, EDATAMAX, rval)
	}

	sym = stfind (st, "egain")
	if (sym != NULL) {
	    call strcpy ("GAIN", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("GAIN", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, EGAIN, rval)
	}

	sym = stfind (st, "erdnoise")
	if (sym != NULL) {
	    call strcpy ("RDNOISE", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("RDNOISE", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, ERDNOISE, rval)
	}

	sym = stfind (st, "ewavlen")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, EWAVLEN, rval)
	}

	sym = stfind (st, "etemp")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, ETEMP, rval)
	}

	sym = stfind (st, "epress")
	if (sym != NULL) {
	    call strcpy ("INDEF", AT_IMSTKVAL(sym), LEN_IMST_STRUCT)
	    ip = 1
	    if (ctor ("INDEF", ip, rval) <= 0)
	        rval = INDEFR
	    call at_setr (at, EPRESS, rval)
	}

	sym = stfind (st, "observat")
	if (sym != NULL) {
	    call strcpy ("OBSERVAT", AT_WCSTKVAL(sym), LEN_WCST_STRUCT)
	    call at_sets (at, OBSERVAT, "OBSERVAT")
	}
end
