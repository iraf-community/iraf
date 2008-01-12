include	<imset.h>
#include      <tbset.h>
define        TBL_NROWS       0
include	<math.h>
include	"ace.h"
include	"cat.h"
include	"objs.h"


# CATOPEN -- Open a catalog.
# This may be used just to allocate the structure or to actually open
# a catalog file.  It does not read the objects.  Use catrobjs.

procedure catopen (cat, input, output, catdef)

pointer	cat			#U Catalog structure
char	input[ARB]		#I Input catalog name
char	output[ARB]		#I Output catalog name
char	catdef[ARB]		#I Catalog definition file

pointer	tbl

bool	streq()
pointer	tbtopn()

begin
	if (cat == NULL)
	    call calloc (cat, CAT_LEN, TY_STRUCT)

	if (input[1] == EOS && output[1] == EOS)
	    return

	if (streq (input, output)) {		# READ_WRITE
	    call calloc (tbl, TBL_LEN, TY_STRUCT)
	    CAT_INTBL(cat) = tbl
	    CAT_OUTTBL(cat) = tbl

	    TBL_TP(tbl) = tbtopn (input, READ_WRITE, 0)
	    call catdefine (tbl, READ_ONLY, catdef)
	    call catrhdr (cat)
	} else if (output[1] == EOS) {		# READ_ONLY
	    call calloc (tbl, TBL_LEN, TY_STRUCT)
	    CAT_INTBL(cat) = tbl
	    CAT_OUTTBL(cat) = NULL

	    TBL_TP(tbl) = tbtopn (input, READ_ONLY, 0)
	    call catdefine (tbl, READ_ONLY, catdef)
	    call catrhdr (cat)
	} else if (input[1] == EOS) {		# NEW_FILE
	    call calloc (tbl, TBL_LEN, TY_STRUCT)
	    CAT_INTBL(cat) = NULL
	    CAT_OUTTBL(cat) = tbl

	    TBL_TP(tbl) = tbtopn (output, NEW_FILE, 0)
	    call catdefine (tbl, NEW_FILE, catdef)
	} else {				# NEW_COPY
	    call calloc (tbl, TBL_LEN, TY_STRUCT)
	    CAT_INTBL(cat) = tbl

	    TBL_TP(tbl) = tbtopn (input, READ_ONLY, 0)
	    call catdefine (tbl, NEW_COPY, catdef)
	    call catrhdr (cat)

	    call calloc (tbl, TBL_LEN, TY_STRUCT)
	    CAT_OUTTBL(cat) = tbl
	    TBL_TP(tbl) = tbtopn (output, NEW_COPY, TBL_TP(CAT_INTBL(cat)))
	    call catdefine (tbl, NEW_COPY, catdef)
	}
end


procedure catcreate (cat)

pointer	cat			#I Catalog structure

pointer	tbl, tp

begin
	if (cat == NULL)
	    return
	tbl = CAT_OUTTBL(cat)
	if (tbl == NULL)
	    return
	tp = TBL_TP(tbl)
	if (tp == NULL)
	    return
	if (CAT_INTBL(cat) != NULL) {
	    if (tp == TBL_TP(CAT_INTBL(cat)))
		return
	}
	call tbtcre (tp)
end


# CATCLOSE -- Close a catalog.

procedure catclose (cat)

pointer	cat			#I Catalog pointer

int	i
pointer	tbl, objs

begin
	if (cat == NULL)
	    return

	tbl = CAT_INTBL(cat)
	if (tbl != NULL) {
	    if (TBL_STP(tbl) != NULL)
		call stclose (TBL_STP(tbl))
	    if (tbl == CAT_OUTTBL(cat))
		CAT_OUTTBL(cat) = NULL
	    call tbtclo (TBL_TP(tbl))
	}
	tbl = CAT_OUTTBL(cat)
	if (tbl != NULL) {
	    if (TBL_STP(tbl) != NULL)
		call stclose (TBL_STP(tbl))
	    call tbtclo (TBL_TP(tbl))
	}

	objs = CAT_OBJS(cat)
	if (objs != NULL) {
	    do i = 0, CAT_NUMMAX(cat)-1
		call mfree (Memi[objs+i], TY_STRUCT)
	}

	call mfree (CAT_APFLUX(cat), TY_REAL)
	call mfree (CAT_OBJS(cat), TY_POINTER)
	call mfree (CAT_INTBL(cat), TY_STRUCT)
	call mfree (CAT_OUTTBL(cat), TY_STRUCT)
	call mfree (CAT_HDR(cat), TY_STRUCT)
	call mfree (cat, TY_STRUCT)
end


# CATGETS -- Get a string parameter from the catalog header.

procedure catgets (cat, param, value, maxchar)

pointer	cat			#I Catalog pointer
char	param[ARB]		#I Parameter to get
char	value[ARB]		#O Returned value 
int	maxchar			#I Maximum characters in value

int	i, strdic()

begin
	value[1] = EOS

	if (cat == NULL)
	    return

	i = strdic (param, CAT_STR(cat), CAT_SZSTR, CATPARAMS)
	switch (i) {
	case 1:
	    if (CAT_HDR(cat) == NULL)
		i = 0
	    else
		call strcpy (HDR_IMAGE(CAT_HDR(cat)), value, maxchar)
	case 2:
	    if (CAT_HDR(cat) == NULL)
		i = 0
	    else
		call strcpy (HDR_MASK(CAT_HDR(cat)), value, maxchar)
	case 3:
	    call strcpy (CAT_OBJID(cat), value, maxchar)
	case 4:
	    call strcpy (CAT_CATALOG(cat), value, maxchar)
	default:
	    call sprintf (CAT_STR(cat), CAT_SZSTR,
		"catgets: unknown catalog parameter `%s'")
		call pargstr (param)
	    call error (1, CAT_STR(cat))
	}

	if (i == 0) {
	    call sprintf (CAT_STR(cat), CAT_SZSTR,
		"catgets: parameter `%s' not found")
		call pargstr (param)
	    call error (1, CAT_STR(cat))
	}
end


procedure catgeti (cat, param, value)

pointer	cat			#I Catalog pointer
char	param[ARB]		#I Parameter to get
int	value			#O Returned value 

int	i, strdic()

begin
	value = INDEFI

	if (cat == NULL)
	    return

	i = strdic (param, CAT_STR(cat), CAT_SZSTR, CATPARAMS)
	switch (i) {
	case 5:
	    value = CAT_NOBJS(cat)
	default:
	    call sprintf (CAT_STR(cat), CAT_SZSTR,
		"catgeti: unknown catalog parameter `%s'")
		call pargstr (param)
	    call error (1, CAT_STR(cat))
	}
end


procedure catputs (cat, param, value)

pointer	cat			#I Catalog pointer
char	param[ARB]		#I Parameter to get
char	value[ARB]		#I Value 

int	i, strdic()

begin
	if (cat == NULL)
	    return

	i = strdic (param, CAT_STR(cat), CAT_SZSTR, CATPARAMS)
	switch (i) {
	case 0:
	    call sprintf (CAT_STR(cat), CAT_SZSTR,
		"catgets: unknown catalog parameter `%s'")
		call pargstr (param)
	    call error (1, CAT_STR(cat))
	case 1:
	    if (CAT_HDR(cat) == NULL)
		call calloc (CAT_HDR(cat), HDR_LEN, TY_STRUCT)
	    call strcpy (value, HDR_IMAGE(CAT_HDR(cat)), HDR_SZFNAME)
	case 2:
	    if (CAT_HDR(cat) == NULL)
		call calloc (CAT_HDR(cat), HDR_LEN, TY_STRUCT)
	    call strcpy (value, HDR_MASK(CAT_HDR(cat)), HDR_SZFNAME)
	case 3:
	    call strcpy (value, CAT_OBJID(cat), CAT_SZSTR)
	case 4:
	    call strcpy (value, CAT_CATALOG(cat), CAT_SZSTR)
	}
end


procedure catputr (cat, param, value)

pointer	cat			#I Catalog pointer
char	param[ARB]		#I Parameter to get
real	value			#I Value

int	i, strdic()

begin
	if (cat == NULL)
	    return

	i = strdic (param, CAT_STR(cat), CAT_SZSTR, CATPARAMS)
	switch (i) {
	case 6:
	    if (CAT_HDR(cat) == NULL)
		call calloc (CAT_HDR(cat), HDR_LEN, TY_STRUCT)
	    HDR_MAGZERO(CAT_HDR(cat)) = value
	default:
	    call sprintf (CAT_STR(cat), CAT_SZSTR,
		"catgetr: unknown catalog parameter `%s'")
		call pargstr (param)
	    call error (1, CAT_STR(cat))
	}
end


procedure catrobjs (cat, filt)

pointer	cat			#I Catalog pointer
char	filt[ARB]		#I Filter string

int	i, num, nrows, nobjs, nummax, nalloc, tbpsta()
pointer	tbl, tp, objs, obj
bool	filter()

begin
	if (cat == NULL)
	    return

	tbl = CAT_INTBL(cat)
	if (tbl == NULL)
	    return
	tp = TBL_TP(tbl)

	nrows = tbpsta (tp, TBL_NROWS)
	nalloc = nrows + NUMSTART - 1
	call calloc (objs, nalloc, TY_POINTER)

	nobjs = 0
	nummax = 0
	obj = NULL
	do i = 1, nrows {
	    call catrobj (cat, obj, i)
	    if (!filter (obj, filt))
		next
	    num = OBJ_NUM(obj)
	    if (num > nalloc) {
		nalloc = nalloc + 1000
		call realloc (objs, nalloc, TY_POINTER)
		call aclri (Memi[objs+nalloc-1000], 1000)
	    }
	    if (Memi[objs+num-1] == NULL)
		nobjs = nobjs + 1
	    nummax = max (num, nummax)
	    Memi[objs+num-1] = obj
	    obj = NULL
	}

	CAT_OBJS(cat) = objs
	CAT_NOBJS(cat) = nobjs
	CAT_NUMMAX(cat) = nummax
end


procedure catrobj (cat, obj, row)

pointer	cat			#I Catalog pointer
pointer	obj			#U Object pointer
int	row			#I Table row

int	id, type, ori()
pointer	tbl, tp, stp, sym, cdef, sthead(), stnext()

begin
	if (cat == NULL)
	    return

	tbl = CAT_INTBL(cat)
	if (tbl == NULL)
	    return

	tp = TBL_TP(tbl)
	stp = TBL_STP(tbl)

	if (obj == NULL)
	    call calloc (obj, OBJ_LEN, TY_STRUCT)

	for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
	    id = ENTRY_ID(sym)
	    if (id > 1000 || id == ID_APFLUX)
		next
	    switch (id) {
	    case ID_FLAGS:
		OBJ_FLAGS(obj) = 0
		ifnoerr (call tbegtt (tp, cdef, row, CAT_STR(cat), CAT_SZSTR)) {
		    if (Memc[CAT_STRPTR(cat)] == 'M')
			SETFLAG(obj,OBJ_SPLIT)
		}
		next
	    }
		
	    type = ENTRY_TYPE(sym)
	    cdef = ENTRY_CDEF(sym)
	    switch (type) {
	    case TY_INT:
		iferr (call tbegti (tp, cdef, row, OBJI(obj,id)))
		    OBJI(obj,id) = INDEFI
	    case TY_REAL:
		iferr (call tbegtr (tp, cdef, row, OBJR(obj,id)))
		    OBJR(obj,id) = INDEFR
	    case TY_DOUBLE:
		iferr (call tbegtd (tp, cdef, row, OBJD(obj,id)))
		    OBJD(obj,id) = INDEFD
	    default:
		iferr (call tbegtt (tp, cdef, row, OBJC(obj,id), -type))
		    OBJC(obj,id) = EOS
	    }
	}

	OBJ_ROW(obj) = row
end


procedure catwobj (cat, obj, row)

pointer	cat			#I Catalog pointer
pointer	obj			#I Object pointer
int	row			#I Table row

int	ival
real	rval
double	dval
pointer	sval

int	id, type, func, napr, andi()
real	magzero, a, b, theta, elong, ellip, r, cxx, cyy, cxy
real	aerr, berr, thetaerr, cxxerr, cyyerr, cxyerr
bool	doshape
pointer	tbl, tp, stp, sym, cdef, sthead(), stnext()

begin
	if (obj == NULL)
	    return

	tbl = CAT_OUTTBL(cat)
	if (tbl == NULL)
	    return
	tp = TBL_TP(tbl)
	stp = TBL_STP(tbl)

	#call sprintf (CAT_STR(cat), CAT_SZSTR, "%s-%d")
	#    if (OBJ_OBJID(obj) != NULL)
	#	call pargstr (Memc[OBJ_OBJID(obj)])
	#    else
	#	call pargstr (CAT_OBJID(cat))
	#    call pargi (OBJ_NUM(obj))
	#call tbeptt (tp, TBL_BJID(tbl), row, CAT_STR(cat))
	#call tbeptt (tp, TBL_CLASS(tbl), row, OBJ_CLASS(obj))

	magzero = CAT_MAGZERO(cat)
	if (IS_INDEFR(magzero))
	    magzero = 0.
	sval = CAT_STRPTR(cat)
	napr = 0
	doshape = false
	for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
	    id = ENTRY_ID(sym)
	    func = ENTRY_FUNC(sym)
	    type = ENTRY_TYPE(sym)
	    cdef = ENTRY_CDEF(sym)
	    if (id > 1000) {
		switch (id) {
		case ID_A, ID_B, ID_THETA, ID_ELONG, ID_ELLIP, ID_R, ID_CXX,
		    ID_CYY, ID_CXY:
		    if (!doshape) {
			call catshape (obj, a, b, theta, elong, ellip, r,
			    cxx, cyy, cxy, aerr, berr, thetaerr, cxxerr,
			    cyyerr, cxyerr)
			doshape = true
		    }
		    switch (id) {
		    case ID_A:
			rval = a
		    case ID_B:
			rval = b
		    case ID_THETA:
			rval = theta
		    case ID_ELONG:
			rval = elong
		    case ID_ELLIP:
			rval = ellip
		    case ID_R:
			rval = r
		    case ID_CXX:
			rval = cxx
		    case ID_CYY:
			rval = cyy
		    case ID_CXY:
			rval = cxy
		    }
		case ID_FLUXERR, ID_XERR, ID_YERR:
		    switch (id) {
		    case ID_FLUXERR:
			rval = OBJ_FLUXVAR(obj)
		    case ID_XERR:
			rval = OBJ_XVAR(obj)
		    case ID_YERR:
			rval = OBJ_YVAR(obj)
		    }
		    if (IS_INDEFR(rval) || rval < 0.)
			rval = INDEFR
		    else
			rval = sqrt (rval)
		case ID_AERR, ID_BERR, ID_THETAERR, ID_CXXERR, ID_CYYERR,
		    ID_CXYERR:
		    if (!doshape) {
			call catshape (obj, a, b, theta, elong, ellip, r,
			    cxx, cyy, cxy, aerr, berr, thetaerr, cxxerr,
			    cyyerr, cxyerr)
			doshape = true
		    }
		    switch (id) {
		    case ID_AERR:
			rval = aerr
		    case ID_BERR:
			rval = aerr
		    case ID_THETAERR:
			rval = aerr
		    case ID_CXXERR:
			rval = aerr
		    case ID_CYYERR:
			rval = aerr
		    case ID_CXYERR:
			rval = aerr
		    }
		}
	    } else if (id == ID_FLAGS) {
		if (SPLIT(obj))
		    call strcpy ("M", Memc[sval], SZ_LINE)
		else
		    call strcpy ("-", Memc[sval], SZ_LINE)
	    } else if (id == ID_APFLUX) {
		if (OBJ_APFLUX(obj) == NULL)
		    rval = INDEFR
		else {
		    rval = Memr[OBJ_APFLUX(obj)+napr]
		    napr = napr + 1
		}
	    } else {
		switch (type) {
		case TY_INT:
		    ival = OBJI(obj,id)
		case TY_REAL:
		    rval = OBJR(obj,id)
		case TY_DOUBLE:
		    dval = OBJD(obj,id)
		default:
		    call strcpy (OBJC(obj,id), Memc[sval], SZ_LINE)
		}
	    }

	    # Apply function.
	    if (func > 0) {
		if (ENTRY_CTYPE(sym) != type) {
		    # For now all function types are real.
		    switch (type) {
		    case TY_INT:
			rval = ival
		    case TY_DOUBLE:
			rval = dval
		    }
		}
		type = ENTRY_CTYPE(sym)
		switch (func) {
		case FUNC_MAG:
		    if (!IS_INDEFR(rval)) {
			if (rval <= 0.)
			    rval = INDEFR
			else
			    rval = -2.5 * log10 (rval) + magzero
		    }
		}
	    }

	    # Write to catalog.
	    switch (type) {
	    case TY_INT:
		call tbepti (tp, cdef, row, ival)
	    case TY_REAL:
		call tbeptr (tp, cdef, row, rval)
	    case TY_DOUBLE:
		call tbeptd (tp, cdef, row, dval)
	    default:
		call tbeptt (tp, cdef, row, Memc[sval])
	    }
	}
	OBJ_ROW(obj) = row
end


# CATWCS -- Set catalog WCS information.

procedure catwcs (cat, im)

pointer	cat			#I Catalog pointer
pointer	im			#I IMIO pointer

int	i
pointer	sp, axtype, label, units, format
pointer	mw, tbl, tp, stp, sym, cdef

bool	streq()
pointer	mw_openim(), sthead(), stnext(), stname()
errchk	mw_openim

begin
	if (cat == NULL)
	    return
	if (CAT_OUTTBL(cat) == NULL)
	    return

	call smark (sp)
	call salloc (axtype, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)
	call salloc (format, SZ_FNAME, TY_CHAR)

	tbl = CAT_OUTTBL(cat)
	tp = TBL_TP(tbl)
	stp = TBL_STP(tbl)

	mw = mw_openim (im)
	do i = 1, 2 {
	    iferr (call mw_gwattrs (mw, i, "axtype", Memc[axtype], SZ_FNAME))
		Memc[axtype] = EOS
	    iferr (call mw_gwattrs (mw, i, "label", Memc[label], SZ_FNAME)) {
		if (streq (Memc[axtype], "ra"))
		    call strcpy ("RA", Memc[label], SZ_FNAME)
		else if (streq (Memc[axtype], "dec"))
		    call strcpy ("DEC", Memc[label], SZ_FNAME)
		else
		    Memc[label] = EOS
	    }
	    iferr (call mw_gwattrs (mw, i, "units", Memc[units], SZ_FNAME)) {
		if (streq (Memc[axtype], "ra") || streq (Memc[axtype], "dec"))
		    call strcpy ("deg", Memc[units], SZ_FNAME)
		else
		    Memc[units] = EOS
	    }
	    iferr (call mw_gwattrs (mw, i, "format", Memc[format], SZ_FNAME)) {
		if (streq (Memc[axtype], "ra"))
		    call strcpy ("%.2H", Memc[format], SZ_FNAME)
		else if (streq (Memc[axtype], "dec"))
		    call strcpy ("%.1h", Memc[format], SZ_FNAME)
		else
		    Memc[format] = EOS
	    }

	    if (i == 1) {
		for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
		    if (ENTRY_ID(sym) != ID_WX)
			next
		    if (!(streq (Memc[stname(stp,sym)], "WX") ||
			streq (Memc[stname(stp,sym)], "wx")))
			Memc[label] = EOS
		    break
		}
	    } else {
		for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
		    if (ENTRY_ID(sym) != ID_WY)
			next
		    if (!(streq (Memc[stname(stp,sym)], "WY") ||
			streq (Memc[stname(stp,sym)], "wy")))
			Memc[label] = EOS
		    break
		}
	    }

	    if (sym != NULL) {
		cdef = ENTRY_CDEF(sym)
		if (Memc[label] != EOS)
		    call tbcnam (tp, cdef, Memc[label])
		if (Memc[units] != EOS)
		    call tbcnit (tp, cdef, Memc[units])
		if (Memc[format] != EOS)
		    call tbcfmt (tp, cdef, Memc[format])
	    }
	}
	call mw_close (mw)

	call sfree (sp)
end


procedure catrhdr (cat)

pointer	cat			#I Catalog pointer

pointer	tp, hdr

begin
	if (cat == NULL)
	    return

	if (CAT_HDR(cat) != NULL)
	    call mfree (CAT_HDR(cat), TY_STRUCT)
	if (CAT_INTBL(cat) == NULL)
	    return
	tp = TBL_TP(CAT_INTBL(cat))

	call calloc (CAT_HDR(cat), HDR_LEN, TY_STRUCT)
	hdr = CAT_HDR(cat)

	iferr (call tbhgtt (tp, "IMAGE", HDR_IMAGE(hdr), HDR_SZFNAME))
	    HDR_IMAGE(hdr) = EOS
	iferr (call tbhgtt (tp, "MASK", HDR_MASK(hdr), HDR_SZFNAME))
	    HDR_MASK(hdr) = EOS
	iferr (call tbhgtr (tp, "MAGZERO", HDR_MAGZERO(hdr)))
	    HDR_MAGZERO(hdr) = INDEFR
end


procedure catwhdr (cat, im)

pointer	cat			#I Catalog pointer
pointer	im			#I Image pointer

pointer	tp, hdr

begin
	if (cat == NULL)
	    return

	tp = CAT_OUTTBL(cat)
	hdr = CAT_HDR(cat)
	if (tp == NULL || hdr == NULL)
	    return
	tp = TBL_TP(tp)
	
	if (HDR_IMAGE(hdr) != EOS)
	    call tbhadt (tp, "IMAGE", HDR_IMAGE(hdr))
	if (HDR_MASK(hdr) != EOS)
	    call tbhadt (tp, "MASK", HDR_MASK(hdr))
	if (!IS_INDEFR(HDR_MAGZERO(hdr)))
	    call tbhadr (tp, "MAGZERO", HDR_MAGZERO(hdr))
end


procedure catwobjs (cat)

pointer	cat			#I Catalog pointer

int	i, j
pointer	objs, obj

begin
	if (cat == NULL)
	    return
	if (CAT_OUTTBL(cat) == NULL)
	    return
	if (CAT_OBJS(cat) == NULL)
	    return

	objs = CAT_OBJS(cat)
	j = 0
	do i = 1, CAT_NUMMAX(cat) {
	    obj = Memi[objs+i-1]
	    if (obj == NULL)
		next
	    j = j + 1
	    call catwobj (cat, obj, j)
	}
end
	


procedure catdump (cat)

pointer	cat			#I Catalog pointer

int	i
pointer	objs, obj

begin
	if (CAT_OBJS(cat) == NULL)
	    return

	objs = CAT_OBJS(cat)
	do i = 1, CAT_NUMMAX(cat) {
	    obj = Memi[objs+i-1]
	    if (obj == NULL)
		next

	    call printf ("%d %d %g %g\n")
		call pargi (OBJ_NUM(obj))
		call pargi (OBJ_NPIX(obj))
		call pargr (OBJ_XAP(obj))
		call pargr (OBJ_YAP(obj))
	}
end


# CATGOBJ -- Get object given the object number.
#
# Currently this relies on the object pointer array being indexed by
# object number.

pointer procedure catgobj (cat, num)

pointer	cat			#I Catalog
int	num			#I Object number

begin
	return (Memi[CAT_OBJS(cat)+num-1])
end


# These currently work on the object number but eventually there will be
# an array of indices to allow traversing the objects in some sorted order.

pointer procedure cathead (cat)

pointer	cat			#I Catalog pointer

int	i
pointer	objs, obj

begin
	objs = CAT_OBJS(cat)
	do i = 0, CAT_NUMMAX(cat)-1 {
	    obj = Memi[objs+i]
	    if (obj != NULL)
		return (obj)
	}
	return (NULL)
end


pointer	procedure catnext (cat, obj)

pointer	cat			#I Catalog pointer
pointer	obj			#I Input object pointer

int	i
pointer	objs, objnext

begin
	if (obj == NULL)
	    return (NULL)

	objs = CAT_OBJS(cat)
	do i = OBJ_NUM(obj), CAT_NUMMAX(cat)-1 {
	    objnext = Memi[objs+i]
	    if (objnext != NULL)
		return (objnext)
	}
	return (NULL)
end


procedure catshape (obj, a, b, theta, elong, ellip, r, cxx, cyy, cxy,
	aerr, berr, thetaerr, cxxerr, cyyerr, cxyerr)

pointer	obj			#I Object structure
real	a			#O Semimajor axis based on second moments
real	b			#O Semiminor axis based on second moments
real	theta			#O Position angle based on second moments
real	elong			#O Elongation (A/B)
real	ellip			#O Ellipticity (1 - B/A)
real	r			#O Radius based on second moments
real	cxx, cyy, cxy		#O Ellipse parameters based on second moments
real	aerr, berr, thetaerr	#O Errors
real	cxxerr, cyyerr, cxyerr	#O Errors

bool	doerr
real	x2, y2, xy, r2, d, f
real	xvar, yvar, xycov, rvar, dvar, fvar

begin
	a = INDEFR
	b = INDEFR
	theta = INDEFR
	elong = INDEFR
	ellip = INDEFR
	r = INDEFR
	aerr = INDEFR
	berr = INDEFR
	thetaerr = INDEFR
	cxxerr = INDEFR
	cyyerr = INDEFR
	cxyerr = INDEFR

	x2 = OBJ_X2(obj)
	y2 = OBJ_Y2(obj)
	xy = OBJ_XY(obj)
	xvar = OBJ_XVAR(obj)
	yvar = OBJ_YVAR(obj)
	xycov = OBJ_XYCOV(obj)

	if (IS_INDEFR(x2) || IS_INDEFR(y2) || IS_INDEFR(xy))
	    return

	r2 = x2 + y2
	if (r2 < 0.)
	    return

	doerr = !(IS_INDEF(xvar) || IS_INDEF(yvar) || IS_INDEF(xycov))
	if (doerr) {
	    rvar = xvar + yvar
	    if (rvar < 0.)
	       doerr = false
	}

	r = sqrt (r2)

	d = x2 - y2
	theta = RADTODEG (atan2 (2 * xy, d) / 2.)

	if (doerr) {
	    dvar = xvar - yvar
	    thetaerr = atan2 (2 * xycov, dvar) / 2.
	    if (thetaerr < 0.)
		thetaerr = INDEF
	    else
		thetaerr = DEGTORAD (sqrt (thetaerr))
	}

	f = sqrt (d**2 + 4 * xy**2)
	if (f > r2)
	    return

	if (doerr) {
	    fvar = sqrt (dvar**2 + 4 * xycov**2)
	    if (fvar > rvar)
		doerr = false
	}

	a = sqrt ((r2 + f) / 2)
	b = sqrt ((r2 - f) / 2)

	if (doerr) {
	    aerr = sqrt ((rvar + fvar) / 2)
	    berr = sqrt ((rvar - fvar) / 2)
	}

	ellip = 1 - b / a
	if (b > 0.)
	    elong = a / b

	if (f == 0) {
	    cxx = 1. / (a * a)
	    cyy = 1. / (a * a)
	    cxy = 0
	} else {
	    cxx = y2 / f
	    cyy = x2 / f
	    cxy = -2 * xy / f
	}

	if (doerr) {
	    if (fvar == 0) {
		cxxerr = 1. / (aerr * aerr)
		cyyerr = 1. / (berr * berr)
		cxyerr = 0.
	    } else {
		cxxerr = yvar / fvar
		cyyerr = xvar / fvar
		cxyerr = -2 * xycov / fvar
	    }
	}

end
