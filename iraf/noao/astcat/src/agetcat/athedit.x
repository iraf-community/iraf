include "../../lib/astrom.h"
include "../../lib/aimpars.h"
include <pkg/cq.h>


# AT_HEDIT -- Add a set of standard keywords to the image header.

procedure at_hedit (im, res, at, update, verbose)

pointer	im			#I the input image descriptor
pointer	res			#I the image results descriptor
pointer	at			#I the astrometry package descriptor
bool	update			#I update the header ?
bool	verbose			#I verbose mode ?

begin
	if (res != NULL)
	    call at_dbkey (im, res, update, verbose)
	if (at != NULL)
	    call at_parkey (im, at, update, verbose)
end


# AT_DBKEY -- Add a set of standard keywords required by astrometric 
# reductions to the image header. New keywords will only be added if
# the keyword name is defined in the in the image survey database and the
# standard keyword does not already exist in the image header, or if the
# keyword has a default value in the image survey database.

procedure at_dbkey (im, res, update, verbose)

pointer	im			#I the input image descriptor
pointer	res			#I the image results descriptor
bool	update			#I update the header ?
bool	verbose			#I verbose mode ?

pointer	sp, kfield, kname, kvalue, kunits
int	i, nkey, ktype
int	cq_istati(), cq_kinfon(), imaccf(), at_akeyword()
bool	streq()

begin
	call smark (sp)
	call salloc (kfield, CQ_SZ_QPNAME, TY_CHAR) 
	call salloc (kname, CQ_SZ_QPNAME, TY_CHAR) 
	call salloc (kvalue, CQ_SZ_QPVALUE, TY_CHAR) 
	call salloc (kunits, CQ_SZ_QPUNITS, TY_CHAR) 

	# Loop over the keywords.
	nkey = cq_istati (res, CQNIMPARS)
	do i = 1, nkey {

	    # Get the keyword information.
	    if (cq_kinfon (res, i, Memc[kfield], CQ_SZ_QPNAME, Memc[kname],
	    	CQ_SZ_QPNAME, Memc[kvalue], CQ_SZ_QPVALUE, ktype, Memc[kunits],
		CQ_SZ_QPUNITS) != i)
		next

	    # The keyword names is INDEF.
	    if (streq (Memc[kname], "INDEF")) {

		# Go to next keyword if the keyword value is also INDEF.
		if (streq (Memc[kvalue], "INDEF"))
		    next

		# Add keyword with its default value if it does not exist.
		if (imaccf (im, Memc[kfield]) == NO) {
		    if (at_akeyword (im, Memc[kfield], Memc[kvalue], ktype,
			Memc[kunits], update) == OK) {
			#if (update || verbose) {
			if (verbose) {
		            call printf (
			        "    Adding survey keyword %s = %s to header\n")
			        call pargstr (Memc[kfield])
				call pargstr (Memc[kvalue])
			}
		    #} else if (update || verbose) {
		    } else if (verbose) {
		        call printf (
			    "    Error adding survey keyword %s to header\n")
			    call pargstr (Memc[kfield])
		    }
		#} else if (update || verbose) {
		} else if (verbose) {
		    call printf (
		        "    Warning survey keyword %s already exists\n")
			call pargstr (Memc[kfield])
		}

	    # The keyword name is defined and it exists in the image.
	    } else if (imaccf (im, Memc[kname]) == YES) {

		# Add the new keyword with the old keyword value to the image
		call imgstr (im, Memc[kname], Memc[kvalue], CQ_SZ_QPVALUE) 
		if (imaccf (im, Memc[kfield]) == NO) {
		    if (at_akeyword (im, Memc[kfield], Memc[kvalue], ktype,
			Memc[kunits], update) == OK) {
			#if (update || verbose) {
			if (verbose) {
		            call printf (
			        "    Adding survey keyword %s = %s to header\n")
			        call pargstr (Memc[kfield])
				call pargstr (Memc[kvalue])
			}
		    #} else if (update || verbose) {
		    } else if (verbose) {
		        call printf (
			    "    Error adding survey keyword %s to header\n")
			    call pargstr (Memc[kfield])
		    }
		#} else if (update || verbose) {
		} else if (verbose) {
		    call printf (
		        "    Warning survey keyword %s already exists\n")
			call pargstr (Memc[kfield])
		}

	    # The keywords names does not exist in the image.
	    #} else if (update || verbose) {
	    } else if (verbose) {

	        call printf (
	            "    Warning survey keyword %s value %s does not exist\n")
		    call pargstr (Memc[kfield])
		    call pargstr (Memc[kname])
	    }
	}

	call sfree (sp)
end


# AT_PARKEY -- Add a set of standard keywords required by astrometric 
# reductions to the image header. New keywords will only be added if
# the keyword name is defined in default AIMPARS parameter file and the
# standard keyword does not already exist in the image header, or if the
# keyword has a default value in the AIMPARS parameter file.

procedure at_parkey (im, at, update, verbose)

pointer	im			#I the input image descriptor
pointer	at			#I the astrometry package descriptor
bool	update			#I update the header ?
bool	verbose			#I verbose mode ?

double	dval
real	rval
pointer	imst, sym, sp, kfield, kvalue
int	i, key
double	at_statd(), imgetd()
real	at_statr(), imgetr()
pointer	at_statp(), stfind()
int	at_wrdstr(), imaccf()
bool	streq()
errchk	imgetd(), imgetr(), imgstr()

begin
	if (at_statp (at, PIMPARS) == NULL)
	    return
	imst = at_statp (at, IMST)
	if (imst == NULL)
	    return

	call smark (sp)
	call salloc (kfield, SZ_FNAME, TY_CHAR) 
	call salloc (kvalue, SZ_FNAME, TY_CHAR) 

	# Loop over the keywords.
	do i = 1, AT_NIMFIELDS {

	    # Get the parameter name.
	    key = at_wrdstr (i, Memc[kfield], SZ_FNAME, AT_IMFIELDS) 
	    switch (key) {
	    case HDR_OBSERVAT:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
			call at_stats (at, OBSERVAT, Memc[kvalue], SZ_FNAME)
                    else iferr (call imgstr (im, AT_IMSTKVAL(sym),
		        Memc[kvalue], SZ_FNAME))
			call at_stats (at, OBSERVAT, Memc[kvalue], SZ_FNAME)
                } else 
		    call at_stats (at, OBSERVAT, Memc[kvalue], SZ_FNAME)

		if (! streq (Memc[kvalue], "INDEF")) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imastr (im, Memc[kfield], Memc[kvalue])
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %s to header\n")
			        call pargstr (Memc[kfield])
			        call pargstr (Memc[kvalue])
		        }
		    }
		}

	    case HDR_ESITELNG:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        dval = at_statd (at, ESITELNG)
                    else iferr (dval = imgetd (im, AT_IMSTKVAL(sym)))
                        dval = at_statd (at, ESITELNG)
                } else 
                    dval = at_statd (at, ESITELNG)
		if (! IS_INDEFD(dval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddd (im, Memc[kfield], dval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %h to header\n")
			        call pargstr (Memc[kfield])
			        call pargd (dval)
		        }
		    }
		}
    
	    case HDR_ESITELAT:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        dval = at_statd (at, ESITELAT)
                    else iferr (dval = imgetd (im, AT_IMSTKVAL(sym)))
                        dval = at_statd (at, ESITELAT)
                } else 
                    dval = at_statd (at, ESITELAT)
		if (! IS_INDEFD(dval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddd (im, Memc[kfield], dval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %h to header\n")
			        call pargstr (Memc[kfield])
			        call pargd (dval)
		        }
		    }
		}

	    case HDR_ESITEALT:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, ESITEALT)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, ESITEALT)
                } else 
                    rval = at_statr (at, ESITEALT)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.1f to header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_ESITETZ:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, ESITETZ)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, ESITETZ)
                } else 
                    rval = at_statr (at, ESITETZ)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.1f to header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_EMJDOBS:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        dval = at_statd (at, EMJDOBS)
                    else iferr (dval = imgetd (im, AT_IMSTKVAL(sym)))
                        dval = at_statd (at, EMJDOBS)
                } else 
                    dval = at_statd (at, EMJDOBS)
		if (! IS_INDEFD(dval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddd (im, Memc[kfield], dval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.5f to header\n")
			        call pargstr (Memc[kfield])
				call pargd (dval)
		        }
		    }
		}

	    case HDR_EDATAMIN:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, EDATAMIN)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, EDATAMIN)
                } else 
                    rval = at_statr (at, EDATAMIN)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %g to header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_EDATAMAX:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, EDATAMAX)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, EDATAMAX)
                } else 
                    rval = at_statr (at, EDATAMAX)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %g to image\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_EGAIN:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, EGAIN)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, EGAIN)
                } else 
                    rval = at_statr (at, EGAIN)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.1f to image\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_ERDNOISE:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, ERDNOISE)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, ERDNOISE)
                } else 
                    rval = at_statr (at, ERDNOISE)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.1f to image\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_EWAVLEN:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, EWAVLEN)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, EWAVLEN)
                } else 
                    rval = at_statr (at, EWAVLEN)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %0.1f to header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_ETEMP:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, ETEMP)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, ETEMP)
                } else 
                    rval = at_statr (at, ETEMP)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s to image header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    case HDR_EPRESS:
                sym = stfind (imst, Memc[kfield])
                if (sym != NULL) {
                    if (streq (AT_IMSTKVAL(sym), "INDEF"))
                        rval = at_statr (at, EPRESS)
                    else iferr (rval = imgetr (im, AT_IMSTKVAL(sym)))
                        rval = at_statr (at, EPRESS)
                } else 
                    rval = at_statr (at, EPRESS)
		if (! IS_INDEFR(rval)) {
                    if (imaccf (im, Memc[kfield]) == YES) {
		        #if (update || verbose) {
		        if (verbose) {
			    call printf ("    Keyword %s already exists\n")
			        call pargstr (Memc[kfield])
		        }
		    } else {
			if (update)
		            call imaddr (im, Memc[kfield], rval)
		        #if (update || verbose) {
		        if (verbose) {
			    call printf (
			    "    Adding default keyword %s = %g to header\n")
			        call pargstr (Memc[kfield])
				call pargr (rval)
		        }
		    }
		}

	    default:
		;
	    }
	}

	call sfree (sp)
end


# AT_AKEYWORD -- Add a new keyword to the image header. Note that at present
# nothing is done with the units information although this may be used in the
# future.

int procedure at_akeyword (im, kname, kvalue, ktype, kunits, update)

pointer	im			#I the image descriptor
char	kname[ARB]		#I the image keyword name
char	kvalue[ARB]		#I the image keyword value
int	ktype			#I the image keyword data type
char	kunits[ARB]		#I the image keyword units (not used)
bool	update			#I actually update the header ?

double	dval
real	rval
long	lval
int	ip, stat
int	ctod(), ctor(), ctol()

begin
	stat = OK

	switch (ktype) {

	case TY_DOUBLE:
	    ip = 1
	    if (ctod (kvalue, ip, dval) > 0) {
		if (update)
		    call imaddd (im, kname, dval)
	    } else
		stat = ERR

	case TY_REAL:
	    ip = 1
	    if (ctor (kvalue, ip, rval) > 0) {
		if (update)
		    call imaddr (im, kname, rval)
	    } else
		stat = ERR

	case TY_LONG, TY_INT, TY_SHORT:
	    ip = 1
	    if (ctol (kvalue, ip, lval) > 0) {
		if (update)
		    call imaddl (im, kname, lval)
	    } else
		stat = ERR

	default:
	    if (update)
	        call imastr (im, kname, kvalue)
	}

	return (stat)
end
