include <ctotok.h>
include <ctype.h>
include <evvexpr.h>
include <imhdr.h>
include <pkg/cq.h>
include <pkg/skywcs.h>
include "../../lib/astrom.h"
include "../../lib/acatalog.h"

##############################################################################

# Create a small data structure to describe the field list. Decide whether
# this should be part of the main astrometry package structure later ...

define	FL_FLENGTH	12

define	FL_NEXPR	Memi[$1]	# The number of input expressions
define	FL_NFIELDS	Memi[$1+1]	# The number of output fields

# The field list decription
define	FL_FLIST	Memi[$1+2]	# The list of field expressions
define	FL_FRANGES	Memi[$1+3]	# The list of field ranges

# New quantities to be written in the header (could be a symbol table ...)
define	FL_FNAMES	Memi[$1+4]	# The list of field names
define	FL_FOFFSETS	Memi[$1+5]	# The list of field offsets
define	FL_FSIZES	Memi[$1+6]	# The list of field sizes
define	FL_FTYPES	Memi[$1+7]      # The list of field types
define	FL_FUNITS	Memi[$1+8]	# The list of field units
define	FL_FFMTS	Memi[$1+9]	# The list of field formats

# Useful constants
define	FL_MAX_NEXPR	20
define	FL_MAX_NFIELDS	100
define	FL_SZ_EXPR	SZ_LINE

##############################################################################

# AT_WIFILRECS -- Filter and write the output catalog.

procedure at_wifilrecs (fd, im, at, res, standard)

int	fd			#I the output file descriptor
pointer	im			#I the associated input image descriptor
pointer	at			#I the astrometry package descriptor
pointer	res			#I results descriptor
bool	standard		#I write a standard catalog header

double	raval, decval, oraval, odecval, iraval, idecval, xpval, ypval
pointer	sp, sexpr, sfield, record, raname, decname, sindex
pointer	flist, o, catcoo, outcoo, imcoo, mwim, ct
int	i, nlines, nrecs, rafield, decfield, xpfield, ypfield, xcfield, ycfield
pointer	at_flinit(), evvexpr(), locpr(), mw_sctran()
int	at_wfcathdr(), cq_rstati(), at_srtcat(), at_flnexpr(), cq_grecord()
int	cq_setrecord(), at_wcathdr(), at_mkrecord(), cq_gvald()
bool	streq()
extern	at_getop()

int	nchars

begin
	call smark (sp)
	call salloc (sexpr, FL_SZ_EXPR, TY_CHAR)
	call salloc (sfield, FL_SZ_EXPR, TY_CHAR)
	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (raname, FL_SZ_EXPR, TY_CHAR)
	call salloc (decname, FL_SZ_EXPR, TY_CHAR)

        # Initialize the catalog, output, and image coordinate systems.
	# and set up the image world to logical coordinate transformation.
	call at_cowcs (at, res, im, catcoo, outcoo, imcoo, mwim)
	if (imcoo != NULL)
	    ct = mw_sctran (mwim, "world", "logical", 03B)

	# Determine whether it is necessary to reformat.
	call at_stats (at, FIELDS, Memc[record], SZ_LINE)
	if (streq (Memc[record], "f[*]") && outcoo == NULL) {

	    # The field list is NULL.
	    flist = NULL

	    # Write the filtered catalog header.
	    if (standard)
	        nlines = at_wcathdr (fd, res)

	    # Coordinate fields are not modified.
	    rafield = 0
	    decfield = 0
	    xpfield = 0
	    ypfield = 0
	    xcfield = 0
	    ycfield = 0

	} else {

	    # Decode the output field list.
	    flist = at_flinit (at, res)

	    # Write the filtered catalog header.
	    if (standard)
	        nlines = at_wfcathdr (fd, at, res, flist)

	    # Get the offsets for the ra, dec, xp, yp, xc, and yc output fields.
	    call at_coofields (at, res, flist, Memc[raname], Memc[decname],
	        rafield, decfield, xpfield, ypfield, xcfield, ycfield)
	    if (outcoo == NULL) {
	        rafield = 0
	        decfield = 0
	    }
	    if (imcoo == NULL) {
	        xpfield = 0
	        ypfield = 0
	    }
	    xcfield = 0
	    ycfield = 0
	}

	# Compute the sort index.
	nrecs = cq_rstati (res, CQRNRECS)
	call malloc (sindex, nrecs, TY_INT)
	nrecs = at_srtcat (at, res, Memi[sindex], nrecs)

	# Get the selection expression and replace generic selection expression
	# field names with their catalog equivalents.
	call at_stats (at, FEXPR, Memc[sexpr], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[sexpr], i, Memc[sfield], FL_SZ_EXPR) == EOF)
	    Memc[sfield] = EOS

	# Loop over the sorted records. Note that any reference to
	# coordinates in the selection expression refers to the original
	# not the transformed coordinates. 

	o = NULL
	do i = 1, nrecs {

	    # Reject every record.
	    if (streq (Memc[sfield], "no"))
		next

	    # Evaluate the selection expression.
	    if (! streq (Memc[sfield], "yes")) {
	        if (cq_setrecord (res, Memi[sindex+i-1]) != Memi[sindex+i-1])
		    next
	        if (o != NULL)
		    call evvfree (o)
	        o = evvexpr (Memc[sfield], locpr (at_getop), res, 0, res, 0)
		if (O_TYPE(o) != TY_BOOL)
		    next
		if (O_VALI(o) == NO)
		    next
	    }

	    # Write the record.
	    if (flist == NULL) {

		# Copy the record.
	        nchars = cq_grecord (res, Memc[record], SZ_LINE,
		    Memi[sindex+i-1])

	    } else {

		# Get the ra and dec fields.
		raval = INDEFD
		decval = INDEFD
		if (outcoo != NULL || imcoo != NULL) {
		    if (cq_gvald (res, Memi[sindex+i-1], Memc[raname],
		        raval) <= 0)
		        raval = INDEFD
		    if (cq_gvald (res, Memi[sindex+i-1], Memc[decname],
		        decval) <= 0)
		        decval = INDEFD
		}

		# Transform the catalog coordinates to the output coordinate
		#  system.
		oraval = INDEFD
		odecval = INDEFD
		if (outcoo != NULL && (rafield > 0 || decfield > 0)) {
		    if (! IS_INDEFD(raval) && ! IS_INDEFD(decval))
			call sk_ultran (catcoo, outcoo, raval, decval, oraval,
			    odecval, 1)
		}

		# Transform the catalog coordinates to the image coordinate
		# system and then to the image pixel coordinate system.
		xpval = INDEFD
		ypval = INDEFD
		if (imcoo != NULL && (xpfield > 0 || ypfield > 0)) {
		    if (! IS_INDEFD(raval) && ! IS_INDEFD(decval)) {
			call sk_ultran (catcoo, imcoo, raval, decval, iraval,
			    idecval, 1)
			call mw_c2trand (ct, iraval, idecval, xpval, ypval) 
			if (xpval < 0.5d0 || xpval > (IM_LEN(im,1)+0.5d0) ||
			    ypval < 0.5d0 || ypval > (IM_LEN(im,2)+0.5d0))
			    next
		    } else {
			xpval = INDEFD
			ypval = INDEFD
		    }
		}

		# Reformat the record.
		nchars = at_mkrecord (flist, res, Memc[record], SZ_LINE,
		    Memi[sindex+i-1], rafield, decfield, oraval, odecval,
		    xpfield, ypfield, xpval, ypval) 

	    }

	    # Write the new record.
            if (nchars > 0) {
	        call fprintf (fd, "%s")
	            call pargstr (Memc[record])
	    }
	}

	# Free the selection expression descriptor.
	if (o != NULL)
	    call evvfree (o)

	# Free the catalog, output, and image coordinate system descriptors.
	if (catcoo != NULL)
	    call sk_close (catcoo)
	if (outcoo != NULL)
	    call sk_close (outcoo)
	if (imcoo != NULL)
	    call sk_close (imcoo)
	if (mwim != NULL)
	    call mw_close (mwim)

	# Free output field list.
	if (flist != NULL)
	    call at_flfree (flist)

	# Free thesort index descriptor.
	call mfree (sindex, TY_INT)

	call sfree (sp)
end


# AT_WFILRECS -- Filter and write the output catalog.

procedure at_wfilrecs (fd, at, res, standard)

int	fd			#I the output file descriptor
pointer	at			#I the astrometry package descriptor
pointer	res			#I results descriptor
bool	standard		#I write a standard catalog header

double	raval, decval, oraval, odecval, iraval, idecval, xpval, ypval
pointer	sp, sexpr, sfield, record, raname, decname, sindex
pointer	flist, o, catcoo, outcoo, imcoo, mwim, ct
int	i, nlines, nrecs, rafield, decfield, xpfield, ypfield, xcfield, ycfield
pointer	at_flinit(), evvexpr(), locpr(), mw_sctran()
int	at_wfcathdr(), cq_rstati(), at_srtcat(), at_flnexpr(), cq_grecord()
int	cq_setrecord(), at_wcathdr(), at_mkrecord(), cq_gvald()
bool	streq()
extern	at_getop()

int	nchars

begin
	call smark (sp)
	call salloc (sexpr, FL_SZ_EXPR, TY_CHAR)
	call salloc (sfield, FL_SZ_EXPR, TY_CHAR)
	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (raname, FL_SZ_EXPR, TY_CHAR)
	call salloc (decname, FL_SZ_EXPR, TY_CHAR)

        # Initialize the catalog, output, and field  coordinate systems
	# and set up the image world to logical coordinate transformation.
	call at_cowcs (at, res, NULL, catcoo,  outcoo, imcoo, mwim)
	if (imcoo != NULL)
	    ct = mw_sctran (mwim, "world", "logical", 03B)

	# Determine whether it is necessary to reformat.
	call at_stats (at, FIELDS, Memc[record], SZ_LINE)
	if (streq (Memc[record], "f[*]") && outcoo == NULL) {

	    # The field list is NULL.
	    flist = NULL

	    # Write the filtered catalog header.
	    if (standard)
	        nlines = at_wcathdr (fd, res)

	    # Coordinate fields are not altered.
	    rafield = 0
	    decfield = 0
	    xpfield = 0
	    ypfield = 0
	    xcfield = 0
	    ycfield = 0

	} else {

	    # Decode the output field list.
	    flist = at_flinit (at, res)

	    # Write the filtered catalog header.
	    if (standard)
	        nlines = at_wfcathdr (fd, at, res, flist)

	    # Get the offsets for the ra, dec, xp, yp, xc, and yc output fields.
	    call at_coofields (at, res, flist, Memc[raname], Memc[decname],
	        rafield, decfield, xpfield, ypfield, xcfield, ycfield)
	    if (outcoo == NULL) {
	        rafield = 0
	        decfield = 0
	    }
	    if (imcoo == NULL) {
	        xpfield = 0
	        ypfield = 0
	    }
	    xcfield = 0
	    ycfield = 0
	}

	# Compute the sort index.
	nrecs = cq_rstati (res, CQRNRECS)
	call malloc (sindex, nrecs, TY_INT)
	nrecs = at_srtcat (at, res, Memi[sindex], nrecs)

	# Get the selection expression and replace generic selection expression
	# field names with their catalog equivalents.
	call at_stats (at, FEXPR, Memc[sexpr], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[sexpr], i, Memc[sfield], FL_SZ_EXPR) == EOF)
	    Memc[sfield] = EOS

	# Loop over the sorted records. Note that any reference to
	# coordinates in the selection expression refers to the original
	# not the transformed coordinates. 

	o = NULL
	do i = 1, nrecs {

	    # Reject every record.
	    if (streq (Memc[sfield], "no"))
		next

	    # Evaluate the selection expression.
	    if (! streq (Memc[sfield], "yes")) {
	        if (cq_setrecord (res, Memi[sindex+i-1]) != Memi[sindex+i-1])
		    next
	        if (o != NULL)
		    call evvfree (o)
	        o = evvexpr (Memc[sfield], locpr (at_getop), res, 0, res, 0)
		if (O_TYPE(o) != TY_BOOL)
		    next
		if (O_VALI(o) == NO)
		    next
	    }

	    # Write the record.
	    if (flist == NULL) {

		# Copy the record.
	        nchars = cq_grecord (res, Memc[record], SZ_LINE,
		    Memi[sindex+i-1])

	    } else {

		# Get the ra and dec fields.
		raval = INDEFD
		decval = INDEFD
		if (outcoo != NULL || imcoo != NULL) {
		    if (cq_gvald (res, Memi[sindex+i-1], Memc[raname],
		        raval) <= 0)
		        raval = INDEFD
		    if (cq_gvald (res, Memi[sindex+i-1], Memc[decname],
		        decval) <= 0)
		        decval = INDEFD
		}

		# Transform the catalog coordinates to the output coordinate
		#  system.
		oraval = INDEFD
		odecval = INDEFD
		if (outcoo != NULL && (rafield > 0 || decfield > 0)) {
		    if (! IS_INDEFD(raval) && ! IS_INDEFD(decval))
			call sk_ultran (catcoo, outcoo, raval, decval, oraval,
			    odecval, 1)
		}

		# Transform the catalog coordinates to the image coordinate
		# system and then to the image pixel coordinate system.
		xpval = INDEFD
		ypval = INDEFD
		if (imcoo != NULL && (xpfield > 0 || ypfield > 0)) {
		    if (! IS_INDEFD(raval) && ! IS_INDEFD(decval)) {
			call sk_ultran (catcoo, imcoo, raval, decval, iraval,
			    idecval, 1)
			call mw_c2trand (ct, iraval, idecval, xpval, ypval) 
		    } else {
			xpval = INDEFD
			ypval = INDEFD
		    }
		}

		# Reformat the record.
		nchars = at_mkrecord (flist, res, Memc[record], SZ_LINE,
		    Memi[sindex+i-1], rafield, decfield, oraval, odecval,
		    xpfield, ypfield, xpval, ypval) 

	    }

	    # Write the new record.
            if (nchars > 0) {
	        call fprintf (fd, "%s")
	            call pargstr (Memc[record])
	    }
	}

	# Free the selection expression descriptor.
	if (o != NULL)
	    call evvfree (o)

	# Free the catalog, output, and field coordinate system descriptors.
	if (catcoo != NULL)
	    call sk_close (catcoo)
	if (outcoo != NULL)
	    call sk_close (outcoo)
	if (imcoo != NULL)
	    call sk_close (imcoo)
	if (mwim != NULL)
	    call mw_close (mwim)

	# Free output field list.
	if (flist != NULL)
	    call at_flfree (flist)

	# Free thesort index descriptor.
	call mfree (sindex, TY_INT)

	call sfree (sp)
end


# AT_FCATHDR -- Write the filtered catalog header

int procedure at_wfcathdr (fd, at, res, fl)

int	fd			#I the output file descriptor
pointer	at			#I the astrometry pacakge descriptor
pointer	res			#I the results descriptor descriptor
pointer	fl			#I the output field list descriptor

pointer	sp, catname, qpnames, qpvalues, qpunits, fname, fvalue, funits
int	i, nlines, nfields
int	cq_rstati(), at_wrdstr(), cq_hinfon()
char	cq_itype()
bool	streq(), strne()

begin
	nlines = 0

        # Allocate working space.
        call smark (sp)
        call salloc (catname, SZ_FNAME, TY_CHAR)
        call salloc (qpnames, SZ_LINE, TY_CHAR)
        call salloc (qpvalues, SZ_LINE, TY_CHAR)
        call salloc (qpunits, SZ_LINE, TY_CHAR)
        call salloc (fname, CQ_SZ_QPNAME, TY_CHAR)
        call salloc (fvalue, CQ_SZ_QPVALUE, TY_CHAR)
        call salloc (funits, CQ_SZ_QPUNITS, TY_CHAR)

        # Write the header banner.
        call fprintf (fd, "# BEGIN CATALOG HEADER\n")
        nlines = nlines + 1

        # Write the catalog database and id.
        call cq_rstats (res, CQRCATDB, Memc[catname], SZ_FNAME)
        call fprintf (fd, "# catdb %s\n")
            call pargstr (Memc[catname])
        nlines = nlines + 1
        call cq_rstats (res, CQRCATNAME, Memc[catname], SZ_FNAME)
        call fprintf (fd, "# catname %s\n")
            call pargstr (Memc[catname])
        nlines = nlines + 1

        # Write out the query parameter names, values, and units used
        # to generate the catalog.
        call cq_rstats (res, CQRQPNAMES, Memc[qpnames], SZ_LINE)
        call cq_rstats (res, CQRQPVALUES, Memc[qpvalues], SZ_LINE)
        call cq_rstats (res, CQRQPUNITS, Memc[qpunits], SZ_LINE)
        nfields = cq_rstati (res, CQRNQPARS)
        call fprintf (fd, "# nquery %d\n")
            call pargi (nfields)
        nlines = nlines + 1
        do i = 1, nfields {
            if (at_wrdstr (i, Memc[fname], CQ_SZ_QPNAME, Memc[qpnames]) != i)
                ;
            if (at_wrdstr (i, Memc[fvalue], CQ_SZ_QPVALUE, Memc[qpvalues]) != i)
                ;
            if (at_wrdstr (i, Memc[funits], CQ_SZ_QPUNITS, Memc[qpunits]) != i)
                ;
            call fprintf (fd, "#     %s %s %s\n")
                call pargstr (Memc[fname])
                call pargstr (Memc[fvalue])
                call pargstr (Memc[funits])
            nlines = nlines + 1
        }

        # Write out the results format type.
        if (at_wrdstr (cq_rstati(res, CQRTYPE), Memc[fvalue], CQ_SZ_QPVALUE,
            CQ_RTYPESTR) <= 0)
            call strcpy ("stext", Memc[fvalue], CQ_SZ_QPVALUE)
        call fprintf (fd, "# type %s\n")
            call pargstr (Memc[fvalue])
        nlines = nlines + 1

        # Write out the header parameters,
        nfields = cq_rstati (res, CQNHEADER)
        call fprintf (fd, "# nheader %d\n")
            call pargi (nfields)
        nlines = nlines + 1
        do i = 1, nfields {
            if (cq_hinfon (res, i, Memc[fname], CQ_SZ_QPNAME, Memc[fvalue],
                CQ_SZ_QPVALUE) != i)
                next
			
	     # Check for a changed coordinate system here
	     if (streq ("csystem", Memc[fname])) {
		call at_stats (at, FOSYSTEM, Memc[qpvalues], SZ_LINE)
		if (Memc[qpvalues] != EOS && strne (Memc[qpvalues],
		    Memc[fvalue]))
		    call strcpy (Memc[qpvalues], Memc[fvalue], CQ_SZ_QPVALUE)
	     }

            call fprintf (fd, "#     %s %s\n")
                call pargstr (Memc[fname])
                call pargstr (Memc[fvalue])
            nlines = nlines + 1
        }

	# Write out the field desription.
	nfields = FL_NFIELDS(fl)
        call fprintf (fd, "# nfields %d\n")
            call pargi (nfields)
	do i = 0, nfields - 1 {
            call fprintf (fd, "#     %s %d %d %c %s %s\n")
                call pargstr (Memc[FL_FNAMES(fl)+i*(CQ_SZ_QPNAME+1)])
                call pargi (Memi[FL_FOFFSETS(fl)+i])
                call pargi (Memi[FL_FSIZES(fl)+i])
                call pargc (cq_itype (Memi[FL_FTYPES(fl)+i]))
                call pargstr (Memc[FL_FUNITS(fl)+i*(CQ_SZ_QPUNITS+1)])
                call pargstr (Memc[FL_FFMTS(fl)+i*(CQ_SZ_QPFMTS+1)])
            nlines = nlines + 1
	}

        # Write the header trailer.
        call fprintf (fd, "# END CATALOG HEADER\n#\n")
        nlines = nlines + 1

	call sfree (sp)

	return (nlines)
end


# AT_SRTCAT -- Sort the catalog on the user specified field.

int procedure at_srtcat (at, res, sindex, max_nrecs)

pointer	at			#I the astrometry package descriptor
pointer	res			#I results descriptor
int	sindex[ARB]		#O the output sort index 
int	max_nrecs		#I the maximum number of records

double	dval
pointer	sp, sexpr, sfield, sname, sval, darray, carray, o
int	i, ip, nrecs, stype, snum, nchars, sz_carray
pointer	evvexpr(), locpr()
int	ctotok(), cq_fnumber(), cq_ftype(), cq_fname(), ctoi(), cq_gvald()
int	cq_gvalc(), gstrcpy(), cq_setrecord(), at_flnexpr(), at_stati()
bool	streq()
extern	at_getop()

begin
	call smark (sp)
	call salloc (sexpr, FL_SZ_EXPR, TY_CHAR)
	call salloc (sfield, FL_SZ_EXPR, TY_CHAR)
	call salloc (sname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (sval, SZ_LINE, TY_CHAR)

	# Get the sort expression.
	call at_stats (at, FSORT, Memc[sexpr], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[sexpr], i, Memc[sfield], FL_SZ_EXPR) == EOF)
	    Memc[sfield] = EOS

	# Return initialized index array if the sort expression is undefined.
	if (Memc[sfield] == EOS) {
	    do i = 1, max_nrecs
	        sindex[i] = i
	    call sfree (sp)
	    return (max_nrecs)
	}

	# Determine the type of sort. If sfield and sname are identical
	# sort expression is a field, otherwise it is an expression which
	# must be evaluated.
	ip = 1
	if (ctotok (Memc[sfield], ip, Memc[sname], CQ_SZ_QPNAME) ==
	    TOK_IDENTIFIER)
	    ;

	# Initialize the sort index array.
	do i = 1, max_nrecs
	    sindex[i] = i

	# The sort expression is a simple field. 
	if (streq (Memc[sfield], Memc[sname])) {

	    if (cq_fnumber (res, Memc[sfield]) > 0) {    # Catalog field name
	        stype = cq_ftype (res, Memc[sfield])
	    } else if (Memc[sfield] == 'f') {            # Generic f# name
		ip = 2
		if (ctoi (Memc[sfield], ip, snum) <= 0)
		    stype = INDEFI
		else if (cq_fname (res, snum, Memc[sname], CQ_SZ_FNAME) <= 0)
		    stype = INDEFI
		else
	            stype = cq_ftype (res, Memc[sname])
	    } else {                                     # Unknown name.
		stype = INDEFI
	    }

	    # Do the sort.
	    if (IS_INDEFI(stype)) {		# Field is undecodable.
		nrecs = max_nrecs

	    } else if (stype == TY_CHAR) {      # Character sort.
		sz_carray = 10 * SZ_LINE
		call malloc (carray, sz_carray, TY_CHAR)  
		ip = 1
		do i = 1, max_nrecs {
		    nchars = cq_gvalc (res, i, Memc[sname], Memc[sval],
		        SZ_LINE) 
		    if (nchars > sz_carray - ip + 1) {
			sz_carray = sz_carray + 10 * SZ_LINE
			call realloc (carray, sz_carray, TY_CHAR)
		    }
		    sindex[i] = ip
		    ip = ip + gstrcpy (Memc[sval], Memc[carray+ip-1], nchars)
		    Memc[carray+ip-1] = EOS
		    ip = ip + 1
		}
		call at_ssquick (Memc[carray], sindex, sindex, max_nrecs)
		call mfree (carray, TY_CHAR)
		nrecs = max_nrecs

	    } else {                            # Numeric sort.
		call malloc (darray, max_nrecs, TY_DOUBLE)
		do i = 1, max_nrecs {
		    nchars = cq_gvald (res, i, Memc[sname], dval) 
		    if (nchars <= 0)
			Memd[darray+i-1] = INDEFD
		    else
			Memd[darray+i-1] = dval
		}
		call at_qsortd (Memd[darray], sindex, sindex, max_nrecs)
		call mfree (darray, TY_DOUBLE)
		nrecs = max_nrecs
	    }

	# The sort field is an expression which must be evaluated.
	} else {

	    # Determine the data type of the output from the first record.
	    if (cq_setrecord (res, 1) != 1)
		;
	    o = evvexpr (Memc[sfield], locpr (at_getop), res, 0, res, 0)
	    stype = O_TYPE(o)
	    call evvfree (o)

	    if (stype == 0)                  # Expression cannot be decoded.
		nrecs = max_nrecs
	    else if (stype == TY_CHAR || stype == TY_BOOL) {
		sz_carray = 10 * SZ_LINE
		call malloc (carray, sz_carray, TY_CHAR)  
		ip = 1
		do i = 1, max_nrecs {
	            if (cq_setrecord (res, i) != i)
			break
	    	    o = evvexpr (Memc[sfield], locpr (at_getop), res, 0, res, 0)
		    if (O_LEN(o) > sz_carray - ip + 1) {
			sz_carray = sz_carray + 10 * SZ_LINE
			call realloc (carray, sz_carray, TY_CHAR)
		    }
		    sindex[i] = ip
		    ip = ip + gstrcpy (O_VALC(o), Memc[carray+ip-1], O_LEN(o))
		    Memc[carray+ip-1] = EOS
		    ip = ip + 1
	    	    call evvfree (o)
		}

		call at_ssquick (Memc[carray], sindex, sindex, max_nrecs)
		call mfree (carray, TY_CHAR)
		nrecs = max_nrecs
	    } else {
		call malloc (darray, max_nrecs, TY_DOUBLE)
		do i = 1, max_nrecs {
	            if (cq_setrecord (res, i) != i)
			break
	    	    o = evvexpr (Memc[sfield], locpr (at_getop), res, 0, res, 0)
		    switch (O_TYPE(o)) {
		    case TY_SHORT:
			dval = O_VALS(o)
		    case TY_INT:
			dval = O_VALI(o)
		    case TY_LONG:
			dval = O_VALL(o)
		    case TY_REAL:
			dval = O_VALR(o)
		    case TY_DOUBLE:
		        dval = O_VALD(o)
		    default:
			dval = INDEFD
		    }
		    Memd[darray+i-1] = dval
	    	    call evvfree (o)
		}
		call at_qsortd (Memd[darray], sindex, sindex, max_nrecs)
		call mfree (darray, TY_DOUBLE)
		nrecs = max_nrecs
	    }

	}

	# Flip the index array if the sense of the sort is reversed.
	if (at_stati (at, FREVERSE) == YES) {
	    do i = 1, nrecs / 2 {
		ip = sindex[i]
		sindex[i] = sindex[nrecs-i+1]
		sindex[nrecs-i+1] = ip
	    }
	}

	call sfree (sp)

	return (nrecs)
end


# AT_COWCS -- Initialize the catalog and output coordinate system
# descriptors.

procedure at_cowcs (at, res, im, catcoo, outcoo, imcoo, mwim)

pointer	at			#I the astrometry package descriptor
pointer	res			#I the catalog results descriptor
pointer	im			#I the associated image descriptor
pointer	catcoo			#O the output catalog system descriptor
pointer	outcoo			#O the output output system descriptor
pointer imcoo			#O the output image system descriptor
pointer	mwim			#O the output image mwcs descriptor

pointer	sp, csystem, cfield, fra, fdec, mw
int	i, catstat, outstat, imstat
int	cq_hinfo(), sk_decwcs(), strdic(), at_wrdstr(), sk_stati()
int	at_stati(), sk_decim()

begin
	call smark (sp)
	call salloc (csystem, SZ_LINE, TY_CHAR)
	call salloc (cfield, SZ_LINE, TY_CHAR)
	call salloc (fra, SZ_FNAME, TY_CHAR)
	call salloc (fdec, SZ_FNAME, TY_CHAR)

	# Get the catalog system.
	if (cq_hinfo (res, "csystem", Memc[csystem], SZ_LINE) <= 0)
	    call strcpy ("", Memc[csystem], SZ_LINE)

	# Open the catalog system.
        catstat = sk_decwcs (Memc[csystem], mw, catcoo, NULL)
        if (catstat == ERR || mw != NULL) {
            #call eprintf (
		#"Error: Cannot decode the catalog coordinate system\n")
            if (mw != NULL)
                call mw_close (mw)
	    call sk_close (catcoo)
	    catcoo = NULL
	    imcoo = NULL
	    outcoo = NULL
	    call sfree (sp)
	    return
	}


	# Get and set the ra catalog coordinate units.
	call at_stats (at, FIRA, Memc[fra], SZ_FNAME)
	call cq_funits (res, Memc[fra], Memc[cfield], SZ_LINE)
	i = strdic (Memc[cfield], Memc[cfield], SZ_LINE, SKY_LNG_UNITLIST) 
	if (i > 0)
	    call sk_seti (catcoo, S_NLNGUNITS, i)

	# Get and set the dec catalog coordinate units.
	call at_stats (at, FIDEC, Memc[fdec], SZ_FNAME)
	call cq_funits (res, Memc[fdec], Memc[cfield], SZ_LINE)
	i = strdic (Memc[cfield], Memc[cfield], SZ_LINE, SKY_LAT_UNITLIST) 
	if (i > 0)
	    call sk_seti (catcoo, S_NLATUNITS, i)

	# Open the output coordinate system if the output coordinate system is
	# different from the catalog coordinate system or the units are
	# different.
	call at_stats (at, FOSYSTEM, Memc[csystem], SZ_LINE)
	if (Memc[csystem] != EOS || at_stati(at,FORAUNITS) > 0 || at_stati(at,
	    FODECUNITS) > 0) {

	    if (Memc[csystem] == EOS)
                outstat = sk_decwcs (Memc[csystem], mw, outcoo, catcoo)
	    else
                outstat = sk_decwcs (Memc[csystem], mw, outcoo, NULL)

            if (outstat == ERR || mw != NULL) {
                #call eprintf (
		    #"Error: Cannot decode the output coordinate system\n")
                if (mw != NULL)
                    call mw_close (mw)
		call sk_close (outcoo)
	        outcoo = NULL
            } else {

		# Set the output catalog ra units.
		i = at_stati (at, FORAUNITS)
		if (i <= 0) {
		    Memc[cfield] = EOS
		} else if (at_wrdstr (i, Memc[cfield], SZ_LINE,
		    AT_RA_UNITS) <= 0) {
		    Memc[cfield] = EOS
		}
		if (Memc[cfield] == EOS) {
		    call sk_seti (outcoo, S_NLNGUNITS, sk_stati (catcoo,
		        S_NLNGUNITS))
		} else {
		    i = strdic (Memc[cfield], Memc[cfield], FL_SZ_EXPR,
		        SKY_LNG_UNITLIST)
		    if (i > 0)
		        call sk_seti (outcoo, S_NLNGUNITS, i)
		    else
		        call sk_seti (outcoo, S_NLNGUNITS, sk_stati(catcoo,
		            S_NLNGUNITS))
		}

		# Set the output catalog dec units.
		i = at_stati (at, FODECUNITS)
		if (i <= 0) {
		    Memc[cfield] = EOS
		} else if (at_wrdstr (i, Memc[cfield], SZ_LINE,
		    AT_DEC_UNITS) <= 0) {
		    Memc[cfield] = EOS
		}
		if (Memc[cfield] == EOS) {
		    call sk_seti (outcoo, S_NLATUNITS, sk_stati (catcoo,
		        S_NLATUNITS))
		} else {
		    i = strdic (Memc[cfield], Memc[cfield], SZ_LINE,
			SKY_LAT_UNITLIST)
		    if (i > 0)
		        call sk_seti (outcoo, S_NLATUNITS, i)
		    else
			call sk_seti (outcoo, S_NLATUNITS, sk_stati(catcoo,
			   S_NLATUNITS))
		}

	    }
	} else {
	    outcoo = NULL
	}

	# Open the image coordinate system.
	if (im == NULL) {
	    imcoo = NULL
	    mwim = NULL
	} else {
	    imstat = sk_decim (im, "logical", mwim, imcoo)
	    if (imstat == ERR || mwim == NULL) {
                if (mwim != NULL)
                    call mw_close (mwim)
		mwim = NULL
		call sk_close (outcoo)
	        outcoo = NULL
	    } else {
		call sk_seti (imcoo, S_NLNGUNITS, SKY_DEGREES)
		call sk_seti (imcoo, S_NLATUNITS, SKY_DEGREES)
	    }
	}

	call sfree (sp)
end


# AT_COOFIELDS -- Get the sequence number of the coordinate output fields.

procedure at_coofields (at, res, flist, raname, decname, rafield, decfield,
    xpfield, ypfield, xcfield, ycfield) 


pointer	at			#I the astrometry package descriptor
pointer	res			#I the output results descriptor
pointer	flist			#I the output field list descriptor
char	raname[ARB]		#O the catalog ra name
char	decname[ARB]		#O the catalog dec name
int	rafield			#O the output ra field no
int	decfield		#O the output dec field no
int	xpfield			#O the output xp field no
int	ypfield			#O the output yp field no
int	xcfield			#O the output xp field no
int	ycfield			#O the output yp field no

pointer	sp, xpname, ypname, str
int	i
int	at_flnexpr(), cq_fnumber()
bool	streq()

begin
	# Get working space.
	call smark (sp)
	call salloc (xpname, FL_SZ_EXPR, TY_CHAR)
	call salloc (ypname, FL_SZ_EXPR, TY_CHAR)
	call salloc (str, FL_SZ_EXPR, TY_CHAR)

	# Initialize.
	rafield = 0
	decfield = 0
	xpfield = 0
	ypfield = 0
	xcfield = 0
	ycfield = 0

	# Get the ra and dec field names.
	call at_stats (at, FIRA, Memc[str], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[str], i, raname, FL_SZ_EXPR) == EOF)
	    raname[1] = EOS
	call at_stats (at, FIDEC, Memc[str], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[str], i, decname, FL_SZ_EXPR) == EOF)
	    decname[1] = EOS

	# Get the predicted x and y field names.
	call at_stats (at, FIXP, Memc[str], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[str], i, Memc[xpname], FL_SZ_EXPR) == EOF)
	    Memc[xpname] = EOS
	call at_stats (at, FIYP, Memc[str], FL_SZ_EXPR)
	i = 1
	if (at_flnexpr (res, Memc[str], i, Memc[ypname], FL_SZ_EXPR) == EOF)
	    Memc[ypname] = EOS

	# Get the center x and y field names. Ignore this for now.

	# Check to see whether the field names are in the input catalog
	# and whether at least one of them is in the output catalog.
	if (cq_fnumber (res, raname) > 0 && cq_fnumber (res, decname) > 0) {
	    do i = 0, FL_NFIELDS(flist) - 1 {
		if (streq (raname, Memc[FL_FNAMES(flist)+i*
		    (CQ_SZ_QPNAME+1)])) {
		    rafield = i + 1
		} else if (streq (decname, Memc[FL_FNAMES(flist)+i*
		    (CQ_SZ_QPNAME+1)])) {
		    decfield = i + 1
		} else if (streq (Memc[xpname], Memc[FL_FNAMES(flist)+i*
		    (CQ_SZ_QPNAME+1)])) {
		    xpfield = i + 1
		} else if (streq (Memc[ypname], Memc[FL_FNAMES(flist)+i*
		    (CQ_SZ_QPNAME+1)])) {
		    ypfield = i + 1
		}
		#if (rafield > 0 && decfield > 0)
		    #break
	    }
	}

	call sfree (sp)
end


# AT_MKRECORD -- Format the output catalog record.

int procedure at_mkrecord (flist, res, record, maxch, recno, rafield, decfield,
	raval, decval, xpfield, ypfield, xpval, ypval) 

pointer	flist				#I the output field list descriptor
pointer	res				#I the output results descriptor
char	record[ARB]			#O the output record
int	maxch				#I the maximum size of a record
int	recno				#I the current record number
int	rafield				#I the output ra field
int	decfield			#I the output dec field
double	raval				#I the input ra value
double	decval				#I the input dec value
int	xpfield				#I the output predicted x field
int	ypfield				#I the output predicted y field
double	xpval				#I the input predicted x value
double	ypval				#I the input predicted y value

pointer	sp, newval, eptr, rptr, o
int	i, j, k, op, findex, nchars
pointer	evvexpr(), locpr()
int	gstrcpy(), cq_rstati(), cq_gvalc(), strlen()
extern	at_getop()

begin
	call smark (sp)
	call salloc (newval, SZ_LINE, TY_CHAR)

	# Initialize.
	findex = 0
	op = 1
	record[op] = EOS
	o = NULL
	eptr = FL_FLIST(flist)
	rptr = FL_FRANGES(flist)

	# Loop over the expressions.
	do i = 1, FL_NEXPR(flist) {

	    # The output field is an expression.
	    if (IS_INDEFI(Memi[rptr])) {

		# Evaluate the expression.
		if (xpfield == (findex + 1)) {
		    call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		        findex * (CQ_SZ_QPFMTS + 1)])
			call pargd (xpval)
		} else if (ypfield == (findex + 1)) {
		    call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		        findex * (CQ_SZ_QPFMTS + 1)])
			call pargd (ypval)
		} else {

		    if (o != NULL)
		        call evvfree (o)
		    o = evvexpr (Memc[eptr], locpr (at_getop), res, 0, res, 0)

		    # Encode the expression in a string.
		    switch (O_TYPE(o)) {
		    case TY_CHAR:
		        call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		            findex * (CQ_SZ_QPFMTS + 1)])
			    call pargstr (O_VALC(o))
		    case TY_INT:
		        call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		            findex * (CQ_SZ_QPFMTS + 1)])
			    call pargi (O_VALI(o))
		    case TY_REAL:
		        call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		            findex * (CQ_SZ_QPFMTS + 1)])
			    call pargr (O_VALR(o))
		    case TY_DOUBLE:
		        call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		            findex * (CQ_SZ_QPFMTS + 1)])
			    call pargd (O_VALD(o))
		    default:
		        call sprintf (Memc[newval], maxch, Memc[FL_FFMTS(flist)+
		            findex * (CQ_SZ_QPFMTS + 1)])
			    call pargstr (O_VALC(o))
		    }
		}

		# Copy the string to the output record.
		if (Memi[FL_FSIZES(flist)+findex] == 0) {
		    op = op + gstrcpy (" ", record[op], maxch - op + 1)
		    op = op + gstrcpy (Memc[newval], record[op], maxch - op + 1)
		    op = op + gstrcpy (" ", record[op], maxch - op + 1)
		} else {
		    nchars = min (Memi[FL_FSIZES(flist)+findex],
		        strlen (Memc[newval]))
		    do k = 1, Memi[FL_FSIZES(flist)+findex] - nchars - 1 {
			if (op > maxch)
			    break
			record[op] = ' '
			op = op + 1
		    }
		    op = op + gstrcpy (Memc[newval], record[op], min (nchars,
		        maxch - op + 1))
		    op = op + gstrcpy (" ", record[op], maxch - op + 1)
		}

		findex = findex + 1

	    # The field expression are input catalog columns.
	    } else if (Memi[rptr] >= 1 && Memi[rptr+1] <= cq_rstati (res,
		CQNFIELDS)) {

		# Loop over the fields in each range.
		do j = max (1, Memi[rptr]), min (Memi[rptr+1], cq_rstati(res,
		    CQNFIELDS)), Memi[rptr+2] {

		    # Encode the record values.
		    if (rafield == (findex + 1)) {
		        call sprintf (Memc[newval], SZ_LINE,
			    Memc[FL_FFMTS(flist)+ findex * (CQ_SZ_QPFMTS + 1)])
			    call pargd (raval)
			nchars = strlen (Memc[newval])
		    } else if (decfield == (findex + 1)) {
		        call sprintf (Memc[newval], SZ_LINE,
			    Memc[FL_FFMTS(flist)+ findex * (CQ_SZ_QPFMTS + 1)])
			    call pargd (decval)
			nchars = strlen (Memc[newval])
		    } else if (xpfield == (findex + 1)) {
		        call sprintf (Memc[newval], SZ_LINE,
			    Memc[FL_FFMTS(flist)+ findex * (CQ_SZ_QPFMTS + 1)])
			    call pargd (xpval)
			nchars = strlen (Memc[newval])
		    } else if (ypfield == (findex + 1)) {
		        call sprintf (Memc[newval], SZ_LINE,
			    Memc[FL_FFMTS(flist)+ findex * (CQ_SZ_QPFMTS + 1)])
			    call pargd (ypval)
			nchars = strlen (Memc[newval])
		    } else {
			nchars = cq_gvalc (res, recno, Memc[FL_FNAMES(flist)+
			    findex*(CQ_SZ_QPNAME+1)], Memc[newval], SZ_LINE)
		    }

		    # Copy the string to the output record.
		    if (Memi[FL_FSIZES(flist)+findex] == 0) {
			if ((j == 1) && (! IS_WHITE(Memc[newval])))
		            op = op + gstrcpy (" ", record[op], maxch - op + 1)
			else if (rafield == (findex + 1) || decfield ==
			    (findex + 1) || xpfield == (findex + 1) ||
			    ypfield == (findex + 1))
		            op = op + gstrcpy (" ", record[op], maxch - op + 1)
		        op = op + gstrcpy (Memc[newval], record[op],
			    maxch - op + 1)
			if (rafield == (findex + 1) || decfield ==
			    (findex + 1) || xpfield == (findex + 1) ||
			    ypfield == (findex + 1))
		            op = op + gstrcpy (" ", record[op], maxch - op + 1)
		    } else {
		        nchars = min (Memi[FL_FSIZES(flist)+findex], nchars)
		        do k = 1, Memi[FL_FSIZES(flist)+findex] - nchars - 1 {
			    if (op > maxch)
			        break
			    record[op] = ' '
			    op = op + 1
		        }
		        op = op + gstrcpy (Memc[newval], record[op],
			    min (nchars, maxch - op + 1))
		        op = op + gstrcpy (" ", record[op], maxch - op + 1)
		    }

		    findex = findex + 1
		}
	    }

	    # Increment the expression and ranges pointers.
	    eptr = eptr + FL_SZ_EXPR + 1
	    rptr = rptr + 3
	}
	if (o != NULL)
	    call evvfree (o)

	# Append a newline and EOS to the data.
	if (record[1] != EOS) {
	    record[op] = '\n'
	    record[op+1] = EOS
	}

	call sfree (sp)

	return (op - 1)
end


# AT_NFLIST -- Add new fields to the current output field list and optionally
# specify the field names, field types, field units, and field formats.

procedure at_nflist (at, nfields, nfnames, nftypes, nfunits, nformats, append)

pointer	at			#I the astrometry package descriptors
int	nfields			#I the number of new fields
char	nfnames[ARB]		#I the new field names list
char	nftypes[ARB]		#I the new field types list
char	nfunits[ARB]		#I the new field units list
char	nformats[ARB]		#I the new field formats list
bool	append			#I append the new fields

pointer	sp, str1, str2
int	i, op1, op2
int	gstrcpy(), strlen

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Set the new output field expressions to INDEF and retrieve the
	# original user fields value.
	call at_stats (at, FIELDS, Memc[str1], SZ_LINE)
	op1 = strlen (Memc[str1])
	op2 = 0
	do i = 1, nfields {
	    if (i == 1) {
		op2 = op2 + gstrcpy ("INDEF", Memc[str2+op2], SZ_LINE - op2) 
	    } else {
		op2 = op2 + gstrcpy (",", Memc[str2+op2], SZ_LINE - op2) 
		op2 = op2 + gstrcpy ("INDEF", Memc[str2+op2], SZ_LINE - op2) 
	    }
	}

	# Construct the new output fields string.
	if (append) {
	    op1 = op1 + gstrcpy (",", Memc[str1+op1], SZ_LINE - op1)
	    op1 = op1 + gstrcpy (Memc[str2], Memc[str1+op1], SZ_LINE - op1)
	    call at_sets (at, FIELDS, Memc[str1])
	} else {
	    op2 = op 2+ gstrcpy (",", Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (Memc[str1], Memc[str2+op2], SZ_LINE - op2)
	    call at_sets (at, FIELDS, Memc[str2])
	}

	# Construct the new field names.
	call at_stats (at, FNAMES, Memc[str1], SZ_LINE)
	op1 = strlen (Memc[str1])
	op2 = 0
	if (append) {
	    op1 = op1 + gstrcpy (",", Memc[str1+op1], SZ_LINE - op1)
	    op1 = op1 + gstrcpy (nfnames, Memc[str1+op1], SZ_LINE - op1)
	    call at_sets (at, FNAMES, Memc[str1])
	} else {
	    op2 = op2 + gstrcpy (nfnames, Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (",", Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (Memc[str1], Memc[str2+op2], SZ_LINE - op2)
	    call at_sets (at, FNAMES, Memc[str2])
	}

	# Construct the new field types.
	call at_stats (at, FNTYPES, Memc[str1], SZ_LINE)
	op1 = strlen (Memc[str1])
	op2 = 0
	if (append) {
	    op1 = op1 + gstrcpy (",", Memc[str1+op1], SZ_LINE - op1)
	    op1 = op1 + gstrcpy (nftypes, Memc[str1+op1], SZ_LINE - op1)
	    call at_sets (at, FNTYPES, Memc[str1])
	} else {
	    op2 = op2 + gstrcpy (nftypes, Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (",", Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (Memc[str1], Memc[str2+op2], SZ_LINE - op2)
	    call at_sets (at, FNTYPES, Memc[str2])
	}

	# Construct the new field units.
	call at_stats (at, FNUNITS, Memc[str1], SZ_LINE)
	op1 = strlen (Memc[str1])
	op2 = 0
	if (append) {
	    op1 = op1 + gstrcpy (",", Memc[str1+op1], SZ_LINE - op1)
	    op1 = op1 + gstrcpy (nfunits, Memc[str1+op1], SZ_LINE - op1)
	    call at_sets (at, FNUNITS, Memc[str1])
	} else {
	    op2 = op2 + gstrcpy (nfunits, Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (",", Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (Memc[str1], Memc[str2+op2], SZ_LINE - op2)
	    call at_sets (at, FNUNITS, Memc[str2])
	}

	# Construct the new field units.
	call at_stats (at, FNFORMATS, Memc[str1], SZ_LINE)
	op1 = strlen (Memc[str1])
	op2 = 0
	if (append) {
	    op1 = op1 + gstrcpy (",", Memc[str1+op1], SZ_LINE - op1)
	    op1 = op1 + gstrcpy (nformats, Memc[str1+op1], SZ_LINE - op1)
	    call at_sets (at, FNFORMATS, Memc[str1])
	} else {
	    op2 = op2 + gstrcpy (nformats, Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (",", Memc[str2+op2], SZ_LINE - op2)
	    op2 = op2 + gstrcpy (Memc[str1], Memc[str2+op2], SZ_LINE - op2)
	    call at_sets (at, FNFORMATS, Memc[str2])
	}

	call sfree (sp)
end


# AT_FLINIT -- Initialize the field list structure. This routines: 1)
# creates a list of fields, field ranges, and field expressions, 2) determines
# whether an output field is an input field or a field expression, and 3)
# assembles the information required to write a catalog header.


pointer procedure at_flinit (at, res)

pointer	at			#I the astrometry package descriptor
pointer	res			#I results descriptor

pointer	sp, fields
pointer	fl
int	nexpr, nfields
int	at_flelist(), at_flranges()

begin
	# Get some working space.
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)

	# Get the user field list.
	call at_stats (at, FIELDS, Memc[fields], SZ_LINE)

	# Allocate the field list descriptor.
	call calloc (fl, FL_FLENGTH, TY_STRUCT)

	# Create the field expression list.
	nexpr = at_flelist (res, Memc[fields], fl)
	if (nexpr > 0) {

	    # Determine which individual fields are to be output.
	    nfields = at_flranges (res, fl)

	    # Compile the new header info.
	    if (nfields > 0)
		call at_flflist (at, res, fl)
	}

	call sfree (sp)

	return (fl)
end


# AT_FLFREE -- Free the field list structure.

procedure at_flfree (fl)

pointer	fl			#I the field list descriptor

begin
	if (FL_FLIST(fl) != NULL)
	    call mfree (FL_FLIST(fl), TY_CHAR)
	if (FL_FRANGES(fl) != NULL)
	    call mfree (FL_FRANGES(fl), TY_STRUCT)

	if (FL_FNAMES(fl) != NULL)
	    call mfree (FL_FNAMES(fl), TY_CHAR)
	if (FL_FOFFSETS(fl) != NULL)
	    call mfree (FL_FOFFSETS(fl), TY_INT)
	if (FL_FSIZES(fl) != NULL)
	    call mfree (FL_FSIZES(fl), TY_INT)
	if (FL_FTYPES(fl) != NULL)
	    call mfree (FL_FTYPES(fl), TY_INT)
	if (FL_FUNITS(fl) != NULL)
	    call mfree (FL_FUNITS(fl), TY_CHAR)
	if (FL_FFMTS(fl) != NULL)
	    call mfree (FL_FFMTS(fl), TY_CHAR)

	call mfree (fl, TY_STRUCT)
end


# AT_FLELIST -- Create the expression list from the user field list.

int procedure at_flelist (res, fields, fl)

pointer	res			#I the results descriptor
char	fields[ARB]		#I the user field list
pointer	fl			#O the field list descriptor

int	i, ip, fp, nexpr
int	at_flnexpr()

begin
	# Allocate space for the expression list.
	call malloc (FL_FLIST(fl), FL_MAX_NEXPR * (FL_SZ_EXPR + 1), TY_CHAR)

	# Decode the user field list into a list of comma separated
	# expressions. Expressions may be field names (e.g. "ra" or "f2"),
	# field ranges (e.g. "f[*]" or "f[1-4]"), or field expressions
	# (e.g. "f2 - f3" or "mag2 - mag1").

	ip = 1
	fp = FL_FLIST(fl)
	nexpr = 0
	do i = 1, FL_MAX_NEXPR {
	    if (at_flnexpr (res, fields, ip, Memc[fp], FL_SZ_EXPR) == EOF)
		break
	    #call strlwr (Memc[fp])
	    fp = fp + FL_SZ_EXPR + 1
	    nexpr = nexpr + 1
	}
	call realloc (FL_FLIST(fl), nexpr * (FL_SZ_EXPR + 1), TY_CHAR)
	FL_NEXPR(fl) = nexpr

	return (nexpr)
end


# AT_FLNEXPR -- Get the next expression from an expression list.

int procedure at_flnexpr (res, exprlist, ip, expr, maxch)

pointer	res			#I pointer to the results descriptor
char    exprlist[ARB]           #I the input expression list
int     ip                      #I pointer into the expression list 
char    expr[ARB]               #O the output expression
int     maxch                   #I maximum length of the output expression

int     ep, op, token, fnum
int     ctotok(), strlen(), cq_fnumber(), ctoi(), cq_rstati(), cq_fname()

begin
        # Decode the column labels.
        op = 1
        while (exprlist[ip] != EOS) {

            token = ctotok (exprlist, ip, expr[op], maxch)
            if (expr[op] == EOS)
                next

            if ((token == TOK_PUNCTUATION) && (expr[op] == ',')) {
                if (op == 1)
                    next
                else
                    break
            }

	    # Replace generic identifiers with their catalog equivalents.
	    if (token == TOK_IDENTIFIER) {
	        fnum = cq_fnumber (res, expr[op])
		if (fnum <= 0 && expr[op] == 'f') {
		    ep = 2
		    if (ctoi (expr[op], ep, fnum) <= 0)
			fnum = 0
		    if (fnum < 1 || fnum > cq_rstati (res, CQRNRECS))
			fnum = 0
		    if (fnum > 0) {
			if (cq_fname (res, fnum, expr[op], maxch) != fnum)
			    ;
		    }
		}
	    }


            op = op + strlen (expr[op])
        }

        expr[op] = EOS
        if ((exprlist[ip] == EOS) && (op == 1))
            return (EOF)
        else
            return (op - 1)
end


# AT_FLNITEM -- Get the next expression from an expression list.

int procedure at_flnitem (itemlist, ip, item, maxch)

char    itemlist[ARB]           #I the input item list
int     ip                      #I pointer into the item list 
char    item[ARB]               #O the output item
int     maxch                   #I maximum length of the output item

int     op, token
int     ctotok(), strlen()

begin
        # Decode the column labels.
        op = 1
        while (itemlist[ip] != EOS) {

            token = ctotok (itemlist, ip, item[op], maxch)
            if (item[op] == EOS)
                next

            if ((token == TOK_PUNCTUATION) && (item[op] == ',')) {
                if (op == 1)
                    next
                else
                    break
            }

            op = op + strlen (item[op])
        }

        item[op] = EOS
        if ((itemlist[ip] == EOS) && (op == 1))
            return (EOF)
        else
            return (op - 1)
end


# AT_FLRANGES -- Get the field ranges for each output field. 

int procedure at_flranges (res, fl)

pointer	res			#I the results descriptor
pointer	fl			#I the field list descriptor

pointer	sp, fname, fptr, rptr
int	i, nin, nout, lindex, rindex, ip1, ip2, f1, f2
char	lbracket, rbracket
int	cq_rstati(), strldx(), ctoi(), ctotok(), cq_fnumber()
bool	streq()
data	lbracket /'['/, rbracket /']'/

begin
	# Allocate working space.
	call smark (sp)
	call salloc (fname, FL_SZ_EXPR, TY_CHAR)

	# Allocate space for the ranges list.
	call calloc (FL_FRANGES(fl), 3 * FL_NEXPR(fl) + 1, TY_INT)

	# Initialize.
	nin = cq_rstati (res, CQNFIELDS)
	nout = 0

	# Loop over the expressions. Fields which cannot be decoded
	# have zero-valued range entries. Expression fields have INDEFI
	# valued range entries.
	fptr = FL_FLIST(fl)
	rptr = FL_FRANGES(fl)
	do i = 1, FL_NEXPR (fl) {

	    lindex = strldx (lbracket, Memc[fptr])
	    rindex = strldx (rbracket, Memc[fptr])
	    ip1 = 1
	    ip2 = lindex + 1

	    # Decode generic field ranges.
	    if (Memc[fptr] == 'f' && lindex == 2 && rindex > lindex) {

		# Find the range limits.
		if (Memc[fptr+lindex] == '*') {
		    f1 = 1
		    f2 = nin 
		} else {
		    if (ctoi (Memc[fptr], ip2, f1) <= 0)
			f1 = 0
		    else if (f1 < 1 || f1 > nin)
			f1 = 0
		    if (ctoi (Memc[fptr], ip2, f2) <= 0)
			f2 = 0
		    else
			f2 = -f2
		    if (f2 < 1 || f2 > nin)
			f2 = 0
		}

		# Valid range.
		if (f1 > 0 && f2 > f1) {
		    Memi[rptr] = f1
		    Memi[rptr+1] = f2
		    Memi[rptr+2] = 1
		    nout = nout + f2 - f1 + 1

		# Field cannot be decoded.
		} else {
		    Memi[rptr] = 0
		    Memi[rptr+1] = 0
		    Memi[rptr+2] = 0
		}

	    # Decode fields and expressions.
	    } else if (ctotok (Memc[fptr], ip1, Memc[fname], FL_SZ_EXPR) ==
	        TOK_IDENTIFIER) {

		# Find the field number.
		f1 = cq_fnumber (res, Memc[fptr])
		if (f1 <= 0 && streq (Memc[fptr], Memc[fname])) {
		    if (Memc[fptr] != 'f')
			f1 = 0
		    else {
		        f2 = 1
		        if (ctoi (Memc[fptr+1], f2, f1) <= 0)
			    f1 = 0
		        else if (f1 < 1 || f1 > nin)
			    f1 = 0
		    }
		}

		# Valid single field.
		if (f1 > 0) {
		    Memi[rptr] = f1
		    Memi[rptr+1] = f1
		    Memi[rptr+2] = 1
		    nout = nout + 1

		# Field is an expression.
		} else if (ctotok (Memc[fptr], ip1, Memc[fname],
		    FL_SZ_EXPR) != TOK_EOS) {
		    Memi[rptr] = INDEFI
		    Memi[rptr+1] = INDEFI
		    Memi[rptr+2] = INDEFI
		    nout = nout + 1

		# Field cannot be decoded.
		} else {
		    Memi[rptr] = 0
		    Memi[rptr+1] = 0
		    Memi[rptr+2] = 0
		}

	    # What's left over is an expression field.
	    } else {

		Memi[rptr] = INDEFI
		Memi[rptr+1] = INDEFI
		Memi[rptr+2] = INDEFI
		nout = nout + 1
	    }

	    fptr = fptr + FL_SZ_EXPR + 1
	    rptr = rptr + 3
	}

	# Store the field counts.
	FL_NFIELDS(fl) = nout

	call sfree (sp)

	return (nout)
end


# AT_FLFLIST -- Assemble the field header info for the new catalog.

procedure at_flflist (at, res, fl)

pointer	at			#I the astrometry package descriptor
pointer	res			#I the results descriptor
pointer	fl			#I the field list descriptor

pointer	sp, fnames, fntypes, fnunits, fnfmts, franame, fdecname, xpname
pointer	ypname, xcname, ycname, str, rptr
int	i, j, ip, nfields, fnp, ftp, fup, ffp, rtype, foffset, ival
int	cq_rstati(), ctotok(), at_dtype(), ctoi(), cq_finfon()
int	at_wrdstr(), at_stati(), at_flnitem()
bool	streq(), strne()

begin
	# Get the number of output fields.
	nfields = FL_NFIELDS(fl)
	if (nfields <= 0)
	    return

	# Get some working space.
	call smark (sp)
	call salloc (fnames, SZ_LINE, TY_CHAR)
	call salloc (fntypes, SZ_LINE, TY_CHAR)
	call salloc (fnunits, SZ_LINE, TY_CHAR)
	call salloc (fnfmts, SZ_LINE, TY_CHAR)
	call salloc (franame, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (fdecname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (xpname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (ypname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (xcname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (ycname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the user parameters defining the names, types, units, and
	# formats of the new fields.
	call at_stats (at, FNAMES, Memc[fnames], SZ_LINE)
	call at_stats (at, FNTYPES, Memc[fntypes], SZ_LINE)
	call at_stats (at, FNUNITS, Memc[fnunits], SZ_LINE)
	call at_stats (at, FNFORMATS, Memc[fnfmts], SZ_LINE)
	fnp = 1
	ftp = 1
	fup = 1
	ffp = 1

	# Get the special coordinate field names.
	call at_stats (at, FIRA, Memc[franame], CQ_SZ_FNAME)
	call at_stats (at, FIDEC, Memc[fdecname], CQ_SZ_FNAME)
	call at_stats (at, FIXP, Memc[xpname], CQ_SZ_FNAME)
	call at_stats (at, FIYP, Memc[ypname], CQ_SZ_FNAME)
	call at_stats (at, FIXC, Memc[xcname], CQ_SZ_FNAME)
	call at_stats (at, FIYC, Memc[ycname], CQ_SZ_FNAME)

	# Allocate space for the header field names, offsets, sizes, data
	# types, units, and formats.
	call calloc (FL_FNAMES(fl), nfields * (CQ_SZ_QPNAME + 1), TY_CHAR)
	call calloc (FL_FOFFSETS(fl), nfields, TY_INT)
	call calloc (FL_FSIZES(fl), nfields, TY_INT)
	call calloc (FL_FTYPES(fl), nfields, TY_INT)
	call calloc (FL_FUNITS(fl), nfields * (CQ_SZ_QPUNITS + 1), TY_CHAR)
	call calloc (FL_FFMTS(fl), nfields * (CQ_SZ_QPFMTS + 1), TY_CHAR)

	# Get the output type. This is the same as the input type.
	rtype = cq_rstati (res, CQRTYPE)

	# Loop over the ranges list.
	nfields = 0
	foffset = 1
	rptr = FL_FRANGES(fl)
	do i = 1, FL_NEXPR(fl) {

	    # Skip non-decodable fields.
	    if (Memi[rptr] == 0) {
		rptr = rptr + 3
		next
	    }

	    # The field is an input catalog field.
	    if (! IS_INDEFI (Memi[rptr])) {

		do j = Memi[rptr], Memi[rptr+1] {

		    # Get the field name, the field offset, size, data type,
		    # units and default format.
		    if (cq_finfon (res, j, Memc[FL_FNAMES(fl)+nfields*
		        (CQ_SZ_QPNAME+1)], CQ_SZ_QPNAME, Memi[FL_FOFFSETS(fl)+
			nfields], Memi[FL_FSIZES(fl)+nfields],
			Memi[FL_FTYPES(fl)+nfields],
			Memc[FL_FUNITS(fl)+nfields*(CQ_SZ_QPUNITS+1)],
			CQ_SZ_QPUNITS, Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS) != j)
			;

		    # Correct the field offset and field size.
		    switch (rtype) {
		    case CQ_STEXT:
			Memi[FL_FOFFSETS(fl)+nfields] = nfields + 1
			Memi[FL_FSIZES(fl)+nfields] = 0
		    case CQ_BTEXT:
			Memi[FL_FOFFSETS(fl)+nfields] = foffset
			foffset = foffset + Memi[FL_FSIZES(fl)+nfields]
		    default:
			call error (0, "Unknown output catalog type")
			;
		    }

		    # Correct for coordinate units and format transform here.
		    if (streq (Memc[franame], Memc[FL_FNAMES(fl)+
		        nfields*(CQ_SZ_QPNAME+1)])) {
			ival = at_stati (at, FORAUNITS)
			if (ival <= 0)
			    Memc[str] = EOS
			else if (at_wrdstr (ival, Memc[str], SZ_FNAME,
			    AT_RA_UNITS) <= 0)
			    Memc[str] = EOS
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FUNITS(fl)+nfields*(CQ_SZ_QPUNITS+1)]))
			    call strcpy (Memc[str], Memc[FL_FUNITS(fl)+
			        nfields*(CQ_SZ_QPUNITS+1)], CQ_SZ_QPUNITS)
			call at_stats (at, FORAFORMAT, Memc[str], SZ_FNAME)
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			    call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			        nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }
		    if (streq (Memc[fdecname], Memc[FL_FNAMES(fl)+
		        nfields*(CQ_SZ_QPNAME+1)])) {
			ival = at_stati (at, FODECUNITS)
			if (ival <= 0)
			    Memc[str] = EOS
			else if (at_wrdstr (ival, Memc[str], SZ_FNAME,
			    AT_DEC_UNITS) <= 0)
			    Memc[str] = EOS
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FUNITS(fl)+nfields*(CQ_SZ_QPUNITS+1)]))
			    call strcpy (Memc[str], Memc[FL_FUNITS(fl)+
			        nfields*(CQ_SZ_QPUNITS+1)], CQ_SZ_QPUNITS)
			call at_stats (at, FODECFORMAT, Memc[str], SZ_FNAME)
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			    call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			        nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }

		    # Correct for pixel coordinate formats here.
		    if (streq (Memc[xpname], Memc[FL_FNAMES(fl)+
		        nfields*(CQ_SZ_QPNAME+1)]) || streq (Memc[xcname],
			Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)])) {
			call at_stats (at, FOXFORMAT, Memc[str], SZ_FNAME)
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			    call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			        nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }
		    if (streq (Memc[ypname], Memc[FL_FNAMES(fl)+
		        nfields*(CQ_SZ_QPNAME+1)]) || streq (Memc[ycname],
			Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)])) {
			call at_stats (at, FOYFORMAT, Memc[str], SZ_FNAME)
			if (Memc[str] != EOS && strne (Memc[str],
			    Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			    call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			        nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }


		    nfields = nfields + 1
		}

	    # This field is a new field.
	    } else {

		# Get the field names. The default is f#.
		ip = 1
		if (at_flnitem (Memc[fnames], fnp, Memc[FL_FNAMES(fl)+
		    nfields*(CQ_SZ_QPNAME+1)], CQ_SZ_QPNAME) == EOF) {
		    call sprintf (Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)],
		        CQ_SZ_QPNAME, "f%d")
			call pargi (nfields + 1)
		} else if (ctotok (Memc[FL_FNAMES(fl)+nfields*
		    (CQ_SZ_QPNAME+1)], ip, Memc[FL_FNAMES(fl)+nfields*
		    (CQ_SZ_QPNAME+1)],  CQ_SZ_QPNAME) != TOK_IDENTIFIER) {
		    call sprintf (Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)],
		        CQ_SZ_QPNAME, "f%d")
			call pargi (nfields + 1)
		}

		# Get the data types. The default for now is type real.
		ip = 1
		if (at_flnitem (Memc[fntypes], ftp, Memc[str],
		    SZ_FNAME) == EOF) {
		    Memi[FL_FTYPES(fl)+nfields] = TY_REAL
		} else if (ctotok (Memc[str], ip, Memc[str], SZ_FNAME) !=
		    TOK_IDENTIFIER) {
		    Memi[FL_FTYPES(fl)+nfields] = TY_REAL
		} else {
		    #call strlwr (Memc[str])
		    #Memi[FL_FTYPES(fl)+nfields] = cq_dtype(Memc[str])
		    Memi[FL_FTYPES(fl)+nfields] = at_dtype(Memc[str])
		}

		# Get the data units. The default is INDEF.
		ip = 1
		if (at_flnitem (Memc[fnunits], fup, Memc[FL_FUNITS(fl)+
		    nfields*(CQ_SZ_QPUNITS+1)], CQ_SZ_QPUNITS) == EOF) {
		    call strcpy ("INDEF", Memc[FL_FUNITS(fl)+nfields*
		        (CQ_SZ_QPUNITS+1)], CQ_SZ_QPUNITS)
		} else if (ctotok (Memc[FL_FUNITS(fl)+nfields*
		    (CQ_SZ_QPUNITS+1)], ip, Memc[FL_FUNITS(fl)+nfields*
		    (CQ_SZ_QPUNITS+1)],  CQ_SZ_QPUNITS) != TOK_IDENTIFIER) {
		    call strcpy ("INDEF", Memc[FL_FUNITS(fl)+nfields*
		        (CQ_SZ_QPUNITS+1)], CQ_SZ_QPUNITS)
		}

		# Get the data formats. The default is %10s, %10d, and %10g
		# for character, integer, and floating data points respectively.
		ip = 1
		if (at_flnitem (Memc[fnfmts], ffp, Memc[FL_FFMTS(fl)+nfields*
		    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS) == EOF) {
		    switch (Memi[FL_FTYPES(fl)+nfields]) {
		    case TY_CHAR:
			call strcpy ("%10s", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    case TY_SHORT, TY_INT, TY_LONG:
			call strcpy ("%10d", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    case TY_REAL, TY_DOUBLE:
			call strcpy ("%10g", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }
		} else if (Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)] != '%') {
		    switch (Memi[FL_FTYPES(fl)+nfields*(CQ_SZ_QPFMTS+1)]) {
		    case TY_CHAR:
			call strcpy ("%10s", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    case TY_SHORT, TY_INT, TY_LONG:
			call strcpy ("%10d", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    case TY_REAL, TY_DOUBLE:
			call strcpy ("%10g", Memc[FL_FFMTS(fl)+nfields*
			    (CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		    }
		}

		# Correct for pixel coordinate formats here.
		if (streq (Memc[xpname], Memc[FL_FNAMES(fl)+
		    nfields*(CQ_SZ_QPNAME+1)]) || streq (Memc[xcname],
		    Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)])) {
		    call at_stats (at, FOXFORMAT, Memc[str], SZ_FNAME)
		    if (Memc[str] != EOS && strne (Memc[str],
		        Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			    nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		}
		if (streq (Memc[ypname], Memc[FL_FNAMES(fl)+
		    nfields*(CQ_SZ_QPNAME+1)]) || streq (Memc[ycname],
		    Memc[FL_FNAMES(fl)+nfields*(CQ_SZ_QPNAME+1)])) {
		    call at_stats (at, FOYFORMAT, Memc[str], SZ_FNAME)
		    if (Memc[str] != EOS && strne (Memc[str],
			Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)]))
			call strcpy (Memc[str], Memc[FL_FFMTS(fl)+
			    nfields*(CQ_SZ_QPFMTS+1)], CQ_SZ_QPFMTS)
		}

		# Get the field width.
		ip = 2
		if (ctoi (Memc[FL_FFMTS(fl)+nfields*(CQ_SZ_QPFMTS+1)], ip,
		    ival) <= 0)
		    ival = 10
		else if (ival <= 0 || IS_INDEFI(ival))
		    ival = 10

		# Get the field offset and field size. Note the extra
		# character added to the width ...
		switch (rtype) {
		case CQ_STEXT:
		    Memi[FL_FOFFSETS(fl)+nfields] = nfields + 1
		    Memi[FL_FSIZES(fl)+nfields] = 0
		case CQ_BTEXT:
		    Memi[FL_FOFFSETS(fl)+nfields] = foffset
		    Memi[FL_FSIZES(fl)+nfields] = ival + 1
		    foffset = foffset + Memi[FL_FSIZES(fl)+nfields]
		default:
		    call error (0, "Unknown output catalog type")
		}

		nfields = nfields + 1

	    }

	    rptr = rptr + 3
	}

	call sfree (sp)
end


# AT_GETOP -- Fetch an operand from the data structure.

procedure at_getop (res, operand, o)

pointer res                     #I pointer to the data structure
char    operand[ARB]            #I name of operand to be returned
pointer o                       #I pointer to output operand

pointer	sp, fvalue
int	fieldno, nchars
int	cq_fnumber(), cq_ftype(), cq_gvald(), cq_gvali(), cq_gvalc()
int	cq_rstati()

begin
	fieldno = cq_fnumber (res, operand)
	if (fieldno <= 0)
	    call error (0, "Illegal operand in expression")

	switch (cq_ftype (res, operand)) {

	case TY_CHAR:
	    call smark (sp)
	    call salloc (fvalue, SZ_LINE, TY_CHAR)
	    nchars = cq_gvalc (res, cq_rstati(res, CQRECPTR), operand,
	        Memc[fvalue], SZ_LINE) 
	    if (nchars <= 0) {
		call strcpy ("INDEF", Memc[fvalue], 5)
		nchars = 5
	    }
	    O_TYPE(o) = TY_CHAR
	    O_LEN(o) = nchars
	    O_FLAGS(o) = O_FREEVAL
	    call malloc (O_VALP(o), nchars, TY_CHAR)
	    call strcpy (Memc[fvalue], O_VALC(o), nchars)
	    call sfree (sp)

	case TY_SHORT, TY_INT, TY_LONG:
	    O_TYPE(o) = TY_INT
	    O_LEN(o) = 0
	    O_FLAGS(o) = 0
	    nchars = cq_gvali (res, cq_rstati (res, CQRECPTR), operand,
	        O_VALI(o)) 

	case TY_REAL, TY_DOUBLE:
	    O_TYPE(o) = TY_DOUBLE
	    O_LEN(o) = 0
	    O_FLAGS(o) = 0
	    nchars = cq_gvald (res, cq_rstati(res, CQRECPTR), operand,
	        O_VALD(o)) 

	default:
	    call smark (sp)
	    call salloc (fvalue, SZ_LINE, TY_CHAR)
	    nchars = cq_gvalc (res, cq_rstati(res, CQRECPTR), operand,
	        Memc[fvalue], SZ_LINE) 
	    if (nchars <= 0) {
		call strcpy ("INDEF", Memc[fvalue], 5)
		nchars = 5
	    }
	    O_TYPE(o) = TY_CHAR
	    O_LEN(o) = nchars
	    O_FLAGS(o) = O_FREEVAL
	    call malloc (O_VALP(o), nchars, TY_CHAR)
	    call strcpy (Memc[fvalue], O_VALC(o), nchars)
	    call sfree (sp)
	}
end
