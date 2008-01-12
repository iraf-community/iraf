include <pkg/cq.h>

# AT_WNOFILRECS -- Write out the catalog header and records without filtering.

procedure at_wnofilrecs (fd, res, standard)

int	fd			#I the output file descriptor
pointer	res			#I the results descriptor
bool	standard		#I write a standard catalog header.

int	nlines, nrecs
int	at_wcathdr(), at_wcatrecs()

begin
	# Write out the catalog header.
	if (standard)
	    nlines = at_wcathdr (fd, res)

	# Write out the records.
	nrecs = at_wcatrecs (fd, res)
end


# AT_WCATHDR -- Write out a catalog header.

int procedure at_wcathdr (fd, res)

int	fd			#I the output file descriptor
pointer	res			#I the results descriptor

pointer	sp, catname, qpnames, qpvalues, qpunits, fname, fvalue, funits, ffmts
int	i, nlines, nfields, fsize, foffset, ftype
int	at_wrdstr(), cq_rstati(), cq_hinfon(), cq_finfon()
char	cq_itype()

begin
	nlines = 0

	# Allocate working space.
	call smark (sp)
	call salloc (catname, SZ_FNAME, TY_CHAR)
	call salloc (fname, max (CQ_SZ_QPNAME, CQ_SZ_FNAME), TY_CHAR)
	call salloc (fvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (funits, max (CQ_SZ_QPUNITS, CQ_SZ_FUNITS), TY_CHAR)
	call salloc (ffmts, CQ_SZ_FFMTS, TY_CHAR)
	call salloc (qpnames, SZ_LINE, TY_CHAR)
	call salloc (qpvalues, SZ_LINE, TY_CHAR)
	call salloc (qpunits, SZ_LINE, TY_CHAR)

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
	    call fprintf (fd, "#     %s %s\n")
	        call pargstr (Memc[fname])
	        call pargstr (Memc[fvalue])
	    nlines = nlines + 1
	}

	# Write out the field parameters.
	nfields = cq_rstati (res, CQNFIELDS)
	call fprintf (fd, "# nfields %d\n")
	    call pargi (nfields)
	do i = 1, nfields {
	    if (cq_finfon (res, i, Memc[fname], CQ_SZ_FNAME, foffset, fsize,
	        ftype, Memc[funits], CQ_SZ_FUNITS, Memc[ffmts],
		CQ_SZ_FFMTS) != i)
		next
	    call fprintf (fd, "#     %s %d %d %c %s %s\n")
	        call pargstr (Memc[fname])
		call pargi (foffset)
		call pargi (fsize)
		call pargc (cq_itype (ftype))
		call pargstr (Memc[funits])
		call pargstr (Memc[ffmts])
	    nlines = nlines + 1
	}

	# Write the header trailer.
	call fprintf (fd, "# END CATALOG HEADER\n#\n")
	nlines = nlines + 1

	call sfree (sp)

	return (nlines)
end


# AT_WCATRECS -- Write out the catalog records without modification, except
# for the builtin trim parameters.

int procedure at_wcatrecs (fd, res)

int	fd			#I the output file descriptor
pointer	res			#I the results descriptor

pointer	sp, record
int	sz_rec, nrec, recptr, nchars
int	cq_rstati(), cq_gnrecord()

begin
	# Allocate space for the record. For now SZ_LINE is the default.
	if (cq_rstati(res, CQRECSIZE) > 0)
	    sz_rec = max (SZ_LINE, cq_rstati (res, CQRECSIZE))
	else
	    sz_rec = SZ_LINE
	nrec = cq_rstati (res, CQRNRECS)

	# Allocate working space.
	call smark (sp)
	call salloc (record, sz_rec, TY_CHAR)

	# For the moment assume that the simple and blocked text file records
	# are newline delimited, and that the simple text file fields are
	# whitespace delimited.

	# Write the records.
	switch (cq_rstati (res, CQRTYPE)) {

	case CQ_STEXT:
	    recptr = 0
	    while (recptr < nrec) {
	        nchars = cq_gnrecord (res, Memc[record], sz_rec, recptr)
	        if (nchars == EOF)
		    break
	        call fprintf (fd, "%s")
		    call pargstr (Memc[record])
	    }

	case CQ_BTEXT:
	    recptr = 0
	    while (recptr < nrec) {
	        nchars = cq_gnrecord (res, Memc[record], sz_rec, recptr)
	        if (nchars == EOF)
		    break
	        call fprintf (fd, "%s")
		    call pargstr (Memc[record])
	    }

	default:
	    ;
	}

	call sfree (sp)

	return (recptr)
end
