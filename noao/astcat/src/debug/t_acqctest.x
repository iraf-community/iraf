include <pkg/cq.h>

# T_ACQCTEST -- Test basic catalog database access and query procedures.

procedure t_acqctest ()

double	dval1, dval2
real	width, rval1, rval2
long	lval1, lval2
pointer	cq, sp, reclist, res
int	i, ip, catno, nqpars, parno, nres, recptr, nchars, foffset, fsize
int	ftype, nfields, ival1, ival2
short	sval1, sval2
char	database[SZ_FNAME], record[SZ_LINE], ra[SZ_FNAME], dec[SZ_FNAME]
char	str[SZ_FNAME], catalog[SZ_LINE]
char	qpname[CQ_SZ_QPNAME], qpvalue[CQ_SZ_QPVALUE], qpunits[CQ_SZ_QPUNITS]
char	qpformats[CQ_SZ_QPFMTS]

real	clgetr()
pointer	cq_map(), cq_query()
int	cq_stati(), cq_statt(), cq_setcat(), cq_setcatn(), cq_nqpars()
int	cq_gqpar(), cq_gqparn(), cq_sqpar(), ctod(), cq_rstati()
int	cq_gnrecord(), cq_grecord(), cq_finfon(), cq_finfo(), cq_fname()
int	cq_foffset(), cq_fsize(), cq_ftype(), cq_gvali(), cq_hinfo()
int	cq_gvalc(), cq_gvald(), cq_gvalr(), cq_gvall(), cq_gvals(), cq_hinfon()
bool	streq()

begin
	# Get the database and record names.
	call clgstr ("record", record, SZ_LINE)
	call clgstr ("ra", ra, SZ_FNAME)
	call clgstr ("dec", dec, SZ_FNAME)
	width = clgetr ("width")
	call clgstr ("database", database, SZ_FNAME)

	# Map the database.
	cq = cq_map (database, READ_ONLY)

	# Print the database file name and number of records.
	call cq_stats (cq, CQCATDB, database, SZ_FNAME)
	call printf ("\nDatabase: %s  Nrecs: %d\n\n")
	    call pargstr (database)
	    call pargi (cq_stati (cq, CQNRECS))

	# Print the record list.
	call printf ("Szreclist = %d characters\n")
	    call pargi (cq_stati (cq, CQSZRECLIST))

	call smark (sp)
	call salloc (reclist, cq_stati(cq, CQSZRECLIST), TY_CHAR)
	if (cq_statt (cq, CQRECLIST, Memc[reclist], cq_stati(cq,
	    CQSZRECLIST)) <= 0)
	    Memc[reclist] = EOS
	call printf ("%s")
	    call pargstr (Memc[reclist])
	call sfree (sp)

	# Print the current catalog name and number.
	call cq_stats (cq, CQCATNAME, catalog, SZ_LINE)
	call printf ("\nCurrent catalog: %s  index: %d\n")
	    call pargstr (catalog)
	    call pargi (cq_stati (cq, CQCATNO))

	# Set the current catalog by name.
	catno = cq_setcat (cq, record)
	call cq_stats (cq, CQCATNAME, catalog, SZ_LINE)
	call printf ("\nCurrent catalog: %s  index: %d\n")
	    call pargstr (catalog)
	    call pargi (cq_stati (cq, CQCATNO))

	# Set the same catalog by number.
	catno = cq_setcatn (cq, catno)
	call cq_stats (cq, CQCATNAME, catalog, SZ_LINE)
	call printf ("\nCurrent catalog: %s  index: %d\n\n")
	    call pargstr (catalog)
	    call pargi (cq_stati (cq, CQCATNO))

	# Set the query parameters. Don't worry about units in this case.
	nqpars = cq_nqpars (cq)
	do i = 1, nqpars {

	    # Get description of each query parameter.
	    parno = cq_gqparn (cq, i, qpname, CQ_SZ_QPNAME, qpvalue,
	        CQ_SZ_QPVALUE, qpunits, CQ_SZ_QPUNITS, qpformats, CQ_SZ_QPFMTS) 
	    call printf ("parno: %d %s %s %s %s\n")
		call pargi (parno)
		call pargstr (qpname)
		call pargstr (qpvalue)
		call pargstr (qpunits)
		call pargstr (qpformats)
	    parno = cq_gqpar (cq, qpname, qpname, CQ_SZ_QPNAME, qpvalue,
	        CQ_SZ_QPVALUE, qpunits, CQ_SZ_QPUNITS, qpformats, CQ_SZ_QPFMTS) 
	    call printf ("parno: %d %s %s %s %s\n")
		call pargi (parno)
		call pargstr (qpname)
		call pargstr (qpvalue)
		call pargstr (qpunits)
		call pargstr (qpformats)


	    # Set the astrometric parameters.
	    if (streq (qpname, "ra")) {
		ip = 1
		if (ctod (ra, ip, dval1) > 0) {
		    call sprintf (ra, SZ_FNAME, qpformats)
			call pargd (dval1)
		}
		parno = cq_sqpar (cq, qpname, ra)
	    } else if (streq (qpname, "dec")) {
		ip = 1
		if (ctod (dec, ip, dval1) > 0) {
		    if (dval1 >= 0.0) {
			#dec[1] = '+'
		        #call sprintf (dec[2], SZ_FNAME - 1, qpformats)
		        call sprintf (dec, SZ_FNAME, qpformats)
		    } else {
		        call sprintf (dec, SZ_FNAME, qpformats)
		    }
		        call pargd (dval1)
		}
		parno = cq_sqpar (cq, qpname, dec)
	    } else if (streq (qpname, "width")) {
		call sprintf (str, SZ_FNAME, qpformats)
		    call pargr (width)
		parno = cq_sqpar (cq, qpname, str)
	    } else if (streq (qpname, "radius")) {
		call sprintf (str, SZ_FNAME, qpformats)
		    call pargr (width / 2.0)
		parno = cq_sqpar (cq, qpname, str)
	    }

	}
	call flush (STDOUT)

	# Send the query and get back the results.
	res = cq_query (cq)
	if (res == NULL)
	    return

	call cq_rstats (res, CQRADDRESS, str, SZ_FNAME)
	call printf ("\nraddress: %s\n")
	    call pargstr (str)
	call cq_rstats (res, CQRQUERY, str, SZ_FNAME)
	call printf ("rquery: %s\n")
	    call pargstr (str)
	call cq_rstats (res, CQRQPNAMES, str, SZ_FNAME)
	call printf ("rqpnames:%s\n")
	    call pargstr (str)
	call cq_rstats (res, CQRQPVALUES, str, SZ_FNAME)
	call printf ("rqpvalues:%s\n")
	    call pargstr (str)

	# Get the number of header parameters.
	nfields = cq_rstati (res, CQNHEADER)
	call printf ("nheader = %d\n")
	    call pargi (nfields)

	# Print the information for each field.
	do i = 1, nfields {
	    if (cq_hinfon (res, i, qpname, CQ_SZ_QPNAME, record, SZ_LINE) <= 0)
		next
	    call printf ("keyword: %d %s %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (record)
	    if (cq_hinfo (res, qpname, record, SZ_LINE) <= 0)
		next
	    call printf ("keyword: %d %s %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (record)
	}
	call printf ("\n")

	# Get the number of fields.
	nfields = cq_rstati (res, CQNFIELDS)
	call printf ("nfields = %d\n")
	    call pargi (nfields)

	# Print the information for each field.
	do i = 1, nfields {
	    if (cq_finfon (res, i, qpname, CQ_SZ_FNAME, foffset, fsize,
	        ftype, qpunits, CQ_SZ_FUNITS, qpformats, CQ_SZ_FFMTS) <= 0)
		next
	    call printf ("field: %d %s %d %d %d %s %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargi (foffset)
		call pargi (fsize)
		call pargi (ftype)
		call pargstr (qpunits)
		call pargstr (qpformats)
	    if (cq_finfo (res, qpname, foffset, fsize, ftype, qpunits,
	        CQ_SZ_FUNITS, qpformats, CQ_SZ_FFMTS) <= 0)
		next
	    call printf ("field: %d %s %d %d %d %s %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargi (foffset)
		call pargi (fsize)
		call pargi (ftype)
		call pargstr (qpunits)
		call pargstr (qpformats)
	    if (cq_fname (res, i, qpname, CQ_SZ_FNAME) <= 0)
		next
	    foffset = cq_foffset (res, qpname)
	    fsize = cq_fsize (res, qpname)
	    ftype = cq_ftype (res, qpname)
	    call cq_funits (res, qpname, qpunits, CQ_SZ_FUNITS)
	    call cq_ffmts (res, qpname, qpformats, CQ_SZ_FFMTS)
	    call printf ("field: %d %s %d %d %d %s %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargi (foffset)
		call pargi (fsize)
		call pargi (ftype)
		call pargstr (qpunits)
		call pargstr (qpformats)
	}
	call printf ("\n")

	# Get the number of records.
	nres = cq_rstati (res, CQRNRECS)
	call printf ("nrecords = %d\n")
	    call pargi (nres)

	# Loop through and print the records.
	recptr = 0
	while (recptr < nres) {
	    nchars = cq_gnrecord (res, record, SZ_LINE, recptr)
	    if (nchars == EOF)
		break
	    call printf ("record %4d %4d %s")
		call pargi (recptr)
		call pargi (nchars)
		call pargstr (record)
	}

	# Find and print records at random.
	record[1] = EOS
	nchars = cq_grecord (res, record, SZ_LINE, 1)
	call printf ("\nrecord %4d %4d %s")
	    call pargi (1)
	    call pargi (nchars)
	    call pargstr (record)

	record[1] = EOS
	nchars = cq_grecord (res, record, SZ_LINE, (1 + nres) / 2)
	call printf ("record %4d %4d %s")
	    call pargi ((1 + nres) / 2)
	    call pargi (nchars)
	    call pargstr (record)
	
	record[1] = EOS
	nchars = cq_grecord (res, record, SZ_LINE, nres)
	call printf ("record %4d %4d %s")
	    call pargi (nres)
	    call pargi (nchars)
	    call pargstr (record)

	# Loop through the records and decode the ra and dec fields as
	# char, double precision, real precision, and integer fields.
	call printf ("\nra dec\n")
	do i = 1, nres {
	    call printf ("rec %d\n")
		call pargi (i)
	    nchars = cq_gvalc (res, i, "ra", ra, SZ_FNAME)
	    nchars = cq_gvalc (res, i, "dec", dec, SZ_FNAME)
	    call printf ("    %s %s\n")
		call pargstr (ra)
		call pargstr (dec)
	    nchars = cq_gvald (res, i, "ra", dval1)
	    nchars = cq_gvald (res, i, "dec", dval2)
	    call printf ("    %h %h\n")
		call pargd (dval1)
		call pargd (dval2)
	    nchars = cq_gvalr (res, i, "ra", rval1)
	    nchars = cq_gvalr (res, i, "dec", rval2)
	    call printf ("    %h %h\n")
		call pargr (rval1)
		call pargr (rval2)
	    nchars = cq_gvall (res, i, "ra", lval1)
	    nchars = cq_gvall (res, i, "dec", lval2)
	    call printf ("    %h %h\n")
		call pargl (lval1)
		call pargl (lval2)
	    nchars = cq_gvali (res, i, "ra", ival1)
	    nchars = cq_gvali (res, i, "dec", ival2)
	    call printf ("    %h %h\n")
		call pargi (ival1)
		call pargi (ival2)
	    nchars = cq_gvals (res, i, "ra", sval1)
	    nchars = cq_gvals (res, i, "dec", sval2)
	    call printf ("    %h %h\n")
		call pargs (sval1)
		call pargs (sval2)
	}

	# Close the query descriptor.
	call cq_rclose (res)

	# Unmap the database.
	call cq_unmap (cq)
end
