include <pkg/cq.h>

# T_ACQITEST -- Test basic catalog database access and query procedures.

procedure t_acqitest ()

double	dval1
real	width
pointer	cq, sp, reclist, res
int	i, ip, catno, nqpars, parno, ftype, nfields
char	database[SZ_FNAME], record[SZ_LINE], ra[SZ_FNAME], dec[SZ_FNAME]
char	str[SZ_FNAME], catalog[SZ_LINE], imname[SZ_LINE]
char	qpname[CQ_SZ_QPNAME], qkname[CQ_SZ_QPNAME], qpvalue[CQ_SZ_QPVALUE]
char	qpunits[CQ_SZ_QPUNITS], qpformats[CQ_SZ_QPFMTS]

real	clgetr()
pointer	cq_map(), cq_imquery()
int	cq_stati(), cq_statt(), cq_setcat(), cq_setcatn(), cq_nqpars()
int	cq_gqpar(), cq_gqparn(), cq_sqpar(), ctod(), cq_istati()
int	cq_winfon(), cq_winfo(), cq_kinfon(), cq_kinfo()
bool	streq()

begin
	# Get the database and record names.
	call clgstr ("record", record, SZ_LINE)
	call clgstr ("image", imname, SZ_LINE)
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
	res = cq_imquery (cq, imname)
	if (res == NULL)
	    return

	call cq_istats (res, CQIMADDRESS, str, SZ_FNAME)
	call printf ("\nimaddress: %s\n")
	    call pargstr (str)
	call cq_istats (res, CQIMQUERY, str, SZ_FNAME)
	call printf ("imquery: %s\n")
	    call pargstr (str)
	call cq_istats (res, CQIQPNAMES, str, SZ_FNAME)
	call printf ("iqpnames:%s\n")
	    call pargstr (str)
	call cq_istats (res, CQIQPVALUES, str, SZ_FNAME)
	call printf ("iqpvalues:%s\n")
	    call pargstr (str)
	call flush (STDOUT)

	# Get the number of wcs parameters.
	call printf ("nheader = 0\n")
	nfields = cq_istati (res, CQNWCS)
	call printf ("nheader = %d\n")
	    call pargi (nfields)
	call flush (STDOUT)

	# Print the information for each field.
	do i = 1, nfields {
	    if (cq_winfon (res, i, qpname, CQ_SZ_QPNAME, qkname, CQ_SZ_QPNAME,
	        qpvalue, CQ_SZ_QPVALUE, ftype, qpunits, CQ_SZ_QPUNITS) <= 0)
		next
	    call printf ("keyword: %d %s %s %s %d %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (qkname)
		call pargstr (qpvalue)
		call pargi (ftype)
		call pargstr (qpunits)
	    if (cq_winfo (res, qpname, qkname, CQ_SZ_QPNAME, qpvalue,
	        CQ_SZ_QPVALUE, ftype, qpunits, CQ_SZ_QPUNITS) <= 0)
		next
	    call printf ("keyword: %d %s %s %s %d %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (qkname)
		call pargstr (qpvalue)
		call pargi (ftype)
		call pargstr (qpunits)
	}
	call printf ("\n")
	call flush (STDOUT)

	# Get the number of fields.
	nfields = cq_istati (res, CQNIMPARS)
	call printf ("nfields = %d\n")
	    call pargi (nfields)
	call flush (STDOUT)

	# Print the information for each field.
	do i = 1, nfields {
	    if (cq_kinfon (res, i, qpname, CQ_SZ_QPNAME, qkname, CQ_SZ_QPNAME,
	        qpvalue, CQ_SZ_QPVALUE, ftype, qpunits, CQ_SZ_QPUNITS) <= 0)
		next
	    call printf ("keyword: %d %s %s %s %d %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (qkname)
		call pargstr (qpvalue)
		call pargi (ftype)
		call pargstr (qpunits)
	    if (cq_kinfo (res, qpname, qkname, CQ_SZ_QPNAME, qpvalue,
	        CQ_SZ_QPVALUE, ftype, qpunits, CQ_SZ_QPUNITS) <= 0)
		next
	    call printf ("keyword: %d %s %s %s %d %s\n")
		call pargi (i)
		call pargstr (qpname)
		call pargstr (qkname)
		call pargstr (qpvalue)
		call pargi (ftype)
		call pargstr (qpunits)
	}
	call printf ("\n")
	call flush (STDOUT)

	# Close the query descriptor.
	call cq_imclose (res)

	# Unmap the database.
	call cq_unmap (cq)
end
