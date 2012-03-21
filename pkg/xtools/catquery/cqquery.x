include <fset.h>
include <ctype.h>
include "cqdef.h"
include "cq.h"


define	DEF_SZ_INBUF	32768	# the maximum network transfer buffer size
define	DEF_SZ_INDEX	1000    # the record index length increment

# CQ_QUERY -- Send a query and return the data.

pointer	procedure cq_query (cq)

pointer	cq			#I the catalog database descriptor

pointer	cc, res, inbuf, line, sp, spfname
char    url[SZ_PATHNAME], addr[SZ_LINE], query[SZ_LINE], buf[SZ_LINE]
int	j, fd, nchars, nlines, nrecs, szindex, ip, op
bool	done
long	note()
pointer	cq_rinit()
int	ndopen(), strlen(), read(), open(), getline(), fstati(), url_get()
errchk	ndopen(), fprintf(), areadb(), awriteb(), open(), read()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)


        if (0<1&& USE_URLGET) {
            # Initialize the image results structure.
	    res = cq_rinit (cq)

            call strcpy (CQ_ADDRESS(cc), buf, SZ_LINE)
            for (ip=1; buf[ip] != ':'; ip=ip+1) ;       # skip 'inet:'
            ip = ip + 1
            for (    ; buf[ip] != ':'; ip=ip+1) ;       # skip '80:'
            ip = ip + 1
            for (op=1; buf[ip] != ':'; ip=ip+1) {
                addr[op] = buf[ip]
                op = op + 1
            }
            addr[op] = EOS

            call strcpy (CQ_RQUERY(res), buf, SZ_LINE)
            for (op=1; !IS_WHITE(buf[op+4]); op=op+1)
                query[op] = buf[op+4]
            query[op] = EOS

            call sprintf (url, SZ_LINE, "http://%s%s")
                call pargstr (addr)
                call pargstr (query)

            iferr {
	        call smark (sp)
	        call salloc (spfname, SZ_FNAME, TY_CHAR)

                call malloc (inbuf, DEF_SZ_INBUF, TY_CHAR)

	        # Open the output spool file.
	        call mktemp ("query", Memc[spfname], SZ_FNAME)

                if (url_get (url, Memc[spfname], inbuf) < 0)
                    call error (0, "Cannot access url")

	        fd = open (Memc[spfname], READ_ONLY, TEXT_FILE)
	        CQ_RFD(res) = open (Memc[spfname], READ_WRITE, SPOOL_FILE)
	        repeat {
	 	    call aclrc (Memc[inbuf], DEF_SZ_INBUF)
	            nchars = read (fd, Memc[inbuf], DEF_SZ_INBUF)
	            if (nchars > 0) {
		        Memc[inbuf+nchars] = EOS
	                call write (CQ_RFD(res), Memc[inbuf], nchars)
		        done = false
	            } else
	                done = true
	        } until (done)
	        call flush (CQ_RFD(res))
		call close (fd)

	        CQ_RBUF(res) = fstati (CQ_RFD(res), F_BUFPTR)
	        call seek (CQ_RFD(res), BOF)

                call mfree (inbuf, TY_CHAR)
		call sfree (sp)

            } then {
                if (res != NULL)
	    	    call cq_rfree (res)
                return (NULL)
            }

        } else {

	    # Open the network connection.
	    iferr (fd = ndopen (CQ_ADDRESS(cc), READ_WRITE))
	        return (NULL)

	    # Initialize the results structure.
	    res = cq_rinit (cq)

	    # Send the query and get back the results.
	    iferr {

	        call smark (sp)

	        # Formulate the query.
	        switch (CQ_RTYPE(res)) {
	        case CQ_STEXT, CQ_BTEXT:
	            call fprintf (fd, "%s")
	                call pargstr (CQ_RQUERY(res))
	        default:
	            nchars = strlen (CQ_RQUERY(res))
		    call write (fd, CQ_RQUERY(res), nchars)
	        }
	        call flush (fd)

	        # Open the output spool file.
	        call salloc (spfname, SZ_FNAME, TY_CHAR)
	        call mktemp ("query", Memc[spfname], SZ_FNAME)
	        CQ_RFD(res) = open (Memc[spfname], READ_WRITE, SPOOL_FILE)
	        call sfree (sp)

	        # Get the data.
	        call malloc (inbuf, DEF_SZ_INBUF, TY_CHAR)
	        call fseti (fd, F_CANCEL, OK)

	        switch (CQ_HFMT(cc)) {
	        case CQ_HNONE:
		    ;
	        case CQ_HHTTP:
	            repeat {
	                nchars = getline (fd, Memc[inbuf])
	                if (nchars <= 0)
		            break
		        Memc[inbuf+nchars] = EOS
	            } until ((Memc[inbuf] == '\r' && Memc[inbuf+1] == '\n') ||
	                (Memc[inbuf] == '\n'))
	        default:
		    ;
	        }

	        repeat {
	            nchars = read (fd, Memc[inbuf], DEF_SZ_INBUF)
	            if (nchars > 0) {
		        Memc[inbuf+nchars] = EOS
	                call write (CQ_RFD(res), Memc[inbuf], nchars)
		        done = false
	            } else {
	                done = true
	            }
	        } until (done)

	        # Cleanup.
	        call flush (CQ_RFD(res))
	        call mfree (inbuf, TY_CHAR)
	        CQ_RBUF(res) = fstati (CQ_RFD(res), F_BUFPTR)
	        call seek (CQ_RFD(res), BOF)
	        call close (fd)

	    } then {
	        call cq_rfree (res)
	        call close (fd)
	        return (NULL)
	    }

	}

	# Construct the record index.
	CQ_RNRECS(res) = 0
	switch (CQ_RTYPE(res)) {
	case CQ_STEXT, CQ_BTEXT:

	    # Initialize.
	    nlines = 0
	    nrecs = 0

	    # Iniitialize the index array.
	    szindex = DEF_SZ_INDEX
	    call malloc (line, SZ_LINE, TY_CHAR)
	    call calloc (CQ_RINDEX(res), szindex, TY_LONG)

	    # Create the index array.
	    repeat {
		Meml[CQ_RINDEX(res)+nrecs] = note (CQ_RFD(res)) 
		nchars = getline (CQ_RFD(res), Memc[line])
		if (nchars == EOF)
		    break
		nlines = nlines + 1
		if (nlines <= CQ_RHSKIP(res))
		    next
		if (Memc[line] == '\n')
		    next
		#if (CQ_RECSIZE(res) > 0 && nchars != CQ_RECSIZE(res))
		if (CQ_RECSIZE(res) > 0 && nchars > CQ_RECSIZE(res))
		    Meml[CQ_RINDEX(res)+nrecs] = EOF
	        else if (CQ_RTRIML(res) > 0 || CQ_RTRIMR(res) > 0) {
		    inbuf = CQ_RBUF(res) + Meml[CQ_RINDEX(res)+nrecs] - 1 
		    do j = 1, min (CQ_RTRIML(res), nchars)
			Memc[inbuf+j-1] = ' '
		    do j = nchars - CQ_RTRIMR(res), nchars - 1
			Memc[inbuf+j-1] = ' '
		}
		nrecs = nrecs + 1
		if (nrecs >= szindex) {
		    szindex = szindex + DEF_SZ_INDEX
	    	    call realloc (CQ_RINDEX(res), szindex, TY_LONG)
		    call aclrl (Meml[CQ_RINDEX(res)+szindex-DEF_SZ_INDEX],
		        DEF_SZ_INDEX)
		}
	    }
	    call mfree (line, TY_CHAR)
	    CQ_RNRECS(res) = nrecs

	    # Remove the incorrectly sized and trailing records.
	    nrecs = 0
	    do j = 0, CQ_RNRECS(res) - CQ_RTSKIP(res) - 1 {
		if (Meml[CQ_RINDEX(res)+j] == EOF)
		    next
		Meml[CQ_RINDEX(res)+nrecs] = Meml[CQ_RINDEX(res)+j]
		nrecs = nrecs + 1
	    }
	    CQ_RNRECS(res) = nrecs

	    # Resize the index array.
	    call realloc (CQ_RINDEX(res), max (1, CQ_RNRECS(res) + 1), TY_LONG)

	default:
	    ;
	}

	# Return the results pointer.
	return (res)
end


# CQ_FQUERY -- Treat a catalog file file as thought it were the results
# of a query. The catalog file file name and file description are passed
# to the routine as arguments.

pointer	procedure cq_fquery (cq, catfile, catfmt)

pointer	cq			#I the catalog database descriptor
char	catfile[ARB]		#I the input catalog file
char	catfmt[ARB]		#I the input catalog description

pointer	res, inbuf, line, sp, spfname
int	j, fd, nchars, nlines, nrecs, szindex
bool	done
pointer	cq_frinit()
long	note()
int	access(), open(), read(), fstati(), getline()
errchk	open(), read()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)

	# Check to see if the catalog file exists.
	if (access (catfile, 0, 0) == NO)
	    return (NULL)

	# Check to see if the fmt string is defined.
	if (catfmt[1] == EOS)
	    return (NULL)

	# Open the catalog file.
	if (access (catfile, READ_ONLY, TEXT_FILE) == YES) {
	    iferr (fd = open (catfile, READ_ONLY, TEXT_FILE))
		return (NULL)
	} else {
	    iferr (fd = open (catfile, READ_ONLY, BINARY_FILE))
		return (NULL)
	}

	# Initialize the results structure using the file description.
	res = cq_frinit (cq, catfmt)
	if (res == NULL)
	    return (NULL)

	# Read in the results.
	iferr {

	    # Open the output spool file.
	    call smark (sp)
	    call salloc (spfname, SZ_FNAME, TY_CHAR)
	    call mktemp ("query", Memc[spfname], SZ_FNAME)
	    #CQ_RFD(res) = open ("dev$null", READ_WRITE, SPOOL_FILE)
	    CQ_RFD(res) = open (Memc[spfname], READ_WRITE, SPOOL_FILE)
	    call sfree (sp)

	    # Get the data.
	    call malloc (inbuf, DEF_SZ_INBUF, TY_CHAR)
	    repeat {
	        nchars = read (fd, Memc[inbuf], DEF_SZ_INBUF)
	        if (nchars > 0) {
		    Memc[inbuf+nchars] = EOS
	            call write (CQ_RFD(res), Memc[inbuf], nchars)
		    done = false
	        } else {
		    done = true
	        }
	    } until (done)

	    # Cleanup.
	    call flush (CQ_RFD(res))
	    call mfree (inbuf, TY_CHAR)
	    CQ_RBUF(res) = fstati (CQ_RFD(res), F_BUFPTR)
	    call close (fd)

	} then {
	    call cq_rfree (res)
	    call close (fd)
	    return (NULL)
	}

	# Construct the record index.
	CQ_RNRECS(res) = 0
	switch (CQ_RTYPE(res)) {
	case CQ_STEXT, CQ_BTEXT:

	    # Initialize.
	    nlines = 0
	    nrecs = 0

	    # Iniitialize the index array.
	    szindex = DEF_SZ_INDEX
	    call malloc (line, SZ_LINE, TY_CHAR)
	    call calloc (CQ_RINDEX(res), szindex, TY_LONG)

	    # Create the index array.
	    call seek (CQ_RFD(res), BOF)
	    repeat {
		Meml[CQ_RINDEX(res)+nrecs] = note (CQ_RFD(res)) 
		nchars = getline (CQ_RFD(res), Memc[line])
		if (nchars == EOF)
		    break
		nlines = nlines + 1
		if (nlines <= CQ_RHSKIP(res))
		    next
		if (Memc[line] == '\n')
		    next
		if (Memc[line] == '#')
		    next
		#if (CQ_RECSIZE(res) > 0 && nchars != CQ_RECSIZE(res))
		if (CQ_RECSIZE(res) > 0 && nchars > CQ_RECSIZE(res))
		    Meml[CQ_RINDEX(res)+nrecs] = EOF
	        else if (CQ_RTRIML(res) > 0 || CQ_RTRIMR(res) > 0) {
		    inbuf = CQ_RBUF(res) + Meml[CQ_RINDEX(res)+nrecs] - 1 
		    do j = 1, min (CQ_RTRIML(res), nchars)
			Memc[inbuf+j-1] = ' '
		    do j = nchars - CQ_RTRIMR(res), nchars - 1
			Memc[inbuf+j-1] = ' '
		}
		nrecs = nrecs + 1
		if (nrecs >= szindex) {
		    szindex = szindex + DEF_SZ_INDEX
	    	    call realloc (CQ_RINDEX(res), szindex, TY_LONG)
		    call aclrl (Meml[CQ_RINDEX(res)+szindex-DEF_SZ_INDEX],
		        DEF_SZ_INDEX)
		}
	    }
	    call mfree (line, TY_CHAR)
	    CQ_RNRECS(res) = nrecs

	    # Check for and reject short records and trim trailing records.
	    nrecs = 0
	    do j = 0, CQ_RNRECS(res) - CQ_RTSKIP(res)  - 1 {
		if (Meml[CQ_RINDEX(res)+j] == EOF)
		    next
		Meml[CQ_RINDEX(res)+nrecs] = Meml[CQ_RINDEX(res)+j]
		nrecs = nrecs + 1
	    }
	    CQ_RNRECS(res) = nrecs

	    # Trim the trailing records.
	    call realloc (CQ_RINDEX(res), max (1, CQ_RNRECS(res) + 1), TY_LONG)

	default:
	    ;
	}

	return (res)
end


# CQ_RCLOSE -- Close the results structure,

procedure cq_rclose (res)

pointer	res			#U the results descriptor.

begin
	call cq_rfree (res)
end


# CQ_RINIT -- Initialize a results descriptor.

pointer procedure cq_rinit (cq)

pointer	cq			#I the catalog descriptor

pointer	cc, res, sp, query, value, kname, fname, funits, ffmt
int	i, ncount, sz1, sz2, sz3, op1, op2, op3, foffset, fsize
char	ftype
int	cq_wrdstr(), strdic(), cq_dgeti(), cq_dscan(), nscan()
int	cq_dtype(), strlen(), gstrcpy()
errchk	cq_dgwrd(), cq_dgeti(), cq_dscan()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)

	# Allocate the results structure.
	call calloc (res, CQ_LEN_RES, TY_STRUCT)

	# Format the query.
	call smark (sp)
	call salloc (query, SZ_LINE, TY_CHAR)
	call salloc (value, CQ_SZ_QPVALUE, TY_CHAR)
	call sprintf (Memc[query], SZ_LINE, CQ_QUERY(cc))
	do i = 1, CQ_NQPARS(cc) {
	    if (cq_wrdstr (i, Memc[value], CQ_SZ_QPVALUE,
	        Memc[CQ_PQPVALUES(cc)]) <= 0)
		next
	    call pargstr (Memc[value])
	}

	# Save the catalog informaton and query in the results structure.
	call strcpy (CQ_CATDB(cq), CQ_RCATDB(res), SZ_FNAME)
	call strcpy (CQ_CATNAME(cq), CQ_RCATNAME(res), SZ_FNAME)
	call strcpy (CQ_ADDRESS(cc), CQ_RADDRESS(res), SZ_LINE)
	call strcpy (Memc[query], CQ_RQUERY(res), SZ_LINE)

	# Copy the query parameters to the results descriptor.
	CQ_RNQPARS(res) = CQ_NQPARS(cc)
	fsize = strlen (Memc[CQ_PQPNAMES(cc)])
	call malloc (CQ_RQPNAMES(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPNAMES(cc)], Memc[CQ_RQPNAMES(res)], fsize)
	fsize = strlen (Memc[CQ_PQPVALUES(cc)])
	call malloc (CQ_RQPVALUES(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPVALUES(cc)], Memc[CQ_RQPVALUES(res)], fsize)
	fsize = strlen (Memc[CQ_PQPUNITS(cc)])
	call malloc (CQ_RQPUNITS(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPUNITS(cc)], Memc[CQ_RQPUNITS(res)], fsize)

	# Get the input data type.
	iferr {
	    call cq_dgwrd (cq, CQ_CATNO(cq), "type", Memc[value],
	        CQ_SZ_QPVALUE)
	} then {
	    Memc[value] = EOS
	   CQ_RTYPE(res) = CQ_STEXT
	} else {
	    CQ_RTYPE(res) = strdic (Memc[value], Memc[value], CQ_SZ_QPVALUE,
	        CQ_RTYPESTR)
	}

	# Get the number of leading and trailing records to be skipped.
	iferr (CQ_RHSKIP(res) = cq_dgeti (cq, CQ_CATNO(cq), "hskip"))
	    CQ_RHSKIP(res) = 0
	iferr (CQ_RTSKIP(res) = cq_dgeti (cq, CQ_CATNO(cq), "tskip"))
	    CQ_RTSKIP(res) = 0

	# Get the record size and trimming parameters.
	iferr (CQ_RECSIZE(res) = cq_dgeti (cq, CQ_CATNO(cq), "recsize"))
	    CQ_RECSIZE(res) = 0
	iferr (CQ_RTRIML(res) = cq_dgeti (cq, CQ_CATNO(cq), "triml"))
	    CQ_RTRIML(res) = 0
	iferr (CQ_RTRIMR(res) = cq_dgeti (cq, CQ_CATNO(cq), "trimr"))
	    CQ_RTRIMR(res) = 0

	iferr (CQ_NHEADER(res) = cq_dgeti (cq, CQ_CATNO(cq), "nheader"))
	    CQ_NHEADER(res) = 0

	# Get the header parameters.
	call calloc (CQ_HKNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_HKVALUES(res), SZ_LINE, TY_CHAR)
	ncount = 0
	if (CQ_NHEADER(res) > 0) {

	    # Initialize the header parameter keywords and values.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    call strcpy ("|", Memc[CQ_HKNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_HKVALUES(res)], sz2)

	    call salloc (kname, CQ_SZ_FNAME, TY_CHAR)
	    do i = 1, CQ_NHEADER(res) {

		# Get the keyword and value.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[kname], CQ_SZ_QPNAME)
		call gargwrd (Memc[query], SZ_LINE)
		if (nscan() != 2)
		    break

		# Add the keyword name to the list.
		if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz1 = sz1 + SZ_LINE
		    call realloc (CQ_HKNAMES(res), sz1, TY_CHAR)
		}
		op1 = op1 + gstrcpy (Memc[kname], Memc[CQ_HKNAMES(res)+op1-1],
		    sz1 - op1 + 1)
		op1 = op1 + gstrcpy ("|", Memc[CQ_HKNAMES(res)+op1-1],
		    sz1 - op1 + 1)

		# Add the keyword value to the list.
		if ((sz2 - op2 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz2 = sz2 + SZ_LINE
		    call realloc (CQ_HKVALUES(res), sz2, TY_CHAR)
		}
		op2 = op2 + gstrcpy (Memc[query], Memc[CQ_HKVALUES(res)+op2-1],
		    sz2 - op2 + 1)
		op2 = op2 + gstrcpy ("|", Memc[CQ_HKVALUES(res)+op2-1],
		    sz2 - op2 + 1)

		ncount = ncount + 1
	    }
	}

	# Resize the header keyword  and value arrays.
	if (ncount != CQ_NHEADER(res)) {
	    CQ_NHEADER(res) = 0
	    call realloc (CQ_HKNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_HKVALUES(res), 1, TY_CHAR)
	    Memc[CQ_HKNAMES(res)] = EOS
	    Memc[CQ_HKVALUES(res)] = EOS
	} else {
	    call realloc (CQ_HKNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_HKVALUES(res), op2, TY_CHAR)
	    Memc[CQ_HKNAMES(res)+op1] = EOS
	    Memc[CQ_HKVALUES(res)+op2] = EOS
	}

	iferr (CQ_NFIELDS(res) = cq_dgeti (cq, CQ_CATNO(cq), "nfields"))
	    CQ_NFIELDS(res) = 0

	# Allocate the field description arrays.
	call calloc (CQ_FNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_FOFFSETS(res), CQ_NFIELDS(res), TY_INT)
	call calloc (CQ_FSIZES(res), CQ_NFIELDS(res), TY_INT)
	call calloc (CQ_FTYPES(res), CQ_NFIELDS(res), TY_INT)
	call calloc (CQ_FUNITS(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_FFMTS(res), SZ_LINE, TY_CHAR)

	# Get the field decoding parameters.
	ncount = 0
	if (CQ_NFIELDS(res) > 0) {

	    call salloc (fname, CQ_SZ_FNAME, TY_CHAR)
	    call salloc (funits, CQ_SZ_FUNITS, TY_CHAR)
	    call salloc (ffmt, CQ_SZ_FFMTS, TY_CHAR)

	    # Initialize the name, units, and format string dictionaries.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    sz3 = SZ_LINE; op3 = 2
	    call strcpy ("|", Memc[CQ_FNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_FUNITS(res)], sz2)
	    call strcpy ("|", Memc[CQ_FFMTS(res)], sz3)

	    do i =1, CQ_NFIELDS(res) {

		# Get the field description.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[fname], CQ_SZ_FNAME)
		call gargi (foffset)
		call gargi (fsize)
		call gargc (ftype)
		call gargwrd (Memc[funits], CQ_SZ_FUNITS)
		call gargwrd (Memc[ffmt], CQ_SZ_FFMTS)
		if (nscan() != 6)
		    break

		# Add the field name to the field name dictionary.
                if ((sz1 - op1 + 1) < (CQ_SZ_FNAME + 1)) {
                    sz1 = sz1 + SZ_LINE
                    call realloc (CQ_FNAMES(res), sz1, TY_CHAR)
                }
                op1 = op1 + gstrcpy (Memc[fname], Memc[CQ_FNAMES(res)+op1-1],
                    sz1 - op1 + 1)
                op1 = op1 + gstrcpy ("|", Memc[CQ_FNAMES(res)+op1-1],
                    sz1 - op1 + 1)

		# Set the field offset, size, and type.
		Memi[CQ_FOFFSETS(res)+i-1] = foffset
		Memi[CQ_FTYPES(res)+i-1] = cq_dtype (ftype)
		Memi[CQ_FSIZES(res)+i-1] = fsize

		# Add the field units to the field units dictionary.
                if ((sz2 - op2 + 1) < (CQ_SZ_FUNITS + 1)) {
                    sz2 = sz2 + SZ_LINE
                    call realloc (CQ_FUNITS(res), sz2, TY_CHAR)
                }
                op2 = op2 + gstrcpy (Memc[funits], Memc[CQ_FUNITS(res)+op2-1],
                    sz2 - op2 + 1)
                op2 = op2 + gstrcpy ("|", Memc[CQ_FUNITS(res)+op2-1],
                    sz2 - op2 + 1)

		# Add the field format to the field format dictionary.
                if ((sz3 - op3 + 1) < (CQ_SZ_FFMTS + 1)) {
                    sz3 = sz3 + SZ_LINE
                    call realloc (CQ_FFMTS(res), sz3, TY_CHAR)
                }
                op3 = op3 + gstrcpy (Memc[ffmt], Memc[CQ_FFMTS(res)+op3-1],
                    sz3 - op3 + 1)
                op3 = op3 + gstrcpy ("|", Memc[CQ_FFMTS(res)+op3-1],
                    sz3 - op3 + 1)

		ncount = ncount + 1
	    }
	} 

	# Adjust the field description size.
	if (ncount != CQ_NFIELDS(res)) {
	    CQ_NFIELDS(res) = 0
	    call realloc (CQ_FNAMES(res), 1, TY_CHAR)
	    Memc[CQ_FNAMES(res)] = EOS
	    call mfree (CQ_FOFFSETS(res), TY_INT); CQ_FOFFSETS(res) = NULL
	    call mfree (CQ_FSIZES(res), TY_INT); CQ_FSIZES(res) = NULL
	    call mfree (CQ_FTYPES(res), TY_INT); CQ_FTYPES(res) = NULL
	    call realloc (CQ_FUNITS(res), 1, TY_CHAR)
	    Memc[CQ_FUNITS(res)] = EOS
	    call realloc (CQ_FFMTS(res), 1, TY_CHAR)
	    Memc[CQ_FFMTS(res)] = EOS
	} else {
	    call realloc (CQ_FNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_FUNITS(res), op2, TY_CHAR)
	    call realloc (CQ_FFMTS(res), op3, TY_CHAR)
	    Memc[CQ_FNAMES(res)+op1] = EOS
	    Memc[CQ_FUNITS(res)+op2] = EOS
	    Memc[CQ_FFMTS(res)+op3] = EOS
	}

	# Allocate space for the simple text field indices array.
	call calloc (CQ_FINDICES(res), CQ_MAX_NFIELDS + 1, TY_INT)

	# Initilize the records descriptor.
	CQ_RFD(res) = NULL

	call sfree (sp)

	return (res)
end


# Temporary definitions to get stuff working. Move into header file at some
# point ?

define	DIC_FNAMES	"|type|hskip|tskip|recsize|triml|trimr|nheader|nfields|"
define	DIC_TYPE	1 
define	DIC_HSKIP	2 
define	DIC_TSKIP	3 
define	DIC_RECORD	4 
define	DIC_TRIML	5 
define	DIC_TRIMR	6 
define	DIC_NHEADER	7 
define	DIC_NFIELDS	8 

# CQ_FRINIT -- Initialize a results descriptor from a file description.

pointer procedure cq_frinit (cq, catfmt)

pointer	cq			#I Initialize the results structure.
char	catfmt[ARB]		#I the catalog format desciption

pointer	res, sp, fname, funits, ffmt, fvalue
int	i, ncount, sz1, sz2, sz3, op1, op2, op3, fd, foffset, fsize
int	fscan(), nscan(), strdic(), strlen(), cq_dtype(), gstrcpy()
char	ftype
int	stropen()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)

	# Allocate the structure.
	call calloc (res, CQ_LEN_RES, TY_STRUCT)

	# Format the catalog information, the address, query, and query
	# parameters.
	call strcpy (CQ_CATDB(cq), CQ_RCATDB(res), SZ_LINE)
	call strcpy (CQ_CATNAME(cq), CQ_RCATNAME(res), SZ_LINE)
	call strcpy ("", CQ_RADDRESS(res), SZ_LINE)
	call strcpy ("", CQ_RQUERY(res), SZ_LINE)
	CQ_RNQPARS(res) = 0
	call malloc (CQ_RQPNAMES(res), 1, TY_CHAR)
	call malloc (CQ_RQPVALUES(res), 1, TY_CHAR)
	call malloc (CQ_RQPUNITS(res), 1, TY_CHAR)
	Memc[CQ_RQPNAMES(res)] = EOS
	Memc[CQ_RQPVALUES(res)] = EOS
	Memc[CQ_RQPUNITS(res)] = EOS

	# Set default file formats.
	CQ_RTYPE(res) = CQ_STEXT
	CQ_RHSKIP(res) = 0
	CQ_RTSKIP(res) = 0
	CQ_RECSIZE(res) = 0
	CQ_RTRIML(res) = 0
	CQ_RTRIMR(res) = 0
	CQ_NFIELDS(res) = 0

	call smark(sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)
	call salloc (funits, CQ_SZ_FUNITS, TY_CHAR)
	call salloc (ffmt, CQ_SZ_FFMTS, TY_CHAR)
	call salloc (fvalue, SZ_LINE, TY_CHAR)

	# Read in the defined file formats.
	fd = stropen (catfmt, strlen (catfmt), READ_ONLY)
	while (fscan (fd) != EOF) {

	    # Get the field name.
	    call gargwrd (Memc[fname], CQ_SZ_FNAME)
	    if (nscan () < 1 || Memc[fname] == EOS)
		next
	    i = strdic (Memc[fname], Memc[fname], CQ_SZ_FNAME, DIC_FNAMES) 

	    # Decode the field.
	    switch (i) {

	    case DIC_TYPE:
	        call gargwrd (Memc[fname], CQ_SZ_FNAME)
		if (nscan () < 2 || Memc[fname] == EOS)
		    CQ_RTYPE(res) = CQ_STEXT
		else
	    	    CQ_RTYPE(res) = strdic (Memc[fname], Memc[fname],
		        CQ_SZ_FNAME, CQ_RTYPESTR)

	    case DIC_HSKIP:
		call gargi (CQ_RHSKIP(res))
		if (nscan() < 2)
		    CQ_RHSKIP(res) = 0

	    case DIC_TSKIP:
		call gargi (CQ_RTSKIP(res))
		if (nscan() < 2)
		    CQ_RTSKIP(res) = 0

	    case DIC_RECORD:
		call gargi (CQ_RECSIZE(res))
		if (nscan() < 2)
		    CQ_RECSIZE(res) = 0

	    case DIC_TRIML:
		call gargi (CQ_RTRIML(res))
		if (nscan() < 2)
		    CQ_RTRIML(res) = 0

	    case DIC_TRIMR:
		call gargi (CQ_RTRIMR(res))
		if (nscan() < 2)
		    CQ_RTRIMR(res) = 0

	    case DIC_NHEADER:
		call gargi (CQ_NHEADER(res))
		if (nscan() < 2)
		    CQ_NHEADER(res) = 0

                call calloc (CQ_HKNAMES(res), SZ_LINE, TY_CHAR)
                call calloc (CQ_HKVALUES(res), SZ_LINE, TY_CHAR)

		ncount = 0
		if (CQ_NHEADER(res) > 0) {

		    # Initialize the header name and value dictionaries.
                    sz1 = SZ_LINE; op1 = 2
                    sz2 = SZ_LINE; op2 = 2
                    call strcpy ("|", Memc[CQ_HKNAMES(res)], sz1)
                    call strcpy ("|", Memc[CQ_HKVALUES(res)], sz2)

	    	    do i = 1, CQ_NHEADER(res) {

			# Get the keyword name and value.
			if (fscan (fd) == EOF)
			    break
			call gargwrd (Memc[fname], CQ_SZ_QPNAME)
			call gargwrd (Memc[fvalue], SZ_LINE)
			if (nscan() != 2)
		    	    break

			# Add the keyword name to the keyword dictionary.
                	if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
                    	    sz1 = sz1 + SZ_LINE
                    	    call realloc (CQ_HKNAMES(res), sz1, TY_CHAR)
                        }
                	op1 = op1 + gstrcpy (Memc[fname], Memc[CQ_HKNAMES(res)+
			    op1-1], sz1 - op1 + 1)
                	op1 = op1 + gstrcpy ("|", Memc[CQ_HKNAMES(res)+op1-1],
                    	    sz1 - op1 + 1)

			# Add the keyword value to the keyword value dictionary.
                	if ((sz2 - op2 + 1) < (CQ_SZ_QPVALUE + 1)) {
                    	    sz2 = sz2 + SZ_LINE
                    	    call realloc (CQ_HKVALUES(res), sz2, TY_CHAR)
                	}
                	op2 = op2 + gstrcpy (Memc[fvalue],
			    Memc[CQ_HKVALUES(res)+ op2-1], sz2 - op2 + 1)
                	op2 = op2 + gstrcpy ("|", Memc[CQ_HKVALUES(res)+op2-1],
                    	    sz2 - op2 + 1)

			ncount = ncount + 1
		    }
		}

		# Addjust the keyword dictionary sizes.
	    	if (ncount != CQ_NHEADER(res)) {
                    CQ_NHEADER(res) = 0
                    call realloc (CQ_HKNAMES(res), 1, TY_CHAR)
                    call realloc (CQ_HKVALUES(res), 1, TY_CHAR)
                    Memc[CQ_HKNAMES(res)] = EOS
                    Memc[CQ_HKVALUES(res)] = EOS
		} else {
                    call realloc (CQ_HKNAMES(res), op1, TY_CHAR)
                    call realloc (CQ_HKVALUES(res), op2, TY_CHAR)
                    Memc[CQ_HKNAMES(res)+op1] = EOS
                    Memc[CQ_HKVALUES(res)+op2] = EOS
		}

	    case DIC_NFIELDS:
		call gargi (CQ_NFIELDS(res))
		if (nscan() < 2)
		    CQ_NFIELDS(res) = 0

		# Allocate space for the field descriptors.
		call calloc (CQ_FNAMES(res), SZ_LINE, TY_CHAR)
	    	call calloc (CQ_FOFFSETS(res), CQ_NFIELDS(res), TY_INT)
	    	call calloc (CQ_FSIZES(res), CQ_NFIELDS(res), TY_INT)
	    	call calloc (CQ_FTYPES(res), CQ_NFIELDS(res), TY_INT)
		call calloc (CQ_FUNITS(res), SZ_LINE, TY_CHAR)
		call calloc (CQ_FFMTS(res), SZ_LINE, TY_CHAR)

		ncount = 0
		if (CQ_NFIELDS(res) > 0) {

		    sz1 = SZ_LINE; op1 = 2
		    sz2 = SZ_LINE; op2 = 2
		    sz3 = SZ_LINE; op3 = 2
	    	    call strcpy ("|", Memc[CQ_FNAMES(res)], sz1)
	    	    call strcpy ("|", Memc[CQ_FUNITS(res)], sz2)
	    	    call strcpy ("|", Memc[CQ_FFMTS(res)], sz3)

	    	    do i = 1, CQ_NFIELDS(res) {

			# Get the field description.
			if (fscan (fd) == EOF)
			    break
			call gargwrd (Memc[fname], CQ_SZ_FNAME)
			call gargi (foffset)
			call gargi (fsize)
			call gargc (ftype)
			call gargwrd (Memc[funits], CQ_SZ_FUNITS)
			call gargwrd (Memc[ffmt], CQ_SZ_FFMTS)
			if (nscan() != 6)
		    	    break

			# Add the field name to the field name dictionary.
                	if ((sz1 - op1 + 1) < (CQ_SZ_FNAME + 1)) {
                    	    sz1 = sz1 + SZ_LINE
                    	    call realloc (CQ_FNAMES(res), sz1, TY_CHAR)
                        }
                	op1 = op1 + gstrcpy (Memc[fname], Memc[CQ_FNAMES(res)+
			    op1-1], sz1 - op1 + 1)
                	op1 = op1 + gstrcpy ("|", Memc[CQ_FNAMES(res)+op1-1],
                    	    sz1 - op1 + 1)

			Memi[CQ_FOFFSETS(res)+i-1] = foffset
			Memi[CQ_FTYPES(res)+i-1] = cq_dtype (ftype)
			Memi[CQ_FSIZES(res)+i-1] = fsize

			# Add the field units to the field units dictionary.
                	if ((sz2 - op2 + 1) < (CQ_SZ_FUNITS + 1)) {
                    	    sz2 = sz2 + SZ_LINE
                    	    call realloc (CQ_FUNITS(res), sz2, TY_CHAR)
                	}
                	op2 = op2 + gstrcpy (Memc[funits],
			    Memc[CQ_FUNITS(res)+ op2-1], sz2 - op2 + 1)
                	op2 = op2 + gstrcpy ("|", Memc[CQ_FUNITS(res)+op2-1],
                    	    sz2 - op2 + 1)

			# Add the field format to the field formats dictionary.
                	if ((sz3 - op3 + 1) < (CQ_SZ_FFMTS + 1)) {
                    	    sz3 = sz3 + SZ_LINE
                    	    call realloc (CQ_FFMTS(res), sz3, TY_CHAR)
                	}
                	op3 = op3 + gstrcpy (Memc[ffmt],
			    Memc[CQ_FFMTS(res)+ op3 -1], sz3 - op3 + 1)
                	op3 = op3 + gstrcpy ("|", Memc[CQ_FFMTS(res)+op3-1],
                    	    sz3 - op3 + 1)

			ncount = ncount + 1
	    	    }
		}
	    	if (ncount != CQ_NFIELDS(res)) {
		    CQ_NFIELDS(res) = 0
		    call realloc (CQ_FNAMES(res), 1, TY_CHAR) 
		    Memc[CQ_FNAMES(res]) = EOS
		    call mfree (CQ_FOFFSETS(res), TY_INT)
		    CQ_FOFFSETS(res) = NULL
		    call mfree (CQ_FSIZES(res), TY_INT)
		    CQ_FSIZES(res) = NULL
		    call mfree (CQ_FTYPES(res), TY_INT)
		    CQ_FTYPES(res) = NULL
		    call realloc (CQ_FUNITS(res), 1, TY_CHAR) 
		    Memc[CQ_FUNITS(res)] = EOS
		    call realloc (CQ_FFMTS(res), 1, TY_CHAR) 
		    Memc[CQ_FFMTS(res)] = EOS
	    	} else {
		    call realloc (CQ_FNAMES(res), op1, TY_CHAR) 
		    call realloc (CQ_FUNITS(res), op2, TY_CHAR) 
		    call realloc (CQ_FFMTS(res), op3, TY_CHAR) 
		    Memc[CQ_FNAMES(res]+op1) = EOS
		    Memc[CQ_FUNITS(res)+op2] = EOS
		    Memc[CQ_FFMTS(res)+op3] = EOS
		}
	    default:
		;
	    }
	}
	call close (fd)
	call sfree (sp)

	# Allocate space for the field indices array.
	call calloc (CQ_FINDICES(res), CQ_MAX_NFIELDS + 1, TY_INT)

	# Initilize the records descriptor.
	CQ_RFD(res) = NULL

	return (res)
end


# CQ_RFREE -- Free the results structure.

procedure cq_rfree (res)

pointer	res			#U the results descriptor.

begin
	# Free the query parameter names, values, and units.
	if (CQ_RQPNAMES(res) != NULL)
	    call mfree (CQ_RQPNAMES(res), TY_CHAR)
	if (CQ_RQPVALUES(res) != NULL)
	    call mfree (CQ_RQPVALUES(res), TY_CHAR)
	if (CQ_RQPUNITS(res) != NULL)
	    call mfree (CQ_RQPUNITS(res), TY_CHAR)

	# Free the header names and values.
	if (CQ_HKNAMES(res) != NULL)
	    call mfree (CQ_HKNAMES(res), TY_CHAR)
	if (CQ_HKVALUES(res) != NULL)
	    call mfree (CQ_HKVALUES(res), TY_CHAR)

	# Free the field offsets, sizes, and types.
	if (CQ_FNAMES(res) != NULL)
	    call mfree (CQ_FNAMES(res), TY_CHAR)
	if (CQ_FOFFSETS(res) != NULL)
	    call mfree (CQ_FOFFSETS(res), TY_INT)
	if (CQ_FSIZES(res) != NULL)
	    call mfree (CQ_FSIZES(res), TY_INT)
	if (CQ_FTYPES(res) != NULL)
	    call mfree (CQ_FTYPES(res), TY_INT)
	if (CQ_FUNITS(res) != NULL)
	    call mfree (CQ_FUNITS(res), TY_CHAR)
	if (CQ_FFMTS(res) != NULL)
	    call mfree (CQ_FFMTS(res), TY_CHAR)

	# Free the record description.
	if (CQ_FINDICES(res) != NULL)
	    call mfree (CQ_FINDICES(res), TY_INT)

	# Free the record buffer.
	if (CQ_RINDEX(res) != NULL)
	    call mfree (CQ_RINDEX(res), TY_LONG)
	if (CQ_RFD(res) != NULL)
	    call close (CQ_RFD(res))

	if (res != NULL)
	    call mfree (res, TY_STRUCT)
end
