include <fset.h>
include <ctype.h>
include "cqdef.h"
include "cq.h"


define	DEF_SZ_INBUF	32768	# the maximum network transfer buffer size


# CQ_FIMQUERY -- Send a dummy image query on an existing image. The immage
# may be any supported IRAF images.

pointer	procedure cq_fimquery (cq, imname)

pointer	cq			#I the catalog database descriptor
char	imname[ARB]		#I the input image name

pointer	res	
int	cc
pointer	cq_firinit()
int	imaccess()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)

	# Open the network connection.
	if (imaccess (imname, READ_WRITE) != YES)
	    return (NULL)

	# Initialize the image results structure.
	res = cq_firinit (cq)

	# Return the results pointer.
	return (res)
end


# CQ_IMQUERY -- Send an image survey query and return the image as a file. 
# Currently only FITS files are supported. The calling program is responsible
# for generating an IRAF compatible image name. If the file already exists
# no file is created but a valid results descriptor is still created.

pointer	procedure cq_imquery (cq, imname)

pointer	cq			#I the catalog database descriptor
char	imname[ARB]		#I the image name

pointer	res, inbuf
char	url[SZ_PATHNAME], addr[SZ_LINE], query[SZ_LINE], buf[SZ_LINE]
int	cc, fd, outfd, nchars, ip, op
bool	done
pointer	cq_irinit()
int	ndopen(), strlen(), open(), read(), getline(), url_get()
errchk	ndopen(), awriteb(), open(), read(), getline()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)


	if (USE_URLGET) {
	    # Initialize the image results structure.
	    res = cq_irinit (cq)

	    call strcpy (CQ_ADDRESS(cc), buf, SZ_LINE)
	    for (ip=1; buf[ip] != ':'; ip=ip+1) ;	# skip 'inet:'
	    ip = ip + 1
	    for (    ; buf[ip] != ':'; ip=ip+1) ;	# skip '80:'
	    ip = ip + 1
	    for (op=1; buf[ip] != ':'; ip=ip+1) {
		addr[op] = buf[ip]
		op = op + 1
	    }
	    addr[op] = EOS

	    call strcpy (CQ_IMQUERY(res), buf, SZ_LINE)
	    for (op=1; !IS_WHITE(buf[op+4]); op=op+1)
		query[op] = buf[op+4]
	    query[op] = EOS

	    call sprintf (url, SZ_LINE, "http://%s%s")
		call pargstr (addr)
		call pargstr (query)

	    iferr {
	        call malloc (inbuf, DEF_SZ_INBUF, TY_CHAR)
		if (url_get (url, imname, inbuf) < 0)
		    call error (0, "Cannot access url")
		call mfree (inbuf, TY_CHAR)
	    } then {
	        if (res != NULL)
	            call cq_imclose (res)
		return (NULL)
	    }

	    return (res)
	}


	# Open the network connection.
	iferr (fd = ndopen (CQ_ADDRESS(cc), READ_WRITE))
	    return (NULL)

	# Initialize the image results structure.
	res = cq_irinit (cq)

	# Formulate the query.
	iferr {
	    switch (CQ_IMTYPE(res)) {
	    case CQ_FITS:
	        nchars = strlen (CQ_IMQUERY(res))
		call write (fd, CQ_IMQUERY(res), nchars)
	    default:
	        nchars = strlen (CQ_IMQUERY(res))
		call write (fd, CQ_IMQUERY(res), nchars)
	    }
	    call flush (fd)
	    call fseti (fd, F_CANCEL, OK)
	} then {
	    if (fd != NULL)
	        call close (fd)
	    if (res != NULL)
	        call cq_imclose (res)
	    return (NULL)
	}

	# Open the output file.
	outfd = NULL
	iferr {
	    # Open the output file. Worry about legal image names at a
	    # higher level.
	    switch (CQ_IMTYPE(res)) {
	    case CQ_FITS:
	        outfd = open (imname, NEW_FILE, TEXT_FILE)
	    default:
	        outfd = open (imname, NEW_FILE, TEXT_FILE)
	    }
	} then {
	    if (fd != NULL)
	        call close (fd)
	    if (res != NULL)
	        call cq_imclose (res)
	    return (NULL)
	}

	# Send the query and get back the results.
	inbuf = NULL
	iferr {

	    # Allocate the maximum buffer size.
	    call malloc (inbuf, DEF_SZ_INBUF, TY_CHAR)

	    # Skip a fixed number of bytes. Dangerous unless the header
	    # is always the same size.
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

	    # Get the data.
	    repeat {
	        nchars = read (fd, Memc[inbuf], DEF_SZ_INBUF)
	        if (nchars > 0) {
		    Memc[inbuf+nchars] = EOS
	            call write (outfd, Memc[inbuf], nchars)
		    done = false
	        } else {
		    done = true
	        }
	    } until (done)

	    # Cleanup.
	    call mfree (inbuf, TY_CHAR)
	    inbuf = NULL
	    call flush (outfd)
	    call close (outfd)
	    outfd = NULL
	    call close (fd)
	    fd = NULL

	} then {
	    if (inbuf != NULL)
		call mfree (inbuf, TY_CHAR)
	    if (outfd != NULL) {
		call close (outfd)
		call delete (imname)
	    }
	    if (fd != NULL)
	        call close (fd)
	    if (res != NULL)
	        call cq_imclose (res)
	    return (NULL)
	}

	# Return the results pointer.
	return (res)
end


# CQ_IMCLOSE -- Close the results structure,

procedure cq_imclose (res)

pointer	res			#U the results descriptor.

begin
	call cq_irfree (res)
end


# CQ_FIRINIT -- Initialize an image results descriptor.

pointer procedure cq_firinit (cq)

pointer	cq			#I the catalog descriptor

pointer	cc, res
pointer	sp, value, wpname, wkname, wkdvalue, wkvalue, wkunits
int	i, ncount, sz1, sz2, sz3, sz4, sz5, op1, op2, op3, op4, op5
char	ftype
int	cq_dgeti(), strdic(), cq_dscan(), nscan()
int	gstrcpy(), cq_dtype()
errchk	cq_dgwrd(), cq_dgeti(), cq_dscan()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)

	# Allocate the results structure.
	call calloc (res, CQ_LEN_IM, TY_STRUCT)

	# Format the query.
	call smark (sp)
	call salloc (value, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (wpname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (wkname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (wkdvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (wkvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (wkunits, CQ_SZ_QPUNITS, TY_CHAR)

	# Save the survey informaton and query in the results structure.
	call strcpy (CQ_CATDB(cq), CQ_IMCATDB(res), SZ_FNAME)
	call strcpy (CQ_CATNAME(cq), CQ_IMCATNAME(res), SZ_FNAME)
	call strcpy ("", CQ_IMADDRESS(res), SZ_LINE)
	call strcpy ("", CQ_IMQUERY(res), SZ_LINE)

	# Copy the query parameters to the results descriptor.
	CQ_INQPARS(res) = 0
	CQ_IQPNAMES(res) = NULL
	CQ_IQPVALUES(res) = NULL
	CQ_IQPUNITS(res) = NULL

	# Get the input image data type.
	iferr {
	    call cq_dgwrd (cq, CQ_CATNO(cq), "type", Memc[value],
	        CQ_SZ_QPVALUE)
	} then {
	    Memc[value] = EOS
	   CQ_IMTYPE(res) = CQ_FITS
	} else {
	    CQ_IMTYPE(res) = strdic (Memc[value], Memc[value], CQ_SZ_QPVALUE,
	        CQ_ITYPESTR)
	}

	# Get the input image data type.
	iferr {
	    call cq_dgwrd (cq, CQ_CATNO(cq), "wcs", Memc[value],
	        CQ_SZ_QPVALUE)
	} then {
	    CQ_IMTYPE(res) = CQ_WNONE
	} else {
	    CQ_WCS(res) = strdic (Memc[value], Memc[value], CQ_SZ_QPVALUE,
	        CQ_WTYPESTR)
	}

	# Get the number of wcs parameters.
	iferr (CQ_NWCS(res) = cq_dgeti (cq, CQ_CATNO(cq), "nwcs"))
	    CQ_NWCS(res) = 0

	# Allocate space for the wcs parameters.
	call calloc (CQ_WPNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKDVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKTYPES(res), CQ_NWCS(res), TY_INT)
	call calloc (CQ_WKUNITS(res), SZ_LINE, TY_CHAR)

	# Get the wcs parameters.
	ncount = 0
	if (CQ_NWCS(res) > 0) {

	    # Initialize the header parameter keywords and values.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    sz3 = SZ_LINE; op3 = 2
	    sz4 = SZ_LINE; op4 = 2
	    sz5 = SZ_LINE; op5 = 2
	    call strcpy ("|", Memc[CQ_WPNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_WKNAMES(res)], sz2)
	    call strcpy ("|", Memc[CQ_WKDVALUES(res)], sz3)
	    call strcpy ("|", Memc[CQ_WKVALUES(res)], sz4)
	    call strcpy ("|", Memc[CQ_WKUNITS(res)], sz5)

	    do i = 1, CQ_NWCS(res) {

		# Get the wcs parameter name, keyword, default value,
		# data type and units value.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[wpname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkdvalue], CQ_SZ_QPVALUE)
		call gargc (ftype)
		call gargwrd (Memc[wkunits], CQ_SZ_QPUNITS)
		if (nscan() != 5)
		    break

		# Add the parameter name to the list.
		if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz1 = sz1 + SZ_LINE
		    call realloc (CQ_WPNAMES(res), sz1, TY_CHAR)
		}
		op1 = op1 + gstrcpy (Memc[wpname], Memc[CQ_WPNAMES(res)+op1-1],
		    sz1 - op1 + 1)
		op1 = op1 + gstrcpy ("|", Memc[CQ_WPNAMES(res)+op1-1],
		    sz1 - op1 + 1)

		# Add the keyword name to the list.
		if ((sz2 - op2 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz2 = sz2 + SZ_LINE
		    call realloc (CQ_WKNAMES(res), sz2, TY_CHAR)
		}
		op2 = op2 + gstrcpy (Memc[wkname], Memc[CQ_WKNAMES(res)+op2-1],
		    sz2 - op2 + 1)
		op2 = op2 + gstrcpy ("|", Memc[CQ_WKNAMES(res)+op2-1],
		    sz2 - op2 + 1)

		# Add the default keyword value to the list.
		if ((sz3 - op3 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz3 = sz3 + SZ_LINE
		    call realloc (CQ_WKDVALUES(res), sz3, TY_CHAR)
		}
		op3 = op3 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_WKDVALUES(res)+op3-1], sz3 - op3 + 1)
		op3 = op3 + gstrcpy ("|", Memc[CQ_WKDVALUES(res)+op3-1],
		    sz3 - op3 + 1)

		# Add the keyword value to the list.
		if ((sz4 - op4 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz4 = sz4 + SZ_LINE
		    call realloc (CQ_WKVALUES(res), sz4, TY_CHAR)
		}
		op4 = op4 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_WKVALUES(res)+op4-1], sz4 - op4 + 1)
		op4 = op4 + gstrcpy ("|", Memc[CQ_WKVALUES(res)+op4-1],
		    sz4 - op4 + 1)

		# Compute the data type.
		Memi[CQ_WKTYPES(res)+i-1] = cq_dtype (ftype)

		# Add the default keyword value to the list.
		if ((sz5 - op5 + 1) < (CQ_SZ_QPUNITS + 1)) {
		    sz5 = sz5 + SZ_LINE
		    call realloc (CQ_WKUNITS(res), sz5, TY_CHAR)
		}
		op5 = op5 + gstrcpy (Memc[wkunits],
		    Memc[CQ_WKUNITS(res)+op5-1], sz5 - op5 + 1)
		op5 = op5 + gstrcpy ("|", Memc[CQ_WKUNITS(res)+op5-1],
		    sz5 - op5 + 1)

		ncount = ncount + 1
	    }
	}

	# Resize the wcs parameter arrays.
	if (ncount != CQ_NWCS(res)) {
	    CQ_NWCS(res) = 0
	    call realloc (CQ_WPNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_WKNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_WKDVALUES(res), 1, TY_CHAR)
	    call realloc (CQ_WKVALUES(res), 1, TY_CHAR)
	    call mfree (CQ_WKTYPES(res), TY_INT)
	    CQ_WKTYPES(res) = NULL
	    call realloc (CQ_WKUNITS(res), 1, TY_CHAR)
	} else {
	    call realloc (CQ_WPNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_WKNAMES(res), op2, TY_CHAR)
	    call realloc (CQ_WKDVALUES(res), op3, TY_CHAR)
	    call realloc (CQ_WKVALUES(res), op4, TY_CHAR)
	    call realloc (CQ_WKUNITS(res), op5, TY_CHAR)
	    Memc[CQ_WPNAMES(res)+op1] = EOS
	    Memc[CQ_WKNAMES(res)+op2] = EOS
	    Memc[CQ_WKDVALUES(res)+op3] = EOS
	    Memc[CQ_WKVALUES(res)+op4] = EOS
	    Memc[CQ_WKUNITS(res)+op5] = EOS
	}

	# Get the number of keyword parameters.
	iferr (CQ_NIMPARS(res) = cq_dgeti (cq, CQ_CATNO(cq), "nkeys"))
	    CQ_NIMPARS(res) = 0

	# Allocate space for the keyword parameters.
	call calloc (CQ_IPNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKDVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKTYPES(res), CQ_NIMPARS(res), TY_INT)
	call calloc (CQ_IKUNITS(res), SZ_LINE, TY_CHAR)

	# Get the keyword parameters.
	ncount = 0
	if (CQ_NIMPARS(res) > 0) {

	    # Initialize the header parameter keywords and values.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    sz3 = SZ_LINE; op3 = 2
	    sz4 = SZ_LINE; op4 = 2
	    sz5 = SZ_LINE; op5 = 2
	    call strcpy ("|", Memc[CQ_IPNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_IKNAMES(res)], sz2)
	    call strcpy ("|", Memc[CQ_IKDVALUES(res)], sz3)
	    call strcpy ("|", Memc[CQ_IKVALUES(res)], sz4)
	    call strcpy ("|", Memc[CQ_IKUNITS(res)], sz5)

	    do i = 1, CQ_NIMPARS(res) {

		# Get the wcs parameter name, keyword, default value,
		# data type and units value.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[wpname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkdvalue], CQ_SZ_QPVALUE)
		call gargc (ftype)
		call gargwrd (Memc[wkunits], CQ_SZ_QPUNITS)
		if (nscan() != 5)
		    break

		# Add the parameter name to the list.
		if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz1 = sz1 + SZ_LINE
		    call realloc (CQ_IPNAMES(res), sz1, TY_CHAR)
		}
		op1 = op1 + gstrcpy (Memc[wpname], Memc[CQ_IPNAMES(res)+op1-1],
		    sz1 - op1 + 1)
		op1 = op1 + gstrcpy ("|", Memc[CQ_IPNAMES(res)+op1-1],
		    sz1 - op1 + 1)

		# Add the keyword name to the list.
		if ((sz2 - op2 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz2 = sz2 + SZ_LINE
		    call realloc (CQ_IKNAMES(res), sz2, TY_CHAR)
		}
		op2 = op2 + gstrcpy (Memc[wkname], Memc[CQ_IKNAMES(res)+op2-1],
		    sz2 - op2 + 1)
		op2 = op2 + gstrcpy ("|", Memc[CQ_IKNAMES(res)+op2-1],
		    sz2 - op2 + 1)

		# Add the default keyword value to the list.
		if ((sz3 - op3 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz3 = sz3 + SZ_LINE
		    call realloc (CQ_IKDVALUES(res), sz3, TY_CHAR)
		}
		op3 = op3 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_IKDVALUES(res)+op3-1], sz3 - op3 + 1)
		op3 = op3 + gstrcpy ("|", Memc[CQ_IKDVALUES(res)+op3-1],
		    sz3 - op3 + 1)

		# Add the keyword value to the list.
		if ((sz4 - op4 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz4 = sz4 + SZ_LINE
		    call realloc (CQ_IKVALUES(res), sz4, TY_CHAR)
		}
		op4 = op4 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_IKVALUES(res)+op4-1], sz4 - op4 + 1)
		op4 = op4 + gstrcpy ("|", Memc[CQ_IKVALUES(res)+op4-1],
		    sz4 - op4 + 1)

		# Compute the data type.
		Memi[CQ_IKTYPES(res)+i-1] = cq_dtype (ftype)

		# Add the default keyword value to the list.
		if ((sz5 - op5 + 1) < (CQ_SZ_QPUNITS + 1)) {
		    sz5 = sz5 + SZ_LINE
		    call realloc (CQ_IKUNITS(res), sz5, TY_CHAR)
		}
		op5 = op5 + gstrcpy (Memc[wkunits],
		    Memc[CQ_IKUNITS(res)+op5-1], sz5 - op5 + 1)
		op5 = op5 + gstrcpy ("|", Memc[CQ_IKUNITS(res)+op5-1],
		    sz5 - op5 + 1)

		ncount = ncount + 1

	    }
	}

	# Resize the wcs parameter arrays.
	if (ncount != CQ_NIMPARS(res)) {
	    CQ_NIMPARS(res) = 0
	    call realloc (CQ_IPNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_IKNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_IKDVALUES(res), 1, TY_CHAR)
	    call realloc (CQ_IKVALUES(res), 1, TY_CHAR)
	    call mfree (CQ_IKTYPES(res), TY_INT)
	    CQ_IKTYPES(res) = NULL
	    call realloc (CQ_IKUNITS(res), 1, TY_CHAR)
	} else {
	    call realloc (CQ_IPNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_IKNAMES(res), op2, TY_CHAR)
	    call realloc (CQ_IKDVALUES(res), op3, TY_CHAR)
	    call realloc (CQ_IKVALUES(res), op4, TY_CHAR)
	    call realloc (CQ_IKUNITS(res), op5, TY_CHAR)
	    Memc[CQ_IPNAMES(res)+op1] = EOS
	    Memc[CQ_IKNAMES(res)+op2] = EOS
	    Memc[CQ_IKDVALUES(res)+op3] = EOS
	    Memc[CQ_IKVALUES(res)+op4] = EOS
	    Memc[CQ_IKUNITS(res)+op5] = EOS
	}

	call sfree (sp)

	return (res)
end


# CQ_IRINIT -- Initialize an image results descriptor.

pointer procedure cq_irinit (cq)

pointer	cq			#I the catalog descriptor

pointer	cc, res
pointer	sp, query, value, wpname, wkname, wkdvalue, wkvalue, wkunits
int	i, fsize, ncount, sz1, sz2, sz3, sz4, sz5, op1, op2, op3, op4, op5
char	ftype
int	cq_wrdstr(), cq_dgeti(), strlen(), strdic(), cq_dscan(), nscan()
int	gstrcpy(), cq_dtype()
errchk	cq_dgwrd(), cq_dgeti(), cq_dscan()

begin
	# Check that the current catalog is defined.
        if (CQ_CAT(cq) == NULL)
            return (NULL)
        if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
            return (NULL)
	cc = CQ_CAT(cq)

	# Allocate the results structure.
	call calloc (res, CQ_LEN_IM, TY_STRUCT)

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

	# Save the survey informaton and query in the results structure.
	call strcpy (CQ_CATDB(cq), CQ_IMCATDB(res), SZ_FNAME)
	call strcpy (CQ_CATNAME(cq), CQ_IMCATNAME(res), SZ_FNAME)
	call strcpy (CQ_ADDRESS(cc), CQ_IMADDRESS(res), SZ_LINE)
	call strcpy (Memc[query], CQ_IMQUERY(res), SZ_LINE)

	# Copy the query parameters to the results descriptor.
	CQ_INQPARS(res) = CQ_NQPARS(cc)
	fsize = strlen (Memc[CQ_PQPNAMES(cc)])
	call malloc (CQ_IQPNAMES(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPNAMES(cc)], Memc[CQ_IQPNAMES(res)], fsize)
	fsize = strlen (Memc[CQ_PQPVALUES(cc)])
	call malloc (CQ_IQPVALUES(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPVALUES(cc)], Memc[CQ_IQPVALUES(res)], fsize)
	fsize = strlen (Memc[CQ_PQPUNITS(cc)])
	call malloc (CQ_IQPUNITS(res), fsize, TY_CHAR)
	call strcpy (Memc[CQ_PQPUNITS(cc)], Memc[CQ_IQPUNITS(res)], fsize)

	# Get the input image data type.
	iferr {
	    call cq_dgwrd (cq, CQ_CATNO(cq), "type", Memc[value],
	        CQ_SZ_QPVALUE)
	} then {
	    Memc[value] = EOS
	   CQ_IMTYPE(res) = CQ_FITS
	} else {
	    CQ_IMTYPE(res) = strdic (Memc[value], Memc[value], CQ_SZ_QPVALUE,
	        CQ_ITYPESTR)
	}

	call salloc (wpname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (wkname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (wkdvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (wkvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (wkunits, CQ_SZ_QPUNITS, TY_CHAR)

	# Get the input image data type.
	iferr {
	    call cq_dgwrd (cq, CQ_CATNO(cq), "wcs", Memc[value],
	        CQ_SZ_QPVALUE)
	} then {
	    CQ_IMTYPE(res) = CQ_WNONE
	} else {
	    CQ_WCS(res) = strdic (Memc[value], Memc[value], CQ_SZ_QPVALUE,
	        CQ_WTYPESTR)
	}

	# Get the number of wcs parameters.
	iferr (CQ_NWCS(res) = cq_dgeti (cq, CQ_CATNO(cq), "nwcs"))
	    CQ_NWCS(res) = 0

	# Allocate space for the wcs parameters.
	call calloc (CQ_WPNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKDVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_WKTYPES(res), CQ_NWCS(res), TY_INT)
	call calloc (CQ_WKUNITS(res), SZ_LINE, TY_CHAR)

	# Get the wcs parameters.
	ncount = 0
	if (CQ_NWCS(res) > 0) {

	    # Initialize the header parameter keywords and values.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    sz3 = SZ_LINE; op3 = 2
	    sz4 = SZ_LINE; op4 = 2
	    sz5 = SZ_LINE; op5 = 2
	    call strcpy ("|", Memc[CQ_WPNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_WKNAMES(res)], sz2)
	    call strcpy ("|", Memc[CQ_WKDVALUES(res)], sz3)
	    call strcpy ("|", Memc[CQ_WKVALUES(res)], sz4)
	    call strcpy ("|", Memc[CQ_WKUNITS(res)], sz5)


	    do i = 1, CQ_NWCS(res) {

		# Get the wcs parameter name, keyword, default value,
		# data type and units value.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[wpname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkdvalue], CQ_SZ_QPVALUE)
		call gargc (ftype)
		call gargwrd (Memc[wkunits], CQ_SZ_QPUNITS)
		if (nscan() != 5)
		    break

		# Add the parameter name to the list.
		if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz1 = sz1 + SZ_LINE
		    call realloc (CQ_WPNAMES(res), sz1, TY_CHAR)
		}
		op1 = op1 + gstrcpy (Memc[wpname], Memc[CQ_WPNAMES(res)+op1-1],
		    sz1 - op1 + 1)
		op1 = op1 + gstrcpy ("|", Memc[CQ_WPNAMES(res)+op1-1],
		    sz1 - op1 + 1)

		# Add the keyword name to the list.
		if ((sz2 - op2 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz2 = sz2 + SZ_LINE
		    call realloc (CQ_WKNAMES(res), sz2, TY_CHAR)
		}
		op2 = op2 + gstrcpy (Memc[wkname], Memc[CQ_WKNAMES(res)+op2-1],
		    sz2 - op2 + 1)
		op2 = op2 + gstrcpy ("|", Memc[CQ_WKNAMES(res)+op2-1],
		    sz2 - op2 + 1)

		# Add the default keyword value to the list.
		if ((sz3 - op3 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz3 = sz3 + SZ_LINE
		    call realloc (CQ_WKDVALUES(res), sz3, TY_CHAR)
		}
		op3 = op3 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_WKDVALUES(res)+op3-1], sz3 - op3 + 1)
		op3 = op3 + gstrcpy ("|", Memc[CQ_WKDVALUES(res)+op3-1],
		    sz3 - op3 + 1)

		# Add the keyword value to the list.
		if ((sz4 - op4 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz4 = sz4 + SZ_LINE
		    call realloc (CQ_WKVALUES(res), sz4, TY_CHAR)
		}
		op4 = op4 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_WKVALUES(res)+op4-1], sz4 - op4 + 1)
		op4 = op4 + gstrcpy ("|", Memc[CQ_WKVALUES(res)+op4-1],
		    sz4 - op4 + 1)

		# Compute the data type.
		Memi[CQ_WKTYPES(res)+i-1] = cq_dtype (ftype)

		# Add the default keyword value to the list.
		if ((sz5 - op5 + 1) < (CQ_SZ_QPUNITS + 1)) {
		    sz5 = sz5 + SZ_LINE
		    call realloc (CQ_WKUNITS(res), sz5, TY_CHAR)
		}
		op5 = op5 + gstrcpy (Memc[wkunits],
		    Memc[CQ_WKUNITS(res)+op5-1], sz5 - op5 + 1)
		op5 = op5 + gstrcpy ("|", Memc[CQ_WKUNITS(res)+op5-1],
		    sz5 - op5 + 1)

		ncount = ncount + 1
	    }
	}

	# Resize the wcs parameter arrays.
	if (ncount != CQ_NWCS(res)) {
	    CQ_NWCS(res) = 0
	    call realloc (CQ_WPNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_WKNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_WKDVALUES(res), 1, TY_CHAR)
	    call realloc (CQ_WKVALUES(res), 1, TY_CHAR)
	    call mfree (CQ_WKTYPES(res), TY_INT)
	    CQ_WKTYPES(res) = NULL
	    call realloc (CQ_WKUNITS(res), 1, TY_CHAR)
	} else {
	    call realloc (CQ_WPNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_WKNAMES(res), op2, TY_CHAR)
	    call realloc (CQ_WKDVALUES(res), op3, TY_CHAR)
	    call realloc (CQ_WKVALUES(res), op4, TY_CHAR)
	    call realloc (CQ_WKUNITS(res), op5, TY_CHAR)
	    Memc[CQ_WPNAMES(res)+op1] = EOS
	    Memc[CQ_WKNAMES(res)+op2] = EOS
	    Memc[CQ_WKDVALUES(res)+op3] = EOS
	    Memc[CQ_WKVALUES(res)+op4] = EOS
	    Memc[CQ_WKUNITS(res)+op5] = EOS
	}

	# Get the number of keyword parameters.
	iferr (CQ_NIMPARS(res) = cq_dgeti (cq, CQ_CATNO(cq), "nkeys"))
	    CQ_NIMPARS(res) = 0

	# Allocate space for the keyword parameters.
	call calloc (CQ_IPNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKNAMES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKDVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKVALUES(res), SZ_LINE, TY_CHAR)
	call calloc (CQ_IKTYPES(res), CQ_NIMPARS(res), TY_INT)
	call calloc (CQ_IKUNITS(res), SZ_LINE, TY_CHAR)

	# Get the keyword parameters.
	ncount = 0
	if (CQ_NIMPARS(res) > 0) {

	    # Initialize the header parameter keywords and values.
	    sz1 = SZ_LINE; op1 = 2
	    sz2 = SZ_LINE; op2 = 2
	    sz3 = SZ_LINE; op3 = 2
	    sz4 = SZ_LINE; op4 = 2
	    sz5 = SZ_LINE; op5 = 2
	    call strcpy ("|", Memc[CQ_IPNAMES(res)], sz1)
	    call strcpy ("|", Memc[CQ_IKNAMES(res)], sz2)
	    call strcpy ("|", Memc[CQ_IKDVALUES(res)], sz3)
	    call strcpy ("|", Memc[CQ_IKVALUES(res)], sz4)
	    call strcpy ("|", Memc[CQ_IKUNITS(res)], sz5)

	    do i = 1, CQ_NIMPARS(res) {

		# Get the wcs parameter name, keyword, default value,
		# data type and units value.
		if (cq_dscan (cq) == EOF)
		    break
		call gargwrd (Memc[wpname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkname], CQ_SZ_QPNAME)
		call gargwrd (Memc[wkdvalue], CQ_SZ_QPVALUE)
		call gargc (ftype)
		call gargwrd (Memc[wkunits], CQ_SZ_QPUNITS)
		if (nscan() != 5)
		    break

		# Add the parameter name to the list.
		if ((sz1 - op1 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz1 = sz1 + SZ_LINE
		    call realloc (CQ_IPNAMES(res), sz1, TY_CHAR)
		}
		op1 = op1 + gstrcpy (Memc[wpname], Memc[CQ_IPNAMES(res)+op1-1],
		    sz1 - op1 + 1)
		op1 = op1 + gstrcpy ("|", Memc[CQ_IPNAMES(res)+op1-1],
		    sz1 - op1 + 1)

		# Add the keyword name to the list.
		if ((sz2 - op2 + 1) < (CQ_SZ_QPNAME + 1)) {
		    sz2 = sz2 + SZ_LINE
		    call realloc (CQ_IKNAMES(res), sz2, TY_CHAR)
		}
		op2 = op2 + gstrcpy (Memc[wkname], Memc[CQ_IKNAMES(res)+op2-1],
		    sz2 - op2 + 1)
		op2 = op2 + gstrcpy ("|", Memc[CQ_IKNAMES(res)+op2-1],
		    sz2 - op2 + 1)

		# Add the default keyword value to the list.
		if ((sz3 - op3 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz3 = sz3 + SZ_LINE
		    call realloc (CQ_IKDVALUES(res), sz3, TY_CHAR)
		}
		op3 = op3 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_IKDVALUES(res)+op3-1], sz3 - op3 + 1)
		op3 = op3 + gstrcpy ("|", Memc[CQ_IKDVALUES(res)+op3-1],
		    sz3 - op3 + 1)

		# Add the keyword value to the list.
		if ((sz4 - op4 + 1) < (CQ_SZ_QPVALUE + 1)) {
		    sz4 = sz4 + SZ_LINE
		    call realloc (CQ_IKVALUES(res), sz4, TY_CHAR)
		}
		op4 = op4 + gstrcpy (Memc[wkdvalue],
		    Memc[CQ_IKVALUES(res)+op4-1], sz4 - op4 + 1)
		op4 = op4 + gstrcpy ("|", Memc[CQ_IKVALUES(res)+op4-1],
		    sz4 - op4 + 1)

		# Compute the data type.
		Memi[CQ_IKTYPES(res)+i-1] = cq_dtype (ftype)

		# Add the default keyword value to the list.
		if ((sz5 - op5 + 1) < (CQ_SZ_QPUNITS + 1)) {
		    sz5 = sz5 + SZ_LINE
		    call realloc (CQ_IKUNITS(res), sz5, TY_CHAR)
		}
		op5 = op5 + gstrcpy (Memc[wkunits],
		    Memc[CQ_IKUNITS(res)+op5-1], sz5 - op5 + 1)
		op5 = op5 + gstrcpy ("|", Memc[CQ_IKUNITS(res)+op5-1],
		    sz5 - op5 + 1)

		ncount = ncount + 1

	    }
	}

	# Resize the wcs parameter arrays.
	if (ncount != CQ_NIMPARS(res)) {
	    CQ_NIMPARS(res) = 0
	    call realloc (CQ_IPNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_IKNAMES(res), 1, TY_CHAR)
	    call realloc (CQ_IKDVALUES(res), 1, TY_CHAR)
	    call realloc (CQ_IKVALUES(res), 1, TY_CHAR)
	    call mfree (CQ_IKTYPES(res), TY_INT)
	    CQ_IKTYPES(res) = NULL
	    call realloc (CQ_IKUNITS(res), 1, TY_CHAR)
	} else {
	    call realloc (CQ_IPNAMES(res), op1, TY_CHAR)
	    call realloc (CQ_IKNAMES(res), op2, TY_CHAR)
	    call realloc (CQ_IKDVALUES(res), op3, TY_CHAR)
	    call realloc (CQ_IKVALUES(res), op4, TY_CHAR)
	    call realloc (CQ_IKUNITS(res), op5, TY_CHAR)
	    Memc[CQ_IPNAMES(res)+op1] = EOS
	    Memc[CQ_IKNAMES(res)+op2] = EOS
	    Memc[CQ_IKDVALUES(res)+op3] = EOS
	    Memc[CQ_IKVALUES(res)+op4] = EOS
	    Memc[CQ_IKUNITS(res)+op5] = EOS
	}

	call sfree (sp)

	return (res)
end


# CQ_IRFREE -- Free the image results structure.

procedure cq_irfree (res)

pointer	res			#U the results descriptor.

begin
	# Free the query parameter names, values, and units.
	if (CQ_IQPNAMES(res) != NULL)
	    call mfree (CQ_IQPNAMES(res), TY_CHAR)
	if (CQ_IQPVALUES(res) != NULL)
	    call mfree (CQ_IQPVALUES(res), TY_CHAR)
	if (CQ_IQPUNITS(res) != NULL)
	    call mfree (CQ_IQPUNITS(res), TY_CHAR)

	# Free the wcs parameters.
	if (CQ_WPNAMES(res) != NULL)
	    call mfree (CQ_WPNAMES(res), TY_CHAR)
	if (CQ_WKNAMES(res) != NULL)
	    call mfree (CQ_WKNAMES(res), TY_CHAR)
	if (CQ_WKDVALUES(res) != NULL)
	    call mfree (CQ_WKDVALUES(res), TY_CHAR)
	if (CQ_WKVALUES(res) != NULL)
	    call mfree (CQ_WKVALUES(res), TY_CHAR)
	if (CQ_WKTYPES(res) != NULL)
	    call mfree (CQ_WKTYPES(res), TY_INT)
	if (CQ_WKUNITS(res) != NULL)
	    call mfree (CQ_WKUNITS(res), TY_CHAR)

	# Free the image keyword parameters.
	if (CQ_IPNAMES(res) != NULL)
	    call mfree (CQ_IPNAMES(res), TY_CHAR)
	if (CQ_IKNAMES(res) != NULL)
	    call mfree (CQ_IKNAMES(res), TY_CHAR)
	if (CQ_IKDVALUES(res) != NULL)
	    call mfree (CQ_IKDVALUES(res), TY_CHAR)
	if (CQ_IKVALUES(res) != NULL)
	    call mfree (CQ_IKVALUES(res), TY_CHAR)
	if (CQ_IKTYPES(res) != NULL)
	    call mfree (CQ_IKTYPES(res), TY_INT)
	if (CQ_IKUNITS(res) != NULL)
	    call mfree (CQ_IKUNITS(res), TY_CHAR)

	if (res != NULL)
	    call mfree (res, TY_STRUCT)
end
