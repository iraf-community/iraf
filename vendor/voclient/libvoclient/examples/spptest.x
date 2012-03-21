#
#  SPPTEST -- SPP test programs for the VOClient interface.
#
#  M. Fitzpatrick, NOAO, Jul 2006


task  simple	= t_simple,
      cone	= t_cone,
      siap 	= t_siap,
      sesame 	= t_sesame,
      skybot 	= t_skybot,
      registry1 = t_registry1,
      registry2 = t_registry2,
      registry3 = t_registry3


define DEF_RA       12.0;                        # default search values
define DEF_DEC      12.0;
define DEF_SIZE     0.1;
define DEF_SR       0.5;

define CONE_SVC     "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&"
define SIAP_SVC     "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?"


define	MAX_IMAGES	5

define	SZ_URL		512
define	SZ_RESULT	128000		# allow up to 128K of results


#  SIMPLE -- Trivial SPP task to call the high-level interface functions.
#  Here we test the all-in-one caller procedures and get the result as a
#  string.  We'll simply write out the string, a real app could just as
#  easily open the string as a file and parse it as needed.

procedure t_simple ()

char	fname[SZ_FNAME], otype[SZ_FNAME], svctype[SZ_FNAME]
char	result[SZ_RESULT]
double	ra, dec, size
int	reslen

double	clgetd()
bool	streq()
int	vx_conecaller()		# returns the length of the result
int	vx_siapcaller()		# returns the length of the result

begin
	call aclrs (fname, SZ_FNAME)		# clear string arrays
	call aclrs (otype, SZ_FNAME)
	call aclrs (svctype, SZ_FNAME)
	call aclrs (result, SZ_RESULT)

	call clgstr ("dal", svctype, SZ_FNAME) 	# get task parameters
	ra = clgetd ("ra")
	dec = clgetd ("dec")
	size = clgetd ("size")
	call clgstr ("type", otype, SZ_FNAME)

	# Now call the service requested.
	if (streq ("cone", svctype)) {
	    reslen = vx_coneCaller (CONE_SVC, ra, dec, size, otype, 
		result, SZ_RESULT)

	} else if (streq ("siap", svctype)) {
	    reslen = vx_siapCaller (SIAP_SVC, ra, dec, size, size, 
		"image/fits", otype, result, SZ_RESULT)

	} else {
	    call eprintf ("Invalid service type '%s'\n")
		call pargstr (svctype)
	    return
	}

	# Print the results to the screen.
	if (reslen > 0) {
	    call write (STDOUT, result, reslen)
	    if (reslen >= SZ_RESULT)
		call eprintf ("\n\nResults possibly truncated....\n\n")
	}  else
	    call eprintf ("No results from service\n")
end


#  CONE -- Simple test program to call a Cone search service and summarize
#  the results to the screen.

procedure t_cone ()

char	qstring[SZ_URL]
char	sid[SZ_FNAME], sra[SZ_FNAME], sdec[SZ_FNAME]
double	ra, dec, size
int	cone, query, qr, rec
int	i, nrec, nattr, len

double	clgetd()
int	vx_initVOClient(), vx_coneConnection()
int	vx_executeQuery(), vx_coneQuery(), vx_stringAttr()
int	vx_record(), vx_recordCount(), vx_atCount()

begin
	ra = clgetd ("ra") 			# get task parameters
	dec = clgetd ("dec")
	size = clgetd ("size")

	# Make the Cone call and summarize the table.

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	# Open a connection and form the query.
	cone = vx_coneConnection (CONE_SVC)
	query = vx_coneQuery (cone, ra, dec, size)

	call vx_queryString (query, "cone", 0, qstring, SZ_PATHNAME)
	call printf ("\nExecuting query:\n  %s\n\n")
	    call pargstr (qstring)

	# Execute the query.
	qr = vx_executeQuery (query)

	# Summarize the response.
	nrec = vx_recordCount (qr)
	if (nrec <= 0) {
	    call eprintf ("No records matched.")
	    return 

	} else {
	    rec = vx_record (qr, 0)
	    nattr = 0
	    if (rec != ERR)  
		nattr = vx_atCount (rec)

	    call printf ("# return %d records containing %d attrs each\n#\n")
		call pargi (nrec)
		call pargi (nattr)

	    call printf ("#\n# --- Summary output ---\n#\n")
	}


	# Download a few images.
	for (i = 0; i < nrec; i=i+1) {
	    rec = vx_record (qr, i)

	    if (rec > 0) {
	        len = vx_stringAttr (rec, "ID_MAIN", sid, SZ_FNAME)
	        len = vx_stringAttr (rec, "POS_EQ_RA_MAIN", sra, SZ_FNAME)
	        len = vx_stringAttr (rec, "POS_EQ_DEC_MAIN", sdec, SZ_FNAME)

	        call printf ("%4d id=%s  ra=%s  dec=%s\n")
		    call pargi (i)
		    call pargstr (sid)
		    call pargstr (sra)
		    call pargstr (sdec)

	    } else {
	        call eprintf ("\nError getting record number %d\n")
		    call pargi (i)
	    }

	}	

	call vx_closeConnection (cone)
	call vx_closeVOClient (0)
end


#  SIAP -- Simple test procedure to call a SIAP service and download several
#  test images.

procedure t_siap ()

char	fname[SZ_FNAME], qstring[SZ_URL], acref[SZ_URL]
double	ra, dec, size
int	siap, query, qr, rec, v
int	i, nrec, nattr, len

double	clgetd()
int	vx_initVOClient(), vx_siapConnection()
int	vx_executeQuery(), vx_siapQuery()
int	vx_record(), vx_recordCount(), vx_atCount()
int	vx_getAttribute(), vx_getDataset(), vx_stringValue()

begin
	ra = clgetd ("ra") 			# get task parameters
	dec = clgetd ("dec")
	size = clgetd ("size")

	# Make the SIAP call and download a few images.

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	# Open a connection and form the query.
	siap = vx_siapConnection (SIAP_SVC)
	query = vx_siapQuery (siap, ra, dec, size, size, "image/fits")

	call vx_queryString (query, "siap", 0, qstring, SZ_PATHNAME)
	call printf ("\nExecuting query:\n  %s\n\n")
	    call pargstr (qstring)

	# Execute the query.
	qr = vx_executeQuery (query)

	# Summarize the response.
	nrec = vx_recordCount (qr)
	if (nrec <= 0) {
	    call eprintf ("No records matched.")
	    return 

	} else {
	    rec = vx_record (qr, 0)
	    nattr = 0
	    if (rec != ERR)  
		nattr = vx_atCount (rec)

	    call printf ("# return %d records containing %d attrs each\n#\n")
		call pargi (nrec)
		call pargi (nattr)
	}


	# Download a few images.
	for (i = 0; i < nrec && i < MAX_IMAGES; i=i+1) {
	    rec = vx_record (qr, i)
	    v = vx_getAttribute (rec, "AccessReference")

	    if (v <= 0)
		next

	    call aclrs (fname, SZ_FNAME)		# clear string arrays
	    call sprintf (fname, SZ_FNAME, "dataset%04d.fits")
		call pargi (i)

	    len = vx_stringValue (v, acref, SZ_URL)
	    call printf ("Downloading: %s\n")
		call pargstr (acref)

	    if (vx_getDataset (rec, acref, fname) == OK) {
		call printf ("\n    Saved to file: %s\n\n")
		    call pargstr (fname)
	    } else 
		call eprintf ("\n    Downloaded failed\n")
	}	

	call vx_closeConnection (siap)
	call vx_closeVOClient (0)
end


#  SESAME -- Simple test procedure of the Sesame service.

procedure t_sesame ()

char	target[SZ_FNAME], pos[SZ_LINE]
double	ra, dec
int	sr, len

int	vx_initVOClient()
int	vx_nameresolver(), vx_resolverpos()
double	vx_resolverra(), vx_resolverdec()

begin
	call clgstr ("target", target, SZ_FNAME)

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	sr  = vx_nameresolver (target)

	ra  = vx_resolverra (sr)
	dec = vx_resolverdec (sr)
	len = vx_resolverpos (sr, pos, SZ_LINE)

	call printf ("target=%s  ra=%.6f  dec=%.6f  (%s)\n\n")
	    call pargstr (target)
	    call pargd (ra)
	    call pargd (dec)
	    call pargstr (pos)

	call vx_closeVOClient (0)
end


#  SKYBOT -- Simple test procedure of the SkyBoT service.

procedure t_skybot ()

char	name[SZ_FNAME]
double	ra, dec, sr, epoch, vmag
int	sb, nobjs, len, i

real	clgetr()
int	vx_initVOClient()
int	vx_skybot(), vx_skybotnobjs(), vx_skybotstr()
double	vx_skybotdbl()

begin
	ra = clgetr ("ra") 		# Get the parameters
	dec = clgetr ("dec")
	sr = clgetr ("sr")
	epoch = clgetr ("epoch")

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	call printf ("#\n# Search Terms: ra=%.6f dec=%.6f sr=%.1f epoch=%.3f\n")
	    call pargd (ra)
	    call pargd (dec)
	    call pargd (sr)
	    call pargd (epoch)

	# Call the service
	sb  = vx_skybot (ra, dec, sr, sr, epoch)

	nobjs = vx_skybotnobjs (sb)
	call printf ("#\n#  Found %d objects\n#\n")
	    call pargi (nobjs)

	for (i=0; i < nobjs; i=i+1) {
	    len = vx_skybotstr (sb, "name", i, name, SZ_FNAME)
	    ra = vx_skybotdbl (sb, "ra", i)
	    dec = vx_skybotdbl (sb, "dec", i)
	    vmag = vx_skybotdbl (sb, "vmag", i)

	    call printf ("%d: obj=%12s  ra=%.6f  dec=%.6f  Mv=%s\n")
	        call pargi (i)
	        call pargstr (name)
	        call pargd (ra)
	        call pargd (dec)
	        call pargd (vmag)
	}

	call vx_closeVOClient (0)
end


#  REGISTRY -- Simple tests of the Registry search procedures.

procedure t_registry1 ()

char	sql[SZ_LINE], keywords[SZ_LINE]
int	orValues, res, count

int	vx_initVOClient(), vx_regsearch(), vx_rscount(), btoi()
bool	clgetb()

begin
	call clgstr ("sql", sql, SZ_LINE)
	call clgstr ("keywords", keywords, SZ_LINE)
	orValues = btoi (clgetb ("orValues"))

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	# Do the Registry search, get back a result object.  Remember that
	# the 'orValues' is an integer in the interface, we converted from
	# the boolean parameter above.

	res  = vx_regsearch (sql, keywords, orValues)

	count = vx_rscount (res)
	call printf ("#\n# Found %d matching records\n#\n\n")
	    call pargi (count)

	call reg_printRecords (res, count)

	call vx_closeVOClient (0)
end


procedure t_registry2 ()

char	sql[SZ_LINE], keywords[SZ_LINE], qstring[1024]
int	orValues, res, query, count, len

int	vx_initVOClient(), vx_rscount(), vx_regquery()
int	btoi(), vx_rgetquerystring(), vx_rexecute()
bool	clgetb()

begin
	call clgstr ("sql", sql, SZ_LINE)
	call clgstr ("keywords", keywords, SZ_LINE)
	orValues = btoi (clgetb ("orValues"))

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	# Do the Registry search, get back a result object.  Remember that
	# the 'orValues' is an integer in the interface, we converted from
	# the boolean parameter above.

	query = vx_regquery (sql, 0);
	call vx_raddsearchterm (query, keywords, orValues)

	len = vx_rgetquerystring (query, qstring, 1024)
	call printf ("#\n# Query;\n    %s\n#\n\n")
	    call pargstr (qstring)

	res = vx_rexecute (query)
	    
	count = vx_rscount (res)
	call printf ("#\n# Found %d matching records\n#\n\n")
	    call pargi (count)

	call reg_printRecords (res, count)

	call vx_closeVOClient (0)
end


procedure t_registry3 ()

char	svc[SZ_LINE], term[SZ_LINE]
int	orValues, res, count

int	vx_initVOClient(), vx_regsearchbysvc(), vx_rscount(), btoi()
bool	clgetb()

begin
	call clgstr ("svc", svc, SZ_LINE)
	call clgstr ("term", term, SZ_LINE)
	orValues = btoi (clgetb ("orValues"))

	if (vx_initVOClient("") == ERR)		# Initialize
	    return

	# Do the Registry search given the particular service type.  The
	# search term may be either a keyword list or another SQL predicate.

	res  = vx_regsearchbysvc (svc, term, orValues)

	count = vx_rscount (res)
	call printf ("#\n# Found %d matching records\n#\n\n")
	    call pargi (count)

	call reg_printRecords (res, count)

	call vx_closeVOClient (0)
end


procedure reg_printRecords (res, count)

int	res, count, i
char	title[SZ_LINE], url[SZ_LINE], svctype[SZ_LINE], type[SZ_LINE]

begin
    for (i = 0; i < count; i=i+1) {
        call vx_rsstr (res, "Title", i, title, SZ_LINE)
        call vx_rsstr (res, "ServiceUrl", i, url, SZ_LINE)
        call vx_rsstr (res, "ResourceType", i, svctype, SZ_LINE)
        call vx_rsstr (res, "Type", i, type, SZ_LINE)

        call printf ("\n----------------------------------------------------\n")
        call printf ("(%d of %d) %s\n")
            call pargi (i+1)
            call pargi (count)
            call pargstr (title)
        call printf ("ServiceURL:  %s\n")
            call pargstr (url)
        call printf ("ResourceType:  %s\t\tType: %s\n")
            call pargstr (svctype)
            call pargstr (type)
    }
end



