# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<mach.h>
include	<fset.h>


# HTTP error codes we care about

define	HTTP_OK		200			# Success
define	HTTP_CREATED	201			# Created
define	HTTP_ACCEPTED	202			# Accepted
define	HTTP_PARTIAL	203			# Partial Information
define	HTTP_NORESP	204			# No Response

define	HTTP_MOVED	301			# Moved
define	HTTP_FOUND	302			# Found
define	HTTP_SEEOTHER	303			# Method
define	HTTP_NOTMOD	304			# Not Modified

define	HTTP_BADREQ	400			# Bad Request
define	HTTP_UNAUTH	401			# Unauthorized
define	HTTP_PAYMENT	402			# Payment Required
define	HTTP_FORBIDDEN	403			# Forbidden
define	HTTP_NOTFOUND	404			# Not Found

define	HTTP_INTERR	500			# Internal Error
define	HTTP_NOTIMP	501			# Not Implemented
define	HTTP_OVERLOAD	502			# Service Temporarily Overloaded
define	HTTP_GWTIMEOUT	503			# Gateway Timeout

define 	SZ_BUF		8192			# download buffer

define 	DBG_HDRS	FALSE



# URL_GET -- Do an HTTP GET on the given URL, save the results to the named
# file.  If a 'reply' pointer is given, return the request reply string (must
# be allocated at least 8192 chars).

int procedure url_get (url, fname, reply)

char	url[ARB]				#i URL to access
char	fname[ARB]				#i local filename
pointer	reply					#u pointer to reply string

char	protocol[SZ_FNAME], host[SZ_FNAME], path[SZ_BUF], emsg[SZ_PATHNAME]
char	inurl[SZ_LINE], outname[SZ_LINE]
int	port, stat
pointer buf

int	url_access(), strcmp()
bool	url_redirect()

define	redirect_	99

begin
	# Breakup the URL into usable pieces.
	call strcpy (url, inurl, SZ_LINE)
redirect_
	call url_break (inurl, protocol, host, port, path)

	# Check for a supported protocol.
	if (strcmp (protocol, "http") != 0) {
	    call aclrc (emsg, SZ_PATHNAME)
	    call sprintf (emsg, SZ_PATHNAME,  "Unsupported URI protocol (%s)")
		call pargstr (protocol)
	    call error (0, emsg)
	}

	# Download the file to the given name
	call strcpy (fname, outname, SZ_PATHNAME)

	if (reply == NULL) {
	    call calloc (buf, SZ_BUF, TY_CHAR)
	    stat = url_access (host, port, path, outname, buf)
	    if (url_redirect (stat, buf, inurl)) {     # check for a redirection
		call mfree (buf, TY_CHAR)
	        goto redirect_
	    }
	    call mfree (buf, TY_CHAR)

	} else {
	    stat = url_access (host, port, path, outname, reply)
	    if (url_redirect (stat, reply, inurl))     # check for a redirection
	        goto redirect_
	}

	# URL Error Codes are returned as negative values, positive values 
	# are the number of bytes read.  We let the caller decode the return
	# value, if desired, using the url_errcode() procedure.

	return (stat)
end


# URL_REDIRECT -- Check for a redirection reply code and modify the URL so
# we can try again.

bool procedure url_redirect (stat, reply, url)

int	stat					#i status code
pointer	reply					#i pointer to reply string
char	url[ARB]				#u access url

int	code, loc
pointer	ip, op
char	inurl[SZ_LINE]

int	strsearch()
bool	streq()

begin
	code = - stat

	if (code == HTTP_MOVED || code == HTTP_FOUND || code == HTTP_SEEOTHER) {
	    loc = strsearch (Memc[reply], "Location:")
	    if (loc > 0) {
	 	call aclrc (inurl, SZ_LINE)
		call strcpy (url, inurl, SZ_LINE)
        	for (ip=reply+loc; IS_WHITE(Memc[ip]); ip=ip+1)
                    ;
		for (op=1; op <= SZ_LINE && ip <= reply + SZ_BUF && Memc[ip] != '\n' && Memc[ip] != EOS; op=op+1) {
		    url[op] = Memc[ip]
		    ip = ip + 1
		}
		url[op-1] = EOS

		if (streq (inurl, url))
		    return (FALSE)

		return (TRUE)
	    }
	}

	return (FALSE)
end


# URL_BREAK -- Break the URL into components needed to make the netpath.

procedure url_break (url, protocol, host, port, path)

char	url[SZ_BUF]			#i url to parse
char	protocol[ARB]			#o URL protocol (only HTTP, for now)
char	host[ARB]			#o host name
int	port				#o server port (if specified, or 80)
char    path[ARB]			#o path part of URL, including args

int	i, nch, ip
int	ctoi()

begin
	port = 80				# set default port number

	# Pull out the protocol part of the URL.
	for (ip=1; url[ip] != ':'; ip = ip + 1)
	    protocol[ip] = url[ip]
	protocol[ip] = '\0'

	# Skip the "://" separator.
	while (url[ip] == ':' || url[ip] == '/')
	    ip = ip + 1

	# Get the host name.
	for (i=1; url[ip] != ':' && url[ip] != '/' && url[ip] != EOS; i=i+1) {
	    host[i] = url[ip]
	    ip = ip + 1
	}
	host[i] = '\0'

	if (url[ip] == EOS) {
	    call strcpy ("/", path, 2)
	    return
	}

	# Extract a port number of specified
	if (url[ip] == ':') {
	    ip = ip + 1
	    nch = ctoi (url, ip, port)
	}

	# Get the remaining path.
	for (i=1; url[ip] != EOS; i = i + 1) {
	    path[i] = url[ip]
	    ip = ip + 1
	}
	path[i] = '\0'
end


# URL_ACCESS -- Do an HTTP GET of a resource to the named file.

int procedure url_access (host, port, path, fname, reply)

char	host[ARB]				#i host name
int	port					#i server port number
char	path[ARB]				#i resource path
char	fname[ARB]				#i saved file path
pointer	reply					#i reply buffer

pointer	rep
int	in, out, nchars, totchars, retcode, clen, ip
char	buf[SZ_BUF], netpath[SZ_PATHNAME], request[SZ_BUF], hd[SZ_LINE]
bool	done

int	open(), access(), ndopen(), getline(), read(), strlen(), ctoi()
int	strncmp(), url_retcode()

begin
	# Connect to server on the given host.
	call sprintf (netpath, SZ_PATHNAME, "inet:%d:%s:%s")
	    call pargi (port)
	    call pargstr (host)
	    call pargstr ("text")

	iferr (in = ndopen (netpath, READ_WRITE)) {
	    call eprintf ("cannot access host '%s:%d'\n")
		call pargstr (host)
		call pargi (port)
	    return (- HTTP_NOTFOUND)
	}

	# Format the request header.
	call aclrc (request, SZ_BUF)
	call sprintf (request, SZ_BUF, "GET %s HTTP/1.0\n")
	    call pargstr (path)
	call strcat ("Accept: */*\n", request, SZ_BUF)
	call strcat ("User-Agent: IRAF/urlget\n", request, SZ_BUF)
	call strcat ("Host: ", request, SZ_BUF)
	call strcat ( host, request, SZ_BUF)
	call strcat ("\n", request, SZ_BUF)
	call strcat ("Connection: keep-alive\n\n", request, SZ_BUF)

	# Send the GET-url request to the server.
	nchars = strlen (request)
	call write (in, request, nchars)
	call flush (in)
	call fseti (in, F_CANCEL, OK)

	if (DBG_HDRS) {
	    call eprintf ("request [%d]:\n%s\n")
		call pargi (nchars)
		call pargstr (request)
	}

        # Read the reply. Read the HTTP header assuming it ends with a \n or
        # a \r\n. and then validate it will return the request correctly.
	done = false
	clen = -1
	call calloc (rep, SZ_BUF, TY_CHAR)
        repeat {
	    call aclrc (hd, SZ_LINE)
            nchars = getline (in, hd)
            if (nchars <= 0)
                break
	    call strcat (hd, Memc[rep], SZ_LINE)
	    if (strncmp (hd, "Content-Length:", 15) == 0) {
		ip = 16
		nchars = ctoi (hd, ip, clen)
	    }
        } until ((hd[1] == '\r' && hd[2] == '\n') || (hd[1] == '\n'))

	if (DBG_HDRS) {
	    call eprintf ("reply: %s\nclen = %d\n")
		call pargstr (Memc[rep])
		call pargi(clen)
	}

	# Make sure we have a valid file.
	retcode = url_retcode (Memc[rep])

	if (reply != NULL)
	    call strcpy (Memc[rep], Memc[reply], SZ_BUF)
	call mfree (rep, TY_CHAR)
	if (retcode != HTTP_OK)
	    return (- retcode)


	# Open the named output file.
	if (access (fname, 0, 0) == YES)
	    call syserrs (SYS_FCLOBBER, fname)
	iferr (out = open (fname, NEW_FILE, TEXT_FILE))
	    call syserrs (SYS_FOPEN, fname)

	# Now read the resource and save it to the named file.
	totchars = 0
	done = false
	repeat {
	    call aclrc (buf, SZ_BUF)
	    nchars = read (in, buf, SZ_BUF)
	    if (nchars > 0) {
		call write (out, buf, nchars)
		call flush (out)
		totchars = totchars + nchars
		done = false
	    } else
		done = true

	    if (clen > 0 && totchars >= clen)
		break
	} until (done)

	call close (in)				# clean up
	call close (out)

	return (totchars)			# return number of chars read
end


# URL_RETCODE -- Get the return code from the HTTP header reply.

int procedure url_retcode (reply)

char	reply[ARB]				#i reply string

int	ip, len, code, ctoi()

begin
	for (ip=1; !IS_WHITE(reply[ip]); ip=ip+1)
		;
	len = ctoi (reply, ip, code)

	return (code)
end


#  URL_ERRCODE - Convert between an HTTP return code and the equivalent
#  syserr() code value.

int procedure url_errcode (code)

int	code					#i http return code

begin
	# Note:  Not all error codes are implemented in syserr.  In this
	# case we just return the input code.

	switch (code) {
	case HTTP_OK:				# Success
	    ;
	case HTTP_CREATED:			# Created
	    ;
	case HTTP_ACCEPTED:			# Accepted
	    ;
	case HTTP_PARTIAL:			# Partial Information
	    ;
	case HTTP_NORESP:			# No Response
	    ;

	case HTTP_MOVED:			# Moved
	    return (SYS_URLREDIRECT);
	case HTTP_FOUND:			# Found
	    return (SYS_URLREDIRECT);
	case HTTP_SEEOTHER:			# See Other
	    return (SYS_URLREDIRECT);
	case HTTP_NOTMOD:			# Not Modified
	    ;

	case HTTP_BADREQ:			# Bad Request
	    return (SYS_URLBADREQUEST)
	case HTTP_UNAUTH:			# Unauthorized
	    ;
	case HTTP_PAYMENT:			# Payment Required
	    ;
	case HTTP_FORBIDDEN:			# Forbidden
	    return (SYS_URLFORBIDDEN)
	case HTTP_NOTFOUND:			# Not Found
	    return (SYS_URLNOTFOUND)

	case HTTP_INTERR:			# Internal Error
	    return (SYS_URLINTERROR)
	case HTTP_NOTIMP:			# Not Implemented
	    ;
	case HTTP_OVERLOAD:			# Service Temporarily Overloaded
	    ;
	case HTTP_GWTIMEOUT:			# Gateway Timeout
	    ;
	}

	return (code)
end
