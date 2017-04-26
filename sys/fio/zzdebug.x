# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>
include	<fset.h>
include	<finfo.h>
include	<fio.h>


# ZZDEBUG -- Debug tasks for the FIO (file i/o) interface.

task	mpp	= t_mpp,
	unget	= t_unget,
	pbb	= t_pbb,
	fnl	= t_fnl,
	txo	= t_txo,
	bfap	= t_bfappend,
	spool	= t_spool,
	many	= t_many,
	server	= t_server,
	client	= t_client,
	oserver	= t_old_server,
	oclient	= t_old_client,
	daytime = t_daytime,
	http	= t_http,
	utime	= t_utime,
	symlink	= t_symlink,
	unlink	= t_unlink


define 	SZ_BUF		2048


# MPP -- Test macro pushback.

procedure t_mpp()

int	fd
char	ch
char	getc()

begin
	fd = STDIN

	while (getc (fd, ch) != EOF) {
	    if (ch == '%') {
		ch = '$'
		call ungetc (fd, ch)
	    } else if (ch == '\n') {
		call putchar (ch)
		call flush (STDOUT)
	    } else if (ch == '^') {
		call ungetline (fd, "carat")
	    } else if (ch == '&') {
		call ungetline (fd, "amper%sand")
	    } else
		call putchar (ch)
	}
end


# UNGET-- Test ungetline.

procedure t_unget()

char	lbuf[SZ_LINE]
int	getline()

begin
	while (getline (STDIN, lbuf) != EOF) {
	    if (lbuf[1] == '.') {
		call ungetline (STDIN, lbuf[2])
		call ungetline (STDIN, "pbb\n")
	    } else {
		call putline (STDOUT, lbuf)
		call flush (STDOUT)
	    }
	    call fdebug (STDOUT, STDIN, STDIN)
	}
end


# PBB -- Test multilevel pushback.

procedure t_pbb()

char	lbuf[SZ_LINE]
int	fd
int	getline()
include	<fio.com>

begin
	fd = STDIN
	fp = fiodes[fd]

	call fdebug (STDERR, fd, fd)
	call ungetline (fd, "aaa\n")
	call fdebug (STDERR, fd, fd)
	call ungetline (fd, "bbb\n")
	call fdebug (STDERR, fd, fd)
	call ungetline (fd, "ccc\n")
	call fdebug (STDERR, fd, fd)
	call ungetline (fd, "ddd\n")
	call fdebug (STDERR, fd, fd)

	call eprintf ("pbb='%s'\n\n"); call pargstr (FPBBUF(fp))

	while (getline (fd, lbuf) != EOF) {
	    call putline (STDERR, lbuf)
	    call fdebug (STDERR, fd, fd)
	}
end


# FNL -- Test filename template expansion.

procedure t_fnl()

char	fname[SZ_FNAME]
int	list, clpopns(), clgfil(), clplen()

begin
	list = clpopns ("files")
	call printf ("nfiles = %d\n"); call pargi (clplen(list))

	while (clgfil (list, fname, SZ_FNAME) != EOF) {
	    call printf ("%s\n"); call pargstr (fname)
	}

	call clpcls (list)
end


# TXO -- Test the mixing of PUTC, PUTLINE, and WRITE calls to a text file.

procedure t_txo()

int	fd, i, j
int	open()

begin
	fd = open ("junk", NEW_FILE, TEXT_FILE)

	do i = 1, 5 {
	    do j = 1, 5
		call putci (fd, 'a' + i - 1)
	    call putline (fd, "12345")
	    call write (fd, "_6789[] ", 8)
	}

	call close (fd)
end


# BFAP -- Test appending to a binary file.  Should create the file and then
# add an unpacked line of text in each successive call.

procedure t_bfappend()

int	fd
int	open(), strlen()
string	text "1234567890\n"

begin
	fd = open ("_bf", APPEND, BINARY_FILE)
	call fdebug (STDERR, fd, fd)
	call write (fd, text, strlen(text))
	call fdebug (STDERR, fd, fd)
	call close (fd)
end


# SPOOL -- Test the spoolfile file type.

procedure t_spool()

int	fd, i, j, n
int	open(), read()

begin
	fd = open ("spool", READ_WRITE, SPOOL_FILE)
	call fdebug (STDERR, fd, fd)

	call eprintf ("write test data\n")
	do i = 1, 10000
	    call write (fd, i, SZ_INT)

	call fdebug (STDERR, fd, fd)
	call eprintf ("rewind file:\n")
	call seek (fd, BOFL)
	call fdebug (STDERR, fd, fd)

	call eprintf ("read back test data\n")
	do i = 1, 10000 {
	    n = read (fd, j, SZ_INT)
	    if (n < SZ_INT || i != j) {
		call eprintf ("read failure at word %d=%d of 10000, stat=%d\n")
		    call pargi (i)
		    call pargi (j)
		    call pargi (n)
	    }
	}

	call eprintf ("test successful\n")
	call close (fd)
end


# MANY -- Test what happens when we try to open too many files.

procedure t_many()

char	fname[SZ_FNAME]
int	list, nfiles, fd
int	open(), fntopn(), fntgfn()

begin
	list = fntopn ("fio$*.x")

	for (nfiles=1;  fntgfn(list,fname,SZ_FNAME) != EOF;  nfiles=nfiles+1) {
	    fd = open (fname, READ_ONLY, TEXT_FILE)
	    call eprintf ("%d %s\n")
		call pargi (nfiles)
		call pargstr (fname)
	}

	call fntcls (list)
end


# SERVER -- Simple ND server for testing the network driver.  This server
# listens on the specified port and waits for a client connection, then
# returns a status message for every message received from the client,
# shutting down when the client exits.

procedure t_server()

char	buf[SZ_BUF]
int	fdi, fdo, nb, i
int	ndopen(), read(), reopen()

begin
	do i = 1, 5 {
	    call printf ("server waiting for connection\n")
	    fdi = ndopen ("unix:/tmp/nd:text", NEW_FILE)
	    fdo = reopen (fdi, READ_WRITE)

	    call printf ("fdin = %d  fdout = %d\n")
		call pargi (fdi) ; call pargi (fdo)

	    call fdebug (STDOUT, fdi, fdo)
	    call flush (STDOUT)

	    repeat {
		nb = read (fdi, buf, SZ_BUF)
		if (nb > 0) {
		    call fprintf (STDOUT, "read %d bytes from client\n")
			call pargi (nb)
		    call flush (STDOUT)

		    call fprintf (fdo, "read %d bytes from client\n")
			call pargi (nb)
		    call flush (fdo)
		}
	    } until (nb <= 0)

	    call printf ("client has disconnected\n")
	    call close (fdi)
	    call close (fdo)
	}
end


# CLIENT -- Connect to the server on the given port and send a number of
# test messages, then close the connection and exit.

procedure t_client()

char	buf[SZ_BUF]
int	fdi, fdo, n, i, msglen
int	msgsize[8]

int	ndopen(), read(), reopen()
data	msgsize /64, 128, 256, 134, 781, 3, 19, 1544/

begin
	fdi = ndopen ("unix:/tmp/nd:text", READ_WRITE)
	fdo = reopen (fdi, READ_WRITE)

	call printf ("fdin = %d  fdout = %d\n")
	    call pargi (fdi) ; call pargi (fdo)

	call fdebug (STDOUT, fdi, fdo)
	call flush (STDOUT)

	for (i=1;  i <= 5;  i=i+1) {
	    msglen = msgsize[mod(i,8)+1)
	    call printf ("send %d chars to server\n")
		call pargi (msglen)

	    call write (fdo, buf, msglen)
	    call flush (fdo)

	    n = read (fdi, buf, SZ_BUF)
	    if (n > 0) {
		buf[n+1] = EOS
		call printf ("read %d bytes from server\n")
		    call pargi (n)
		call printf ("server: %s\n")
		    call pargstr (buf)
	    } else {
		call printf ("server has disconnected\n")
		break
	    }
	    call flush (STDOUT)
	}

	call close (fdi)
	call close (fdo)
end


# SERVER -- Simple ND server for testing the network driver.  This server
# listens on the specified port and waits for a client connection, then
# returns a status message for every message received from the client,
# shutting down when the client exits.
#
# To test the ND driver, start up two copies of zzdebug.e, one running the
# server task and the other the client task.  Give the same value of "port"
# to both, and start the server.  It will pause waiting for a client.  Then
# start a test sequence in the client setting "nmsg" to the number of
# messages to be exchanged.  The client will send nmsg messages of various
# sizes to the server and echo on the stdout the response returned by the
# server.
#
# NOTE - this is the original version, before adding support for "reopen"
# to have two fully streaming file descriptors per connection.

procedure t_old_server()

char	port[SZ_LINE]
char	buf[SZ_BUF]
int	fd, sum, n, maxconn, i
int	ndopen(), read(), checksum(), clgeti()

begin
	call clgstr ("port", port, SZ_LINE)
	maxconn = clgeti ("maxconn")
	if (maxconn <= 0)
	    maxconn = 9999

	do i = 1, maxconn {
	    call printf ("server waiting for connection\n")
	    fd = ndopen (port, NEW_FILE)

	    repeat {
		call fseti (fd, F_CANCEL, YES)
		n = read (fd, buf, SZ_BUF)
		if (n > 0) {
		    call fseti (fd, F_CANCEL, YES)
		    sum = checksum (buf, n)
		    call fprintf (fd, "read %d bytes from client, sum=%x")
			call pargi (n)
			call pargi (sum)
		    call flush (fd)
		}
	    } until (n <= 0)

	    call printf ("client has disconnected\n")
	    call close (fd)
	}
end


# CLIENT -- Connect to the server on the given port and send a number of
# test messages, then close the connection and exit.
#
# NOTE - this is the original version, before adding support for "reopen"
# to have two fully streaming file descriptors per connection.

procedure t_old_client()

char	buf[SZ_BUF]
char	port[SZ_LINE]
int	fd, nmsg, n, i, msglen
int	msgsize[8]

int	ndopen(), read(), clgeti(), checksum()
data	msgsize /64, 128, 256, 134, 781, 3, 19, 1544/

begin
	call clgstr ("port", port, SZ_LINE)
	nmsg = clgeti ("nmsg")

	fd = ndopen (port, READ_WRITE)

	for (i=1;  i <= nmsg;  i=i+1) {
	    msglen = msgsize[mod(i,8)+1)
	    call printf ("send %d chars to server, sum=%x\n")
		call pargi (msglen)
		call pargi (checksum (buf, msglen))

	    call fseti (fd, F_CANCEL, YES)
	    call write (fd, buf, msglen)
	    call flush (fd)

	    call fseti (fd, F_CANCEL, YES)
	    n = read (fd, buf, SZ_BUF)
	    if (n > 0) {
		buf[n+1] = EOS
		call printf ("server: %s\n")
		    call pargstr (buf)
	    } else {
		call printf ("server has disconnected\n")
		break
	    }
	    call flush (STDOUT)
	}

	call close (fd)
end


# DAYTIME -- Connect to the daytime service on the local host and print
# out what it returns.

procedure t_daytime()

int	fd, nchars, ip
char	hostname[SZ_FNAME]
char	line[SZ_LINE], netpath[SZ_LINE]
int	ndopen(), read(), strlen()

begin
	# Open the daytime service on the named host or the local host.
	call clgstr ("host", hostname, SZ_FNAME)
	if (strlen(hostname) > 0) {
	    call sprintf (netpath, SZ_LINE, "inet:daytime:%s")
		call pargstr (hostname)
	    iferr (fd = ndopen (netpath, READ_WRITE)) {
		call printf ("cannot access host\n")
		return
	    }
	} else {
	    iferr (fd = ndopen("inet:daytime",READ_WRITE))
		call printf("fail 1\n")
	    iferr (fd = ndopen("inet:daytime:localhost",READ_WRITE))
		call printf("fail 2\n")
	}

	# Read and print the daytime text.
	call fseti (fd, F_CANCEL, OK)
	nchars = read (fd, line, SZ_LINE)
	if (nchars > 0) {
	    call strupk (line, line, SZ_LINE)
	    for (ip=1;  line[ip] != EOS;  ip=ip+1)
		if (line[ip] == '\n') {
		    line[ip] = EOS
		    break
		}
	    call printf ("%s\n")
		call pargstr (line)
	}

	call close (fd)
end


# HTTP -- Connect to a HTTP server on the given host, read a URL and print
# what it returns.

procedure t_http()

bool	done
int	fd, nchars, lastch
char	hostname[SZ_FNAME], buf[SZ_BUF]
char	netpath[SZ_LINE], path[SZ_LINE]
int	ndopen(), read()

begin
	# Connect to HTTP server (default port 80) on the given host.
	call clgstr ("host", hostname, SZ_FNAME)
	call sprintf (netpath, SZ_LINE, "inet:80:%s:text")
	    call pargstr (hostname)
	iferr (fd = ndopen (netpath, READ_WRITE)) {
	    call printf ("cannot access host\n")
	    return
	}

	# Get the URL/URI (file pathname) to be read.
	call clgstr ("path", path, SZ_LINE)

	# Send the get-url request to the server.
	call fprintf (fd, "GET %s HTTP/1.0\n\n")
	    call pargstr (path)
	call flush (fd)

	# Read and print the given URL.  The returned text consists of the
	# HTTP protocol header, a blank line, then the document text.
	# Since this is a debug routine we output the protocol header as
	# well as the document, but a real program would probably strip
	# the header since it is not part of the document data.

	repeat {
	    call fseti (fd, F_CANCEL, OK)
	    nchars = read (fd, buf, SZ_BUF)
	    if (nchars > 0) {
		buf[nchars+1] = EOS
		call putline (STDOUT, buf)
		lastch = buf[nchars]
		done = false
	    } else
		done = true
	} until (done)

	if (lastch != '\n')
	    call putline (STDOUT, "\n")

	call close (fd)
end


# CHECKSUM -- Compute the checksum of a data buffer.

int procedure checksum (buf, nchars)

char	buf[ARB]		#I input buffer
int 	nchars			#I number of chars

int	sum, i

begin
	sum = 0
	do i = 1, nchars {
	    if (and (sum, 1) != 0)
		sum = sum / 2 + 8000X
	    else
		sum = sum / 2
	    sum = sum + buf[i]
	    sum = and (sum, 0FFFFX)
	}

	return (sum)
end


# UTIME -- Test file modify time updates.

procedure t_utime ()

char	fname[SZ_LINE]
int	offset
long    fi[LEN_FINFO]

int	futime(), finfo(), clgeti()

begin
	# Get parameters.
	call clgstr ("fname", fname, SZ_LINE)
	offset = clgeti ("offset")

	# Get initial file times.
        if (finfo (fname, fi) == ERR) 
            call syserrs (SYS_FOPEN, fname)
	call printf ("Initial times:  atime = %d  mtime = %d\n")
	    call pargl (FI_ATIME(fi))
	    call pargl (FI_MTIME(fi))


	# Update the time by the offset.
	if (futime (fname, FI_ATIME(fi)+offset, FI_MTIME(fi)+offset) == ERR)
	    call error (0, "Fatal futime() error")
	
	# Get modified file times.
        if (finfo (fname, fi) == ERR) 
            call syserrs (SYS_FOPEN, fname)
	call printf ("Mofified times: atime = %d  mtime = %d\n")
	    call pargl (FI_ATIME(fi))
	    call pargl (FI_MTIME(fi))


	# Test the NULL arguments, output shouldn't change.
	if (futime (fname, NULL, FI_MTIME(fi)) == ERR)
	    call error (0, "Fatal futime() error")
	
	# Get modified file times.
        if (finfo (fname, fi) == ERR) 
            call syserrs (SYS_FOPEN, fname)
	call printf ("NULL test time: atime = %d  mtime = %d\n")
	    call pargl (FI_ATIME(fi))
	    call pargl (FI_MTIME(fi))
end


# SYMLINK -- Create a symlink.

procedure t_symlink ()

char	link[SZ_PATHNAME], target[SZ_PATHNAME]
int 	status

int	sum, i

begin
	call clgstr ("link", link, SZ_PATHNAME)
	call clgstr ("target", target, SZ_PATHNAME)

	call fsymlink (link, target)
end


# UNLINK -- Remove a symlink.

procedure t_unlink ()

char	link[SZ_PATHNAME], target[SZ_PATHNAME]
int 	status

int	sum, i

begin
	call clgstr ("link", link, SZ_PATHNAME)
	call funlink (link)
end
