# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fset.h>
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
	daytime = t_daytime


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
#
# To test the ND driver, start up two copies of zzdebug.e, one running the
# server task and the other the client task.  Give the same value of "port"
# to both, and start the server.  It will pause waiting for a client.  Then
# start a test sequence in the client setting "nmsg" to the number of
# messages to be exchanged.  The client will send nmsg messages of various
# sizes to the server and echo on the stdout the response returned by the
# server.

procedure t_server()

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

procedure t_client()

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
