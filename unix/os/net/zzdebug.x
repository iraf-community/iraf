# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	rexec	= t_rexec,
	rtype	= t_rtype,
	rread	= t_rread

define	SZ_BUF		4096


# REXEC -- Execute a command on a remote node and print the resultant output on
# the standard output.  Used to test the kernel server driver.

procedure t_rexec()

char	server[SZ_LINE]
char	buf[SZ_BUF]
int	chan, nbytes, status

begin
	call clgstr ("server", server, SZ_LINE)
	call strpak (server, server, SZ_LINE)

	call zopnks (server, READ_WRITE, chan)
	if (chan == ERR)
	    call error (1, "cannot connect to remote server process")

	repeat {
	    call zardks (chan, buf, SZ_BUF, 0)
	    call zawtks (chan, nbytes)

	    if (nbytes > 0) {
		call chrupk (buf, 1, buf, 1, nbytes)
		call write (STDOUT, buf, nbytes)
		call flush (STDOUT)
	    }
	} until (nbytes <= 0)

	call zclsks (chan, status)
	if (status == ERR)
	    call error (1, "error disconnecting server process")
end


# RTYPE -- Type a text file possibly resident on a remote node.

procedure t_rtype()

char	fname[SZ_FNAME]
char	lbuf[SZ_LINE]
int	fd
int	open(), getline()

begin
	call clgstr ("file", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)

	while (getline (fd, lbuf) != EOF) {
	    call putline (STDOUT, lbuf)
	    call flush (STDOUT)
	}

	call close (fd)
end


# RREAD -- Read a binary file.

procedure t_rread()

char	fname[SZ_FNAME]
char	dbuf[SZ_BUF]
int	fd
long	nchars, totchars
int	open(), read()

begin
	call clgstr ("file", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, BINARY_FILE)

	totchars = 0

	repeat {
	    nchars = read (fd, dbuf, SZ_BUF)
	    if (nchars > 0)
		totchars = totchars + nchars
	} until (nchars == EOF)

	call close (fd)

	call printf ("read %d chars\n")
	    call pargi (totchars)
end
