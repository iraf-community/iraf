# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<time.h>
include	<mach.h>
include	<fset.h>
include	<knet.h>

# BENCH -- IRAF benchmark tasks.

task	ptime		= t_ptime,
	getpar		= t_getpar,
	wipc		= t_wipc,
	rbin		= t_rbin,
	wbin		= t_wbin,
	rrbin		= t_rrbin,
	rtext		= t_rtext,
	wtext		= t_wtext

define	SZ_RBBUF	16384
define	SZ_BBUF		4096
define	SZ_TBUF		64


# PTIME -- Print the current clock time.  This is essentially a no-op task,
# used to test process connect/disconnect, IPC, and task startup/shutdown
# overhead.

procedure t_ptime()

char	tbuf[SZ_TIME]
long	clktime()

begin
	call cnvtime (clktime (long(0)), tbuf, SZ_TIME)
	call printf ("%s\n")
	    call pargstr (tbuf)
end


# GETPAR -- Get a parameter from the CL repeatedly.  Used to test the IPC
# turnaround time.

procedure t_getpar()

int	niter, i
char	paramval[SZ_FNAME]
int	clgeti()

begin
	niter = clgeti ("niter")
	do i = 1, niter
	    call clgstr ("cl.version", paramval, SZ_FNAME)
end


# WIPC -- Write to IPC (tests IPC bandwidth).

procedure t_wipc()

int	fd, i
char	bbuf[SZ_BBUF]
long	n, filesize, clgetl()

begin
	fd = STDOUT
	filesize = clgetl ("filesize") / SZB_CHAR

	do i = 1, SZ_BBUF
	    bbuf[i] = mod (i-1, 128) + 1

	for (n=0;  n < filesize;  n = n + SZ_BBUF)
	    call write (fd, bbuf, SZ_BBUF)

	call eprintf ("wrote %d bytes\n")
	    call pargl (n * SZB_CHAR)
end


# RBIN -- Read from a binary file.

procedure t_rbin()

long	totchars
char	fname[SZ_FNAME]
char	bbuf[SZ_BBUF]
int	fd, open(), read()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, BINARY_FILE)
	call fseti (fd, F_ADVICE, SEQUENTIAL)
	totchars = 0

	while (read (fd, bbuf, SZ_BBUF) == SZ_BBUF)
	    totchars = totchars + SZ_BBUF

	call close (fd)
	call printf ("read %d bytes\n")
	    call pargl (totchars * SZB_CHAR)
end


# WBIN -- Write to a binary file.

procedure t_wbin()

char	fname[SZ_FNAME]
char	bbuf[SZ_BBUF]
int	fd, i, open()
long	n, filesize, clgetl()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	iferr (call delete (fname))
	    ;
	fd = open (fname, APPEND, BINARY_FILE)
	call fseti (fd, F_ADVICE, SEQUENTIAL)
	filesize = clgetl ("filesize") / SZB_CHAR

	do i = 1, SZ_BBUF
	    bbuf[i] = mod (i-1, 128) + 1

	for (n=0;  n < filesize;  n = n + SZ_BBUF)
	    call write (fd, bbuf, SZ_BBUF)

	call close (fd)
	call printf ("wrote %d bytes\n")
	    call pargl (n * SZB_CHAR)
end


# RTEXT -- Read from a text file.

procedure t_rtext()

long	totchars
char	fname[SZ_FNAME]
char	tbuf[SZ_TBUF]
int	fd, nchars, nlines
int	open(), getline()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)
	totchars = 0
	nlines = 0

	repeat {
	    nchars = getline (fd, tbuf)
	    if (nchars > 0) {
		totchars = totchars + nchars
		nlines = nlines + 1
	    }
	} until (nchars == EOF)

	call close (fd)
	call printf ("read %d chars, %d lines\n")
	    call pargl (totchars)
	    call pargi (nlines)
end


# WTEXT -- Write to a text file.

procedure t_wtext()

char	fname[SZ_FNAME]
char	tbuf[SZ_TBUF]
int	fd, op, open()
long	n, nlines, filesize, clgetl()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	iferr (call delete (fname))
	    ;
	fd = open (fname, APPEND, TEXT_FILE)
	filesize = clgetl ("filesize")
	nlines = 0

	for (op=1;  op < SZ_TBUF;  op=op+1)
	    tbuf[op] = '.'

	tbuf[op] = '\n'
	op = op + 1
	tbuf[op] = EOS

	for (n=0;  n < filesize;  n = n + SZ_TBUF) {
	    call putline (fd, tbuf)
	    nlines = nlines + 1
	}

	call close (fd)
	call printf ("wrote %d chars, %d lines\n")
	    call pargl (n)
	    call pargi (nlines)
end


# RRBIN -- Raw (unbuffered) read from a binary file.

procedure t_rrbin()

char	fname[SZ_FNAME]
char	bbuf[SZ_RBBUF]
long	totchars, offset, buflen
int	fd, chan, status
int	open(), fstati()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, BINARY_FILE)
	chan = fstati (fd, F_CHANNEL)

	buflen   = SZ_RBBUF * SZB_CHAR
	totchars = 0
	offset   = 1
	status   = 0

	repeat {
	    totchars = totchars + (status / SZB_CHAR)
	    call zardbf (chan, bbuf, buflen, offset)
	    offset = offset + buflen
	    call zawtbf (chan, status)
	} until (status <= 0)

	call close (fd)
	call printf ("read %d bytes\n")
	    call pargl (totchars * SZB_CHAR)
end
