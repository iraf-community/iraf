# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fset.h>
include	<fio.h>

task	mpp	= t_mpp,
	unget	= t_unget,
	pbb	= t_pbb,
	fnl	= t_fnl,
	txo	= t_txo,
	bfap	= t_bfappend,
	spool	= t_spool,
	many	= t_many


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

procedure t_txo

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

procedure t_spool

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

procedure t_many

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
