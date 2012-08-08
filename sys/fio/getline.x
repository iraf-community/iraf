# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include <fio.h>

# GETLINE -- Get a line of text from a file.  If file buffer is empty and
# file is a text file, read line directly into the buffer supplied by the
# calling procedure.  If text is buffered, copy characters out of file buffer
# into the output buffer until the newline character is encountered.  Refill
# buffer as necessary.  Note IOP, ITOP are moved into local variables to
# optimize the loop.  The fd values must be updated before calling FILBUF
# and upon exit from the loop.

int procedure getline (fd, linebuf)

int	fd				# input file
char	linebuf[ARB]			# output line buffer (>= SZ_LINE)

bool	pushback
char	ch
pointer	ip, ip_top, op
int	maxch, status
int	filbuf(), and()
errchk	filbuf, filerr
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call filerr (FNAME(fp), SYS_FILENOTOPEN)

	pushback = (and (fflags[fd], FF_PUSHBACK) != 0)

	if (FTYPE(fp) == TEXT_FILE && iop[fd] == itop[fd] && !pushback) {
	    # Get next line from text file, initialize pointers.  In raw mode
	    # we only read one character at a time.

	    if (and (FF_RAW, fflags[fd]) == 0)
		maxch = SZ_LINE
	    else
		maxch = 1
	    call zcall4 (ZGETTX(fp), FCHAN(fp), linebuf, maxch, status)

	    if (status == ERR)
		call filerr (FNAME(fp), SYS_FREAD)
	    op = max (0, status+1)

	} else {
	    op = 1
	    ip = iop[fd]			# loop optimization stuff
	    ip_top = itop[fd]
	    if (ip < bufptr[fd])
		goto 10

	    while (op <= SZ_LINE) {
		if (ip >= ip_top) {
		    iop[fd] = ip
 10		    status = filbuf (fd)
		    ip = iop[fd]
		    ip_top = itop[fd]
		    if (status <= 0)
			break
		}
		
		ch = Memc[ip]
		linebuf[op] = ch
		ip = ip + 1
		op = op + 1

		if (ch == '\n')
		    break
	    }
	    iop[fd] = ip
	}

	if (op <= 1) {
	    FNCHARS(fp) = 0
	    return (EOF)
	} else {
	    FNCHARS(fp) = op - 1
	    linebuf[op] = EOS
	    return (op - 1)			# number of chars read
	}
end
