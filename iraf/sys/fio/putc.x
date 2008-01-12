# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# PUTC -- Put a character to a file.

procedure putc (fd, ch)

int	fd			# output file
char	ch			# character to be output

int	and()
errchk	flsbuf
include	<fio.com>

begin
	if (iop[fd] < bufptr[fd] || iop[fd] >= otop[fd])
	    call flsbuf (fd, 0)

	Memc[iop[fd]] = ch
	iop[fd] = iop[fd] + 1

	if (ch == '\n')				# end of line of text?
	    if (and (FF_FLUSH, fflags[fd]) != 0)
		call flsbuf (fd, 0)
end


# PUTCHAR -- Put a character to the standard output.

procedure putchar (ch)

char	ch			# character to be output

begin
	call putc (STDOUT, ch)
end
