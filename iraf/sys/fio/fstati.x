# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fset.h>
include	<error.h>
include	<fio.h>

# FSTATI -- Get information on the status and characteristics of an open
# file.  Returns an integer value.  See also FSTATL and FSTATS (for long
# integer and string status values).

int procedure fstati (fd, what)

int	fd				#I file descriptor
int	what				#I parameter to be returned

int	val
long	lval
long	fstatl()

begin
	lval = fstatl(fd,what)
	val = lval
	return (val)
end
