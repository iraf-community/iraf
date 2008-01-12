# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

define	SWAPI		{tempi=$1;$1=$2;$2=tempi}
define	SWAPL		{templ=$1;$1=$2;$2=templ}
define	SWAPP		{tempp=$1;$1=$2;$2=tempp}

# FSWAPFD -- Swap the file descriptors of two open files.  All i/o to file
# fd1 is redirected to fd2 and vice versa, until the swap is reversed.
# We are used by FREDIR to temporarily redirect i/o (normally to one of the
# standard streams) to a special file.  If CLOSE is called to close a
# redirected file, we are called to unswap the two streams and then the
# redirection file is closed.  All the i/o pointers, buffer pointers, and
# so on of the original stream are restored to exactly the condition they
# were in before (unless i/o has occurred on the other file during the
# interim).

procedure fswapfd (fd1, fd2)

int	fd1, fd2		# file descriptors to be swapped.
int	tempi
long	templ
pointer	tempp
include	<fio.com>

begin
 	SWAPL (boffset[fd1], boffset[fd2])
 	SWAPP (bufptr[fd1], bufptr[fd2])
 	SWAPP (buftop[fd1], buftop[fd2])
 	SWAPP (iop[fd1], iop[fd2])
 	SWAPP (itop[fd1], itop[fd2])
 	SWAPP (otop[fd1], otop[fd2])
 	SWAPP (fiodes[fd1], fiodes[fd2])
 	SWAPI (redir_fd[fd1], redir_fd[fd2])
end
