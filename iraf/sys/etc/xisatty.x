# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<clset.h>
include	<fset.h>

# XISATTY -- Test if the given file is a terminal.

int procedure xisatty (fd)

int	fd			# file descriptor of candidate device
int	epa, epa_tt, epa_ty
extern	zgettt(), zgetty()
int	fstati(), clstati()

begin
	# If we are a connected subprocess, the referenced file is a standard
	# stream, and i/o has not been redirected, assume that the file behaves
	# as a terminal.

	if (clstati(CL_PRTYPE) == PR_CONNECTED)
	    if (fd == STDIN || fd == STDOUT || fd == STDERR)
		if (fstati (fd, F_REDIR) == NO)
		    return (YES)
		else
		    return (NO)

	# Otherwise, the use of the terminal driver tells us if the file is
	# open on a terminal device.

	epa = fstati (fd, F_DEVICE)
	call zlocpr (zgettt, epa_tt)
	call zlocpr (zgetty, epa_ty)

	if (epa == epa_tt || epa == epa_ty)
	    return (YES)
	else
	    return (NO)
end
