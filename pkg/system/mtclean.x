# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MTCLEAN -- Clean the magtape .lok file area, i.e., delete any old .lok files.
# These files no longer serve any device locking purpose, rather, they are
# used by the magtape system to store knowledge of the tape position when
# the drive is not being accessed.  The mtclean task is called during CL
# startup to delete any old .lok files to keep these from being erroneously
# used to indicate the tape position.  In normal use the .lok files are
# created when a tape is allocated and deleted when the tape is deallocated,
# but the files can be left behind if the CL is killed without doing a
# deallocate.

procedure t_mtclean()

int	out
bool	clgetb()
int	clgeti(), btoi()

begin
	out = NULL
	if (clgetb ("verbose"))
	    out = STDOUT

	call mtclean (btoi(clgetb("all")), clgeti("stale"), out)
end
