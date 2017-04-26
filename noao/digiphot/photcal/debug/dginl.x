include	"debug.h"

# DG_INLDUMP -- Dump the NLFIT and INLFIT structures into a file.

procedure dg_inldump (in, nl)

pointer	in		# INLFIT descriptor
pointer	nl		# NLFIT descriptor

int	fd

int	open()

begin
	# Open the dump file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Put in the time stamp.
	call dg_ptime (fd, "dg_inldump")

	# Dump the NLFIT structure.
	call nl_dumpr (fd, nl)

	# Dump the INLFIT structure.
	call in_dumpr (fd, in)

	# Close the dump file.
	call close (fd)
end
