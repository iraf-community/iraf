# CLIO.H -- CLIO definitions (private to the CLIO interface).

define	MAX_PSEUDOFILES		10		# is this still used?
define	MAX_POSARGS		20		# max positional arguments
define	MAX_PSETS		20		# max psets used by a task
define	SZ_PSETNAMEBUF		200		# storage for pset names
define	SZ_PNAME		64		# storage for "pset.param"

# The following must agree with the definitions in cl$task.h.
define	IPCOUT		"IPC$IPCIO-OUT"
define	IPCDONEMSG	"# IPC$IPCIO-FINISHED\n"
