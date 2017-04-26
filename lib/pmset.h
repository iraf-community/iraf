# PMSET.H -- Pixel mask definitions (PMIO is layered upon IMIO and PLIO).

define	PM_MAXDIM	PL_MAXDIM
define	PM_MAXDEPTH	PL_MAXDEPTH
define	PM_UPDATE	PL_UPDATE

# PMIO parameter codes.
define	P_REFIM		P_PRIVATE1
define	P_MAPXY		P_PRIVATE2

# MIO parameter codes.
define	P_PMDES		1		# PMIO descriptor
define	P_IMDES		2		# IMIO descriptor
define	P_REGCOORDS	3		# mio_setrange region relative coords
define	P_PMCLOSE	4		# close mask at image close time

# IMPLMAP flags.
define	BOOLEAN_MASK	100B		# convert mask to boolean
define	INVERT_MASK	200B		# invert a mask (PIX_NOT)

# The following routines are identical in both the PMIO and PLIO packages.
define	pm_close	pl_close
define	pm_compare	pl_compare
define	pm_compress	pl_compress
define	pm_create	pl_create
define	pm_debug	pl_debug
define	pm_emptyline	pl_emptyline
define	pm_gsize	pl_gsize
define	pm_load		pl_load
define	pm_loadf	pl_loadf
define	pm_loadim	pl_loadim
define	pm_newcopy	pl_newcopy
define	pm_open		pl_open
define	pm_save		pl_save
define	pm_savef	pl_savef
define	pm_saveim	pl_saveim
define	pm_ssize	pl_ssize

# The following nested include is safe because the PLIO mkpkg will update the
# modify date of <pmset.h> if <plset.h> is modified.  Hence, only <pmset.h>
# need be referenced in dependency files lists in applications code.

include	<plset.h>
