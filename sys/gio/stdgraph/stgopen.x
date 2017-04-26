# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	"stdgraph.h"

# STG_OPEN -- Install the STDGRAPH kernel as a graphics kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  If a driver does not implement a particular
# instruction the table entry for that procedure may be set to zero, causing
# the interpreter to ignore the instruction.

procedure stg_open (devname, dd, in, out, xres, yres, hardchar)

char	devname[ARB]		# if nonnull, force output to device
int	dd[ARB]			# device table to be initialized
int	in			# input file
int	out			# output file
int	xres			# number of resolved pixels in X
int	yres			# number of resolved pixels in Y
int	hardchar		# use hardware character generator

bool	first_time
pointer	sp, devns
int	len_devname
int	locpr(), strlen()

extern	stg_openws(), stg_closews(), stg_clear(), stg_cancel()
extern	stg_flush(), stg_polyline(), stg_polymarker(), stg_text()
extern	stg_fillarea(), stg_putcellarray(), stg_setcursor(), stg_plset()
extern	stg_pmset(), stg_txset(), stg_faset(), stg_getcursor()
extern	stg_getcellarray(), stg_escape()
extern	stg_reactivatews(), stg_deactivatews()
include	"stdgraph.com"
data	first_time /true/

begin
	call smark (sp)
	call salloc (devns, SZ_FNAME, TY_SHORT)

	if (first_time) {
	    g_nopen = 0
	    g_sg = NULL
	    g_tty = NULL
	    g_term = NULL
	    g_pbtty = NULL
	    g_cursor = 0
	    first_time = false
	}

	g_in   = in
	g_out  = out
	g_xres = xres
	g_yres = yres
	g_nopen = g_nopen + 1
	g_stream = STDGRAPH
	g_hardchar = hardchar
	g_active = NO
	g_enable = NO
	g_message = NO
	g_msgbuf = NULL
	g_msgbuflen = 0
	g_msglen = 0
	call strcpy (devname, g_device, SZ_GDEVICE)

	# Install the device driver.
	dd[GKI_OPENWS]		= locpr (stg_openws)
	dd[GKI_CLOSEWS]		= locpr (stg_closews)
	dd[GKI_REACTIVATEWS]	= locpr (stg_reactivatews)
	dd[GKI_DEACTIVATEWS]	= locpr (stg_deactivatews)
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (stg_clear)
	dd[GKI_CANCEL]		= locpr (stg_cancel)
	dd[GKI_FLUSH]		= locpr (stg_flush)
	dd[GKI_POLYLINE]	= locpr (stg_polyline)
	dd[GKI_POLYMARKER]	= locpr (stg_polymarker)
	dd[GKI_TEXT]		= locpr (stg_text)
	dd[GKI_FILLAREA]	= locpr (stg_fillarea)
	dd[GKI_PUTCELLARRAY]	= locpr (stg_putcellarray)
	dd[GKI_SETCURSOR]	= locpr (stg_setcursor)
	dd[GKI_PLSET]		= locpr (stg_plset)
	dd[GKI_PMSET]		= locpr (stg_pmset)
	dd[GKI_TXSET]		= locpr (stg_txset)
	dd[GKI_FASET]		= locpr (stg_faset)
	dd[GKI_GETCURSOR]	= locpr (stg_getcursor)
	dd[GKI_GETCELLARRAY]	= locpr (stg_getcellarray)
	dd[GKI_ESCAPE]		= locpr (stg_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0

	# If a device was named open the workstation as well.  This is
	# necessary to permit processing of metacode files which do not
	# contain the open workstation instruction.

	len_devname = strlen (devname)
	if (len_devname > 0) {
	    call achtcs (devname, Mems[devns], len_devname)
	    call stg_openws (Mems[devns], len_devname, NEW_FILE)
	}

	call sfree (sp)
end
