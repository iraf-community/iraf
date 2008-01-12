# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"gkt.h"

# GKT_OPEN -- Install the nspp kernel as a graphics kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  If a driver does not implement a particular
# instruction the table entry for that procedure may be set to zero, causing
# the interpreter to ignore the instruction.

procedure gkt_open (devname, dd)

char	devname[ARB]		# nonnull for forced output to a device
int	dd[ARB]			# device table to be initialized

pointer	sp, devns
int	len_devname
int	locpr(), strlen()
extern	gkt_openws(), gkt_closews(), gkt_clear(), gkt_cancel()
extern	gkt_flush(), gkt_polyline(), gkt_polymarker(), gkt_text()
extern	gkt_fillarea(), gkt_putcellarray(), gkt_plset()
extern	gkt_pmset(), gkt_txset(), gkt_faset()
extern	gkt_escape()
include	"gkt.com"

begin
	call smark (sp)
	call salloc (devns, SZ_FNAME, TY_SHORT)

	# Flag first pass.  Save forced device name in common for OPENWS.
	# Zero the frame and instruction counters.

	g_kt = NULL
	g_nframes = 0
	g_ndraw = 0
	call strcpy (devname, g_device, SZ_GDEVICE)

	# Install the device driver.

	dd[GKI_OPENWS]		= locpr (gkt_openws)
	dd[GKI_CLOSEWS]		= locpr (gkt_closews)
	dd[GKI_DEACTIVATEWS]	= 0
	dd[GKI_REACTIVATEWS]	= 0
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (gkt_clear)
	dd[GKI_CANCEL]		= locpr (gkt_cancel)
	dd[GKI_FLUSH]		= locpr (gkt_flush)
	dd[GKI_POLYLINE]	= locpr (gkt_polyline)
	dd[GKI_POLYMARKER]	= locpr (gkt_polymarker)
	dd[GKI_TEXT]		= locpr (gkt_text)
	dd[GKI_FILLAREA]	= locpr (gkt_fillarea)
	dd[GKI_PUTCELLARRAY]	= locpr (gkt_putcellarray)
	dd[GKI_SETCURSOR]	= 0
	dd[GKI_PLSET]		= locpr (gkt_plset)
	dd[GKI_PMSET]		= locpr (gkt_pmset)
	dd[GKI_TXSET]		= locpr (gkt_txset)
	dd[GKI_FASET]		= locpr (gkt_faset)
	dd[GKI_GETCURSOR]	= 0
	dd[GKI_GETCELLARRAY]	= 0
	dd[GKI_ESCAPE]		= locpr (gkt_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0

	# If a device was named open the workstation as well.  This is
	# necessary to permit processing of metacode files which do not
	# contain the open workstation instruction.

	len_devname = strlen (devname)
	if (len_devname > 0) {
	    call achtcs (devname, Mems[devns], len_devname)
	    call gkt_openws (Mems[devns], len_devname, NEW_FILE)
	}

	call sfree (sp)
end
