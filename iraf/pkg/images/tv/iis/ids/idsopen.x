# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"../lib/ids.h"

# IDS_OPEN -- Install the image kernel as a kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  If a driver does not implement a particular
# instruction the table entry for that procedure may be set to zero, causing
# the interpreter to ignore the instruction.

procedure ids_open (devname, dd)

char	devname[ARB]		# nonnull for forced output to device
int	dd[ARB]			# device table to be initialized

int	locpr()
extern	ids_openws(), ids_closews(), ids_clear(), ids_cancel()
extern	ids_flush(), ids_polyline(), ids_polymarker(), ids_text()
extern	ids_fillarea(), ids_putcellarray(), ids_plset()
extern	ids_pmset(), ids_txset(), ids_faset()
extern	ids_escape()
extern	ids_setcursor(), ids_getcursor(), ids_getcellarray()

include	"../lib/ids.com"

begin
	# Flag first pass.  Save forced device name in common for OPENWS.

	i_kt = NULL
	call strcpy (devname, i_device, SZ_IDEVICE)

	# Install the device driver.
	dd[GKI_OPENWS]		= locpr (ids_openws)
	dd[GKI_CLOSEWS]		= locpr (ids_closews)
	dd[GKI_DEACTIVATEWS]	= 0
	dd[GKI_REACTIVATEWS]	= 0
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (ids_clear)
	dd[GKI_CANCEL]		= locpr (ids_cancel)
	dd[GKI_FLUSH]		= locpr (ids_flush)
	dd[GKI_POLYLINE]	= locpr (ids_polyline)
	dd[GKI_POLYMARKER]	= locpr (ids_polymarker)
	dd[GKI_TEXT]		= locpr (ids_text)
	dd[GKI_FILLAREA]	= locpr (ids_fillarea)
	dd[GKI_PUTCELLARRAY]	= locpr (ids_putcellarray)
	dd[GKI_SETCURSOR]	= locpr (ids_setcursor)
	dd[GKI_PLSET]		= locpr (ids_plset)
	dd[GKI_PMSET]		= locpr (ids_pmset)
	dd[GKI_TXSET]		= locpr (ids_txset)
	dd[GKI_FASET]		= locpr (ids_faset)
	dd[GKI_GETCURSOR]	= locpr (ids_getcursor)
	dd[GKI_GETCELLARRAY]	= locpr (ids_getcellarray)
	dd[GKI_ESCAPE]		= locpr (ids_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0
end
