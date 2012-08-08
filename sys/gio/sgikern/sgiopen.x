# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"sgi.h"

# SGI_OPEN -- Install the SGI kernel as a graphics kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  If a driver does not implement a particular
# instruction the table entry for that procedure may be set to zero, causing
# the interpreter to ignore the instruction.

procedure sgi_open (devname, dd)

char	devname[ARB]		# nonnull for forced output to a device
int	dd[ARB]			# device table to be initialized

pointer	sp, devns
int	len_devname
int	locpr(), strlen()
extern	sgi_openws(), sgi_closews(), sgi_clear(), sgi_cancel()
extern	sgi_flush(), sgi_polyline(), sgi_polymarker(), sgi_text()
extern	sgi_fillarea(), sgi_putcellarray(), sgi_plset()
extern	sgi_pmset(), sgi_txset(), sgi_faset()
extern	sgi_escape()
include	"sgi.com"

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

	dd[GKI_OPENWS]		= locpr (sgi_openws)
	dd[GKI_CLOSEWS]		= locpr (sgi_closews)
	dd[GKI_DEACTIVATEWS]	= 0
	dd[GKI_REACTIVATEWS]	= 0
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (sgi_clear)
	dd[GKI_CANCEL]		= locpr (sgi_cancel)
	dd[GKI_FLUSH]		= locpr (sgi_flush)
	dd[GKI_POLYLINE]	= locpr (sgi_polyline)
	dd[GKI_POLYMARKER]	= locpr (sgi_polymarker)
	dd[GKI_TEXT]		= locpr (sgi_text)
	dd[GKI_FILLAREA]	= locpr (sgi_fillarea)
	dd[GKI_PUTCELLARRAY]	= locpr (sgi_putcellarray)
	dd[GKI_SETCURSOR]	= 0
	dd[GKI_PLSET]		= locpr (sgi_plset)
	dd[GKI_PMSET]		= locpr (sgi_pmset)
	dd[GKI_TXSET]		= locpr (sgi_txset)
	dd[GKI_FASET]		= locpr (sgi_faset)
	dd[GKI_GETCURSOR]	= 0
	dd[GKI_GETCELLARRAY]	= 0
	dd[GKI_ESCAPE]		= locpr (sgi_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0

	# If a device was named open the workstation as well.  This is
	# necessary to permit processing of metacode files which do not
	# contain the open workstation instruction.

	len_devname = strlen (devname)
	if (len_devname > 0) {
	    call achtcs (devname, Mems[devns], len_devname)
	    call sgi_openws (Mems[devns], len_devname, NEW_FILE)
	}

	call sfree (sp)
end
