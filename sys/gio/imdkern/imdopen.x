# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"imd.h"

# IMD_OPENDEV -- Install the IMD kernel as a graphics kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  If a driver does not implement a particular
# instruction the table entry for that procedure may be set to zero, causing
# the interpreter to ignore the instruction.

procedure imd_opendev (devname, frame, color, dd)

char	devname[ARB]		# nonnull for forced output to a device
int	frame			# display frame buffer number
int	color			# graphics overlay color index
int	dd[ARB]			# device table to be initialized

pointer	sp, devns
int	len_devname
int	locpr(), strlen()
extern	imd_openws(), imd_closews(), imd_clear(), imd_cancel()
extern	imd_flush(), imd_polyline(), imd_polymarker(), imd_text()
extern	imd_fillarea(), imd_putcellarray(), imd_plset()
extern	imd_pmset(), imd_txset(), imd_faset()
extern	imd_escape()
include	"imd.com"

begin
	call smark (sp)
	call salloc (devns, SZ_FNAME, TY_SHORT)

	# Flag first pass.  Save forced device name in common for OPENWS.
	# Zero the frame and instruction counters.

	g_kt = NULL
	g_nframes = 0
	g_ndraw = 0
	g_frame = frame
	g_color = color
	call strcpy (devname, g_device, SZ_GDEVICE)

	# Install the device driver.

	dd[GKI_OPENWS]		= locpr (imd_openws)
	dd[GKI_CLOSEWS]		= locpr (imd_closews)
	dd[GKI_DEACTIVATEWS]	= 0
	dd[GKI_REACTIVATEWS]	= 0
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (imd_clear)
	dd[GKI_CANCEL]		= locpr (imd_cancel)
	dd[GKI_FLUSH]		= locpr (imd_flush)
	dd[GKI_POLYLINE]	= locpr (imd_polyline)
	dd[GKI_POLYMARKER]	= locpr (imd_polymarker)
	dd[GKI_TEXT]		= locpr (imd_text)
	dd[GKI_FILLAREA]	= locpr (imd_fillarea)
	dd[GKI_PUTCELLARRAY]	= locpr (imd_putcellarray)
	dd[GKI_SETCURSOR]	= 0
	dd[GKI_PLSET]		= locpr (imd_plset)
	dd[GKI_PMSET]		= locpr (imd_pmset)
	dd[GKI_TXSET]		= locpr (imd_txset)
	dd[GKI_FASET]		= locpr (imd_faset)
	dd[GKI_GETCURSOR]	= 0
	dd[GKI_GETCELLARRAY]	= 0
	dd[GKI_ESCAPE]		= locpr (imd_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0

	# If a device was named open the workstation as well.  This is
	# necessary to permit processing of metacode files which do not
	# contain the open workstation instruction.

	len_devname = strlen (devname)
	if (len_devname > 0) {
	    call achtcs (devname, Mems[devns], len_devname)
	    call imd_openws (Mems[devns], len_devname, NEW_FILE)
	}

	call sfree (sp)
end
