# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"ccp.h"

# CCP_OPEN -- Install the calcomp kernel as a graphics kernel device driver.
# The device table DD consists of an array of the entry point addresses for
# the driver procedures.  The table entry for non-implemented procedures is
# set to zero, causing the interpreter to ignore the instruction.

procedure ccp_open (devname, dd)

char	devname[ARB]		# ignored if only one plotter on system
int	dd[ARB]			# device table to be initialized

pointer	sp, devns
int	len_devname
int	locpr(), strlen()
extern	ccp_openws(), ccp_closews(), ccp_clear()
extern	ccp_polyline(), ccp_polymarker(), ccp_text()
extern	ccp_plset()
extern	ccp_pmset(), ccp_txset()
extern	ccp_escape()
include	"ccp.com"

begin
	call smark (sp)
	call salloc (devns, SZ_FNAME, TY_SHORT)

	# Flag first pass.  Save forced device name in common for OPENWS.
	# Zero the frame and instruction counters.

	g_cc = NULL
	g_ndraw = 0 #????? may not need; also used in ccp_openws,ccp_clear,
		    # ccp_polyline, ccp_polymarker, ccp_text; may want for
		    # debug etc.
	call strcpy (devname, g_device, SZ_GDEVICE)

	# Install the device driver.

	dd[GKI_OPENWS]		= locpr (ccp_openws)
	dd[GKI_CLOSEWS]		= locpr (ccp_closews)
	dd[GKI_DEACTIVATEWS]	= 0
	dd[GKI_REACTIVATEWS]	= 0
	dd[GKI_MFTITLE]		= 0
	dd[GKI_CLEAR]		= locpr (ccp_clear)
	dd[GKI_CANCEL]		= 0
	dd[GKI_FLUSH]		= 0
	dd[GKI_POLYLINE]	= locpr (ccp_polyline)
	dd[GKI_POLYMARKER]	= locpr (ccp_polymarker)
	dd[GKI_TEXT]		= locpr (ccp_text)
	dd[GKI_FILLAREA]	= 0
	dd[GKI_PUTCELLARRAY]	= 0
	dd[GKI_SETCURSOR]	= 0
	dd[GKI_PLSET]		= locpr (ccp_plset)
	dd[GKI_PMSET]		= locpr (ccp_pmset)
	dd[GKI_TXSET]		= locpr (ccp_txset)
	dd[GKI_FASET]		= 0
	dd[GKI_GETCURSOR]	= 0
	dd[GKI_GETCELLARRAY]	= 0
	dd[GKI_ESCAPE]		= locpr (ccp_escape)
	dd[GKI_SETWCS]		= 0
	dd[GKI_GETWCS]		= 0
	dd[GKI_UNKNOWN]		= 0

	# If a device was named open the workstation as well.  This is
	# necessary to permit processing of metacode files which do not
	# contain the open workstation instruction.

	len_devname = strlen (devname)
	if (len_devname > 0) {
	    call achtcs (devname, Mems[devns], len_devname)
	    call ccp_openws (Mems[devns], len_devname, NEW_FILE)
	}

	call sfree (sp)
end
