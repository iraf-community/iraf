# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	<gki.h>
include	<error.h>
include	"ccp.h"

# CCP_OPENWS -- Open the named workstation.  Once a workstation has been
# opened we leave it open until some other workstation is opened or the
# kernel is closed.  Opening a workstation involves initialization of the
# kernel data structures, following by initialization of the device itself.

procedure ccp_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

pointer	sp, buf
pointer	ttygdes()
bool	streq()
bool	need_open, same_dev
include	"ccp.com"

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	# If a particular plotter was named when the kernel was opened then 
	# output will always go to that plotter (g_device) regardless of the 
	# plotter named in the OPENWS instruction.  If no plotter was named 
	# (null string) then unpack the plotter name, passed as a short integer 
	# array.

	if (g_device[1] == EOS) {
	    call achtsc (devname, Memc[buf], n)
	    Memc[buf+n] = EOS
	} else
	    call strcpy (g_device, Memc[buf], SZ_FNAME)

	# Find out if first time, and if not, if same device as before
	# note that if (g_cc == NULL), then same_dev is false.

	same_dev = false
	need_open = true

	if (g_cc != NULL) {			# not first time
	    same_dev = (streq (Memc[CCP_DEVNAME(g_cc)], Memc[buf]))
	    if (!same_dev) {
		# close previous plotter, initialize new one.
		call plot (0, 0, 999)
		call plots (0, 0, CCP_DEVCHAN(g_cc))
	    } else
		need_open = false
	}

	# Initialize the kernel data structures.  Open graphcap descriptor
	# for the named device, allocate and initialize descriptor and common.
	# graphcap entry for device must exist.

	if (need_open) {
	    if ((g_cc != NULL) && !same_dev)
		call ttycdes (g_tty)			# close prev tty
	    if (!same_dev) {
	        iferr (g_tty = ttygdes (Memc[buf]))
		    call erract (EA_ERROR)
		g_ndraw = 0
	    }
	}

	# Initialize data structures if we had to open a new device.
	if (!same_dev) {
	    call ccp_init (g_tty, Memc[buf])
	    call ccp_reset()
	    call plots (0, 0, CCP_DEVCHAN(g_cc))
	}

	# Advance a frame if device is being opened in new_file mode.
	# This is a nop if we really opened a new device, but it will advance
	# the paper if this is just a reopen of the same device in new file
	# mode.

	if (mode == NEW_FILE)
	    call ccp_clear (0)

	call sfree (sp)
end
