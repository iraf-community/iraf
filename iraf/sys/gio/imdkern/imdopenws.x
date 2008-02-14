# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	<gki.h>
include	<error.h>
include	"imd.h"

# IMD_OPENWS -- Open the named workstation.  Once a workstation has been
# opened we leave it open until some other workstation is opened or the
# kernel is closed.  Opening a workstation involves initialization of the
# kernel data structures, following by initialization of the device itself.

procedure imd_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

pointer	sp, buf
pointer	ttygdes()
bool	streq()
int	idk_open()
bool	need_open, same_dev
include	"imd.com"

begin
	call smark (sp)
	call salloc (buf, max (SZ_FNAME, n), TY_CHAR)

	# If a device was named when the kernel was opened then output will
	# always go to that device (g_device) regardless of the device named
	# in the OPENWS instruction.  If no device was named (null string)
	# then unpack the device name, passed as a short integer array.

	if (g_device[1] == EOS) {
	    call achtsc (devname, Memc[buf], n)
	    Memc[buf+n] = EOS
	} else
	    call strcpy (g_device, Memc[buf], SZ_FNAME)

	# Find out if first time, and if not, if same device as before
	# note that if (g_kt == NULL), then same_dev is false.

	same_dev = false
	need_open = true

	if (g_kt != NULL) {
	    same_dev = (streq (Memc[IMD_DEVNAME(g_kt)], Memc[buf]))
	    if (!same_dev) {
		# Does this device require a frame advance at end of metafile?
		if (IMD_ENDFRAME(g_kt) == YES)
		    call idk_frame (g_out)
		call idk_close (g_out)
	    } else
		need_open = false
	}

	# Initialize the kernel data structures.  Open graphcap descriptor
	# for the named device, allocate and initialize descriptor and common.
	# graphcap entry for device must exist.

	if (need_open) {
	    if (!same_dev) {
		if (g_kt != NULL)
		    call ttycdes (g_tty)
	        iferr (g_tty = ttygdes (Memc[buf]))
		    call erract (EA_ERROR)

		# Initialize data structures if we had to open a new device.
		call imd_init (g_tty, Memc[buf])
		call imd_reset()
	    }

	    # Open the output file.  Metacode output to the device will be
	    # spooled and then disposed of to the device at CLOSEWS time.

	    iferr (g_out = idk_open (g_frame, g_color, g_tty)) {
	        call ttycdes (g_tty)
	        call erract (EA_ERROR)
	    } else {
		# Does this device require a frame advance at start of metafile?
		if (IMD_STARTFRAME(g_kt) == YES)
		    call idk_frame (g_out)
		g_nframes = 0
		g_ndraw = 0
	    }
	}

	# Clear the screen if device is being opened in new_file mode.
	# This is a nop if we really opened a new device, but it will clear
	# the screen if this is just a reopen of the same device in new file
	# mode.

	if (mode == NEW_FILE)
	    call imd_clear (0)

	call sfree (sp)
end
