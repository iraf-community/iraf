# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <fset.h>
include	<gki.h>
include	<error.h>
include	"../lib/ids.h"

# IDS_OPENWS -- Open the named workstation.  Once a workstation has been
# opened we leave it open until some other workstation is opened or the
# kernel is closed.  Opening a workstation involves initialization of the
# kernel data structures. Initialization of the device itself is left to
# an explicit reset command.

procedure ids_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

long	filesize
bool	need_open, same_dev
pointer	sp, buf, devinfo

long	fstatl()
pointer	ttygdes()
bool	streq(), ttygetb()
int	fopnbf(), ttygets()
extern	zopnim(), zardim(), zawrim(), zawtim(), zsttim(), zclsim()
errchk	ttygdes
int	oldmode
data	oldmode /-1/

include	"../lib/ids.com"

begin
	call smark (sp)
	call salloc (buf, max (SZ_FNAME, n), TY_CHAR)
	call salloc (devinfo, SZ_LINE, TY_CHAR)

	# If a device was named when the kernel was opened then output will
	# always be to that device (i_device) regardless of the device named
	# in the OPENWS instruction.  If no device was named (null string)
	# then unpack the device name, passed as a short integer array.

	if (i_device[1] == EOS) {
	    call achtsc (devname, Memc[buf], n)
	    Memc[buf+n] = EOS
	} else
	    call strcpy (i_device, Memc[buf], SZ_FNAME)

	# find out if first time, and if not, if same device as before
	# note that if (i_kt == NULL), then same_dev is false.

	same_dev = false
	need_open = true
	if ( i_kt != NULL ) {
	    same_dev = (streq(Memc[IDS_DEVNAME(i_kt)], Memc[buf]))
	    if ( !same_dev  || ( oldmode != mode))
		call close(i_out)
	    else
		need_open = false
	}
	oldmode = mode

	# Initialize the kernel data structures.  Open graphcap descriptor
	# for the named device, allocate and initialize descriptor and common.
	# graphcap entry for device must exist.

	if (need_open) {
	    if ((i_kt != NULL) && !same_dev)
		call ttycdes (i_tty)
	    if (!same_dev) {
	        i_tty = ttygdes (Memc[buf])
		if (ttygetb (i_tty, "LC"))
		    call error (1, "operation not supported on device")
	    }

	    if (ttygets (i_tty, "DD", Memc[devinfo], SZ_LINE) <= 0)
		call strcpy (Memc[buf], Memc[devinfo], SZ_LINE)

	    # Open the output file.  The device is connected to FIO as a 
	    # binary file.  mode must be READ_WRITE or WRITE_ONLY
	    # for image display!

	    iferr (i_out = fopnbf (Memc[devinfo], mode, zopnim, zardim,
	        zawrim, zawtim, zsttim, zclsim)) {

	        call ttycdes (i_tty)
	        call erract (EA_ERROR)
	    }
	    call fseti (i_out, F_ADVICE, SEQUENTIAL)

	}

	# Initialize data structures.
	# Device specific initialization will be done in the zinit call
	# from ids_init().

	if (!same_dev) {
	    call ids_init (i_tty, Memc[buf])

	    # Now set the file size to allow mapping of all control registers
	    # as well as all image and graphics planes.  The call to fstatl
	    # returns the size of an image plane (!!).  zinit does whatever
	    # device work it needs to do, and uses its arguments to determine
	    # the total file size, which it returns.
	    # This feature need not be used (and is not for the IIS display).
	    #
	    # We also set the F_ASYNC parameter to YES.

	    i_frsize = fstatl(i_out, F_FILESIZE)
	    filesize = i_frsize
	    call zinit(i_maxframes, i_maxgraph, filesize)
	    call fseti(i_out, F_ASYNC, YES)

	}

	call sfree (sp)
end
