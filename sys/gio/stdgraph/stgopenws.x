# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<ttset.h>
include	<error.h>
include	"stdgraph.h"

# STG_OPENWS -- Open the named workstation.  Once a workstation has been
# opened we leave it open until some other workstation is opened or the
# kernel is closed.  Opening a workstation involves initialization of the
# kernel data structures, following by initialization of the device itself.

procedure stg_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

bool	reinit
pointer	sp, buf
int	dummy, init_file
bool	strne()
pointer	ttygdes(), ttyodes()
int	ttygets(), open(), ttstati()
extern	stg_onerror()
include	"stdgraph.com"

begin
	call smark (sp)
	call salloc (buf, max (SZ_FNAME, n), TY_CHAR)

	# Open a termcap descriptor for the terminal too, in case we need
	# to talk to the terminal as a terminal.

	if (g_term == NULL)
	    iferr (g_term = ttyodes ("terminal"))
		g_term = NULL

	# If a device was named when the kernel was opened then output will
	# always go to that device (g_device) regardless of the device named
	# in the OPENWS instruction.  If no device was named (null string)
	# then unpack the device name, passed as a short integer array.

	if (g_device[1] == EOS) {
	    call achtsc (devname, Memc[buf], n)
	    Memc[buf+n] = EOS
	} else
	    call strcpy (g_device, Memc[buf], SZ_FNAME)

	# If the kernel is already open for this device skip most of the
	# initialization.  If already open for a different device free
	# storage before reinitialization.

	reinit = true
	if (g_sg != NULL)
	    if (strne (Memc[buf], Memc[SG_DEVNAME(g_sg)])) {
		call mfree (SG_SBUF(g_sg), TY_CHAR)
		call mfree (g_sg, TY_STRUCT)
		reinit = true
	    } else
		reinit = false

	# Reinitialize the kernel datastructures.  Open graphcap descriptor
	# for the named device, allocate and initialize descriptor and common.

	if (reinit) {
	    if (g_tty != NULL) {
		call ttycdes (g_tty)
		g_tty = NULL
	    }

	    iferr (g_tty = ttygdes (Memc[buf])) {
		g_tty = ttygdes ("4012")
		call erract (EA_WARN)
	    }

	    # Initialize data structures.
	    call stg_init (g_tty, Memc[buf])
	}

	call stg_reset()

	if (g_active == NO) {
	    # Must disable stty ucaseout mode when in graphics mode, else
	    # plotting commands may be modified by the terminal driver.

	    g_ucaseout = ttstati (g_out, TT_UCASEOUT)
	    if (g_ucaseout == YES)
		call ttseti (g_out, TT_UCASEOUT, NO)

	    # Post ONERROR cleanup routine.
	    call onerror (stg_onerror)
	    g_active = YES
	    g_enable = YES
	}

	# Initialize the device.  Output initialization string followed by
	# contents of initialization file, if named.

	call stg_ctrl ("OW")
	if (ttygets (g_tty, "IF", Memc[buf], SZ_FNAME) > 0) {
	    iferr (init_file = open (Memc[buf], READ_ONLY, BINARY_FILE))
		call erract (EA_WARN)
	    iferr (call fcopyo (init_file, g_out))
		call erract (EA_WARN)
	    call close (init_file)
	}

	# Clear the screen if device is being opened in new_file mode.
	if (mode == NEW_FILE)
	    call stg_clear (dummy)

	call sfree (sp)
end
