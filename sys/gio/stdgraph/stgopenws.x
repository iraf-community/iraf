# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<ttset.h>
include	<error.h>
include	<chars.h>
include	<finfo.h>
include	"stdgraph.h"

# STG_OPENWS -- Open the named workstation.  Once a workstation has been
# opened we leave it open until some other workstation is opened or the
# kernel is closed.  Opening a workstation involves initialization of the
# kernel data structures, followed by initialization of the device itself.

procedure stg_openws (devname, n, mode)

short	devname[ARB]		#I device name (actually device[,uifname])
int	n			#I length of device name
int	mode			#I access mode

bool	reinit
long	fi[LEN_FINFO]
int	dummy, init_file
pointer	sp, ip, op, buf, device, uifname, fname

pointer	ttygdes(), ttyodes()
bool	ttygetb(), strne(), streq()
int	ttygets(), open(), ttstati(), finfo(), gstrcpy()
int	nowhite(), envfind(), strlen(), fnroot(), access()
extern	stg_onerror()
include	"stdgraph.com"
define	ow_ 91

begin
	call smark (sp)
	call salloc (buf, max (SZ_PATHNAME, n), TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Open a termcap descriptor for the terminal too, in case we need
	# to talk to the terminal as a terminal.

	if (g_term == NULL)
	    iferr (g_term = ttyodes ("terminal"))
		g_term = NULL

	# If we are appending merely reactivate the device without performing
	# any initialization.

	if (g_sg != NULL && mode == APPEND) {
	    if (g_active == NO) {
		g_ucaseout = ttstati (g_out, TT_UCASEOUT)
		if (g_ucaseout == YES)
		    call ttseti (g_out, TT_UCASEOUT, NO)

		g_active = YES
		g_enable = YES
	    }
	    goto ow_
	}

	# If a device was named when the kernel was opened then output will
	# always go to that device (g_device) regardless of the device named
	# in the OPENWS instruction.  If no device was named (null string)
	# then unpack the device name, passed as a short integer array.

	if (g_device[1] == EOS) {
	    call achtsc (devname, Memc[buf], n)
	    Memc[buf+n] = EOS
	} else
	    call strcpy (g_device, Memc[buf], SZ_FNAME)

	# Parse the "device,uifname" specification into the two fields.
	device = buf
	uifname = NULL
	for (ip=buf;  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == ',') {
		Memc[ip] = EOS
		if (Memc[ip+1] != EOS)
		    uifname = ip + 1
		if (nowhite (Memc[uifname], Memc[uifname], ARB) == 0)
		    uifname = NULL
		break
	    }

	# If the kernel is already open for this device skip most of the
	# initialization.  If already open for a different device free
	# storage before reinitialization.

	reinit = true
	if (g_sg != NULL)
	    if (strne (Memc[device], Memc[SG_DEVNAME(g_sg)])) {
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

	    iferr (g_tty = ttygdes (Memc[device])) {
		g_tty = ttygdes ("4012")
		call erract (EA_WARN)
	    }

	    # Initialize data structures.
	    call stg_init (g_tty, Memc[device])
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

	# If no UI file was specified but the device has the EM capability,
	# use the default UI if any specified in the graphcap entry.  If the
	# EM capability is missing, ignore any uifname specified when the
	# device was opened.

	if (ttygetb (g_tty, "EM")) {
	    if (uifname == NULL) {
		uifname = buf + strlen(Memc[device]) + 1
		if (ttygets (g_tty, "ED", Memc[uifname], ARB) <= 0)
		    uifname = NULL
	    }

	    # If the user has a version of the named UI file in their GUIDIR,
	    # use that instead.

	    if (envfind (GUIDIR, Memc[fname], SZ_PATHNAME) > 0) {
		op = fname + strlen (Memc[fname])
		op = op + fnroot (Memc[uifname], Memc[op],
		    fname + SZ_PATHNAME - op)
		op = op + gstrcpy (".gui", Memc[op], fname + SZ_PATHNAME - op)
		if (access (Memc[fname], 0, 0) == YES)
		    uifname = fname
	    }

	    # If the UI is already running and has not been modified there
	    # is no need to download it again.

	    if (g_sg != NULL)
		if (streq (Memc[uifname], Memc[SG_UIFNAME(g_sg)]))
		    if (finfo (Memc[uifname], fi) != ERR)
			if (SG_UIFDATE(g_sg) == FI_MTIME(fi))
			    uifname = NULL
	} else {
	    # Ignore UI file if no EM capability.
	    Memc[SG_UIFNAME(g_sg)] = EOS
	    SG_UIFDATE(g_sg) = 0
	    uifname = NULL
	}

	# Open and Initialize the device.  Output contents of UI definition
	# file if any, followed by graphics device initialization file,
	# if any.

	if (mode == NEW_FILE) {
	    # Output UI definition file.
	    if (uifname != NULL) {
		iferr (init_file = open (Memc[uifname], READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    call stg_ctrl ("OW")
		} else {
		    call flush (g_out)
		    call stg_ctrl ("EM")

		    # Download the UI.
		    call putline (g_out, "server ")
		    iferr (call fcopyo (init_file, g_out))
			call erract (EA_WARN)
		    call close (init_file)

		    # Record particulars of active UI file.
		    call strcpy (Memc[uifname], Memc[SG_UIFNAME(g_sg)],
			SZ_UIFNAME)
		    if (finfo (Memc[uifname], fi) != ERR)
			SG_UIFDATE(g_sg) = FI_MTIME(fi)
		    call sgf_post_filter (g_out)

		    call putci (g_out, US)
		    call flush (g_out)
		}
	    } else
		call stg_ctrl ("OW")

	    # Output device graphics initialization file if any.
	    if (ttygets (g_tty, "IF", Memc[buf], SZ_FNAME) > 0) {
		iferr (init_file = open (Memc[buf], READ_ONLY, TEXT_FILE))
		    call erract (EA_WARN)
		iferr (call fcopyo (init_file, g_out))
		    call erract (EA_WARN)
		call close (init_file)
	    }

	    # Clear the screen if device is being opened in new_file mode.
	    call stg_clear (dummy)

	} else
ow_	    call stg_ctrl ("OW")

	call sfree (sp)
end
