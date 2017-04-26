# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xwhen.h>
include	<syserr.h>
include	<fset.h>

define	SZ_ERRMSG	SZ_LINE

# MA_IDEH -- Iraf Main routine which installs the default exception handler.
# A single handler processes all exceptions.

procedure ma_ideh()

extern	xstdexh()
int	junk, i, epa_standard_handler
int	exception[4]
data	exception /X_ACV, X_INT, X_ARITH, X_IPC/

begin
	call zlocpr (xstdexh, epa_standard_handler)
	do i = 1, 4
	    call xwhen (exception[i], epa_standard_handler, junk)

	# Initialize the critical section protection stack.
	call intr_reset()
end


# XSTDEXH -- Standard exception handler.  Unless the user code posts a handler
# for a particular exception, this handler will gain control.

procedure xstdexh (exception, next_handler)

int	exception			# code for exception
int	next_handler			# EPA of next handler to be called

char	os_errmsg[SZ_ERRMSG]
int	os_errcode

begin
	# Get OS description of the exception.
	call zxgmes (os_errcode, os_errmsg, SZ_ERRMSG)
	call strupk (os_errmsg, os_errmsg, SZ_ERRMSG)

	# Cancel any output and resync awaits.
	call fseti (STDOUT, F_CANCEL, OK)
	call fseti (CLOUT, F_CANCEL, OK)
	call fseti (CLIN, F_CANCEL, OK)

	# Set this here as error() will return immediately if it comes back.
	next_handler = X_IGNORE

	# Take error action.
	switch (exception) {
	case X_ACV:
	    if (os_errcode > 0)
		call fatal (SYS_XACV, os_errmsg)
	    else
		call fatal (SYS_XACV, "Access violation")
	case X_ARITH:
	    if (os_errcode > 0)
		call fatal (SYS_XARITH, os_errmsg)
	    else
		call fatal (SYS_XARITH, "Arithmetic exception")
	case X_INT:
	    if (os_errcode > 0)
		call fatal (SYS_XINT, os_errmsg)
	    else
		call fatal (SYS_XINT, "Keyboard interrupt")
	case X_IPC:
	    call fatal (SYS_XIPC, "Write to IPC with no reader")

	default:
	    call fatal (ERR, "Unknown exception")
	}
end
