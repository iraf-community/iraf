# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_POPEN -- Open a variable-array type parameter as a file.  A call to
# fio.close is used to close the file.  Note that the varlen parameter, which
# is stored in its own lfile in the datafile, is opened directly as a file
# independently of the FMIO file buffer cache.  Most QPOE parameter i/o is
# via the cache, hence mixing QP_POPEN calls with ordinary QPOE i/o on the
# same parameter at the same time could lead to loss of data integrity due
# to the same lfile being opened simultaneously on two different file
# descriptors.  We ensure that the lfile is not in the file cache at qp_popen
# time, but no checks are made once the file has been opened.  A FIO file
# descriptor is returned as the function value; CLOSE should be called to
# close the file descriptor when it is no longer needed.

int procedure qp_popen (qp, param, mode, type)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
int	mode			#I file(param) access mode
int	type			#I file type, text or binary

pointer	sym
int	fm_fopen()
pointer	qp_gpsym()
errchk	qp_gpsym, qp_addf(), fm_lockout, syserrs

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	# Lookup the parameter; make sure it is a varlen parameter.
	# Create a new parameter if none exists and the mode is NEW_FILE.

	sym = qp_gpsym (qp, param)
	if (sym == NULL) {
	    if (mode != NEW_FILE)
		call syserrs (SYS_QPUKNPAR, param)
	    else {
		# Create a new parameter.
		if (type == TEXT_FILE)
		    call qp_addf (qp, param, "c", 0, "", 0)
		else
		    call qp_addf (qp, param, "opaque", 0, "", 0)
		sym = qp_gpsym (qp, param)
		if (sym == NULL)
		    call syserrs (SYS_QPUKNPAR, param)
	    }
	} else if (S_MAXELEM(sym) != 0)
	    call syserrs (SYS_QPPOPEN, param)

	# Place a lock on the file and then remove it, to cause an error
	# if the lfile is already active in the file cache.

	call fm_lockout (QP_FM(qp), S_LFILE(sym))
	call fm_unlock  (QP_FM(qp), S_LFILE(sym))

	# Open the assigned lfile and return the file descriptor.
	return (fm_fopen (QP_FM(qp), S_LFILE(sym), mode, type))
end
