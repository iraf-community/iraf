# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<error.h>
include	<ttset.h>
include	<fio.h>

# FINIT -- Initialize FIO.  Called once by the IRAF Main upon process startup.
# Mark all file descriptors empty and install drivers for the standard file 
# defices, i.e., text file, binary file, terminal, and IPC.

procedure finit()

int	fd, first_time

extern	zgettx(), zputtx(), zflstx(), zstttx(), zclstx(), zsektx(), znottx()
extern	zgetty(), zputty(), zflsty(), zsttty(), zclsty(), zsekty(), znotty()
extern	zgettt(), zputtt(), zflstt(), zstttt(), zclstt(), zsektt(), znottt()
extern	zgetnu(), zputnu(), zflsnu(), zsttnu(), zclsnu(), zseknu(), znotnu()
extern	zardbf(), zawrbf(), zawtbf(), zsttbf(), zclsbf()
extern	zardsf(), zawrsf(), zawtsf(), zsttsf(), zclssf()
extern	zardpr(), zawrpr(), zawtpr(), zsttpr(), pr_zclspr()
extern	zardps(), zawrps(), zawtps(), zsttps(), zclsps()
extern	zardnu(), zawrnu(), zawtnu()

include	<fio.com>
data	first_time /YES/
errchk	syserr

begin
	# If we are called more than once it is probably due to a name conflict
	# with a user routine, so generate a fatal error abort.

	if (first_time == YES)
	    first_time = NO
	else iferr (call syserr (SYS_FINITREP))
	    call erract (EA_FATAL)

	# Free up all the file descriptors.  Note that FDs 1 through FIRST_FD
	# will be assigned to CLIN through STDERR by CLOPEN.

	do fd = 1, LAST_FD
	    fiodes[fd] = NULL

	# Install the standard devices in the device table.  The first entry
	# should be the standard text file device, followed by the standard
	# binary file device.  NOTE: the standard devices must be installed
	# in the table in the order TX,BF,TY,PR,SF to agree with the device
	# code definitions in fio.h.  The NU drivers implement the nullfile.

	next_dev = 1
	call fdevtx (zgettx, zputtx, zflstx, zstttx, zclstx, zsektx, znottx)
	call fdevbf (zardbf, zawrbf, zawtbf, zsttbf, zclsbf)
	call fdevtx (zgettt, zputtt, zflstt, zstttt, zclstt, zsektt, znottt)
	call fdevbf (zardpr, zawrpr, zawtpr, zsttpr, pr_zclspr)
	call fdevbf (zardsf, zawrsf, zawtsf, zsttsf, zclssf)

	call fdevtx (zgetty, zputty, zflsty, zsttty, zclsty, zsekty, znotty)
	call fdevtx (zgetnu, zputnu, zflsnu, zsttnu, zclsnu, zseknu, znotnu)
	call fdevbf (zardnu, zawrnu, zawtnu, zsttnu, zclsnu)
	call fdevbf (zardps, zawrps, zawtps, zsttps, zclsps)

	# Initialize the TEMP_FILE handler.
	call fsvtfn ("")

	# Initialize the TT logical terminal driver.
	call zsettt (0, TT_INITIALIZE, 0)
end
