# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FDEVTX -- Install a new text file device in the device table, if it has
# not already been installed.

procedure fdevtx (zget, zput, zfls, zstt, zcls, zsek, znot)

int	i, dev_epa
extern	zget(), zput(), zfls(), zstt(), zcls(), zsek(), znot()
include	<fio.com>

begin
	# Search the device table to determine if the device has already been
	# installed.  The ZDEV array indices the device table by the EPA of
	# each device driver.

	call zlocpr (zget, dev_epa)
	for (i=1;  i < next_dev;  i=i+LEN_DTE)
	    if (zdev[i] == dev_epa)
		return

	# Device not found.  Install the new device in the table.
	next_dev = next_dev + LEN_DTE
	if (next_dev > LEN_DEVTBL)
	    call syserr (SYS_FDEVTBLOVFL)
	else {
	    call zlocpr (zget, zdev[i])
	    call zlocpr (zput, zdev[i+1])
	    call zlocpr (zfls, zdev[i+2])
	    call zlocpr (zstt, zdev[i+3])
	    call zlocpr (zcls, zdev[i+4])
	    call zlocpr (zsek, zdev[i+5])
	    call zlocpr (znot, zdev[i+6])
	}
end
