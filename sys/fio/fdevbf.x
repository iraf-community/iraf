# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FDEVBF -- Install a new binary file device in the device table, if it has
# not already been installed.

procedure fdevbf (zard, zawr, zawt, zstt, zcls)

int	i, dev_epa
extern	zard(), zawr(), zawt(), zstt(), zcls()
include	<fio.com>

begin
	# Search the device table to see if the device is already installed.
	# The ZDEV array indices the EPA of the read procedure of each device
	# driver.

	call zlocpr (zard, dev_epa)
	for (i=1;  i < next_dev;  i=i+LEN_DTE)
	    if (zdev[i] == dev_epa)
		return

	# Device not found; install the new device in the device table.
	next_dev = next_dev + LEN_DTE
	if (next_dev > LEN_DEVTBL)
	    call syserr (SYS_FDEVTBLOVFL)
	else {
	    call zlocpr (zard, zdev[i])
	    call zlocpr (zawr, zdev[i+1])
	    call zlocpr (zawt, zdev[i+2])
	    call zlocpr (zstt, zdev[i+3])
	    call zlocpr (zcls, zdev[i+4])
	}
end
