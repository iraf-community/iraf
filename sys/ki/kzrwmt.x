# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KZRWMT -- Rewind a (nonopen) magtape drive.

procedure kzrwmt (drive, status)

char	drive[ARB]		# packed name of drive to be rewound
int	status			# receives status, ok|err

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (drive)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zzrwmt (p_sbuf, status)

	} else {
	    if (ki_sendrcv (server, KI_ZFIOMT, MT_RW) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
