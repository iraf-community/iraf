# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KZOPMT -- Open a magtape file.

procedure kzopmt (drive, density, mode, oldrec, oldfile, newfile, chan)

char	drive[ARB]		# logical drive name
int	density			# desired density or 0
int	mode			# access mode
int	oldrec			# current record
int	oldfile			# current file
int	newfile			# receives new file number
int	chan			# channel assigned for reading filenames

int	server
int	ki_connect(), ki_sendrcv(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (drive)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zzopmt (p_sbuf, density, mode, oldrec, oldfile, newfile, chan)

	} else {
	    p_arg[2] = density
	    p_arg[3] = mode
	    p_arg[4] = oldrec
	    p_arg[5] = oldfile
	    p_arg[6] = newfile

	    if (ki_sendrcv (server, KI_ZFIOMT, MT_OP) == ERR)
		chan = ERR
	    else if (p_arg[1] == ERR)
		chan = ERR
	    else {
		chan    = p_arg[1]
		newfile = p_arg[2]
	    }
	}

	if (chan != ERR)
	    chan = ki_getchan (server, chan)
end
