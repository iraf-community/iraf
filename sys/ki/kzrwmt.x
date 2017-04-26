# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KZRWMT -- Rewind a (nonopen) magtape drive.

procedure kzrwmt (drive, devcap, status)

char	drive[ARB]		#I packed name of drive to be rewound
char	devcap[ARB]		#I packed tapecap entry for device
int	status			#O receives status, ok|err

pointer	sp, bp
int	server, dv_len, dc_len, dc_off, nbytes
int	ki_connect(), ki_send(), ki_receive(), strlen()
include	"kii.com"

begin
	server = ki_connect (drive)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zzrwmt (p_sbuf, devcap, status)

	} else {
	    call smark (sp)
	    call salloc (bp, SZ_COMMAND, TY_CHAR)

	    # Determine whether devcap string will fit in sbuf.
	    call strupk (devcap, Memc[bp], SZ_COMMAND)
	    dv_len = strlen (p_sbuf[p_arg[1]])
	    dc_len = strlen (Memc[bp])
	    if (dv_len+1 + dc_len > SZ_SBUF) {
		dc_off = 0
		nbytes = (dc_len + SZB_CHAR-1) / SZB_CHAR
	    } else {
		dc_off = dv_len + 1
		call strcpy (Memc[bp], p_sbuf[dc_off], ARB)
		p_sbuflen = dc_off + dc_len
	    }

	    # Prepare the arguments.
	    p_arg[2] = dc_off
	    p_arg[3] = dc_len

	    if (ki_send (server, KI_ZFIOMT, MT_RW) == ERR)
		status = ERR
	    else if (dc_len > 0 && dc_off == 0) {
		call ks_awrite (server, devcap, nbytes)
		call ks_await (server, status)
	    }

	    if (ki_receive (server, KI_ZFIOMT, MT_RW) == ERR)
		status = ERR
	    else
		status = p_arg[1]

	    call sfree (sp)
	}
end
