# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KZOPMT -- Open a magtape file.

procedure kzopmt (device, mode, devcap, devpos, newfile, chan)

char	device[ARB]		#I logical device name
int	mode			#I access mode
char	devcap[ARB]		#I tapecap entry for device
int	devpos[ARB]		#I tape position information
int	newfile			#U receives new file number
int	chan			#O channel assigned for reading filenames

pointer	sp, bp, bd
int	server, dv_len, dc_len, dc_off
int	ki_connect(), ki_send(), ki_receive(), ki_getchan()
int	kmalloc(), strlen()
include	"kichan.com"
include	"kii.com"

begin
	server = ki_connect (device)

	# We must preallocate a channel descriptor in order for error
	# recovery to work, if an abort occurs during the zzopmt.

	chan = ki_getchan (server, chan)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zzopmt (p_sbuf, mode, devcap, devpos, newfile, k_oschan[chan])

	} else {
	    call smark (sp)
	    call salloc (bp, SZ_COMMAND, TY_CHAR)

	    # Determine whether devcap string will fit in sbuf.
	    call strupk (devcap, Memc[bp], SZ_COMMAND)
	    dv_len = strlen (p_sbuf[p_arg[1]])
	    dc_len = strlen (Memc[bp])
	    if (p_arg[1] + dv_len+1 + dc_len > SZ_SBUF)
		dc_off = 0
	    else {
		dc_off = p_arg[1] + dv_len + 1
		call strcpy (Memc[bp], p_sbuf[dc_off], ARB)
		p_sbuflen = dc_off + dc_len
	    }

	    # Prepare the arguments.
	    p_arg[2] = mode
	    p_arg[3] = dc_off
	    p_arg[4] = dc_len
	    p_arg[5] = newfile
	    call amovi (devpos, p_arg[6], LEN_MTDEVPOS)

	    if (ki_send (server, KI_ZFIOMT, MT_OP) == ERR)
		k_oschan[chan] = ERR
	    else if (dc_len > 0 && dc_off == 0) {
		call ks_awrite (server, devcap, dc_len+1)
		call ks_await (server, k_oschan[chan])
	    }

	    if (ki_receive (server, KI_ZFIOMT, MT_OP) == ERR)
		k_oschan[chan] = ERR
	    else {
		k_oschan[chan] = p_arg[1]
		newfile = p_arg[2]
	    }

	    call sfree (sp)
	}

	if (k_oschan[chan] == ERR) {
	    call ki_freechan (chan)
	    chan = ERR
	} else {
	    if (server != NULL) {
		if (kmalloc (bd, LEN_MTDEVPOS, TY_INT) == ERR) {
		    call ki_freechan (chan)
		    chan = ERR
		} else
		    k_bufp[chan] = bd
	    }
	}
end
