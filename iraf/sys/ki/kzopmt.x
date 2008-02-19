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
long	devpos[ARB]		#I tape position information
int	newfile			#U receives new file number
int	chan			#O channel assigned for reading filenames

size_t	sz_val
pointer	sp, bp, bd
long	lstatus
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
	    sz_val = SZ_SBUF
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, sz_val)
	    call zzopmt (p_sbuf, mode, devcap, devpos, newfile, k_oschan[chan])

	} else {
	    call smark (sp)
	    sz_val = SZ_COMMAND
	    call salloc (bp, sz_val, TY_CHAR)

	    # Determine whether devcap string will fit in sbuf.
	    sz_val = SZ_COMMAND
	    call strupk (devcap, Memc[bp], sz_val)
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
	    sz_val = LEN_MTDEVPOS
	    call amovl (devpos, p_arg[6], sz_val)

	    if (ki_send (server, KI_ZFIOMT, MT_OP) == ERR)
		k_oschan[chan] = ERR
	    else if (dc_len > 0 && dc_off == 0) {
		sz_val = dc_len+1
		call ks_awrite (server, devcap, sz_val)
		call ks_await (server, lstatus)
		k_oschan[chan] = lstatus
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
		sz_val = LEN_MTDEVPOS
		if (kmalloc (bd, sz_val, TY_STRUCT) == ERR) {
		    call ki_freechan (chan)
		    chan = ERR
		} else
		    k_bufp[chan] = bd
	    }
	}
end
