# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_OPENWS -- Open workstation.
#
# BOI GKI_OPENWS L M N D
#    
#        L(i)            5 + N
#        M(i)            access mode (APPEND=4, NEW_FILE=5, TEE=6)
#        N(i)            number of characters in field D
#        D(Nc)           device name as in graphcap file

procedure gki_openws (fd, device, mode)

int	fd			# output file
char	device[ARB]		# device name
int	mode			# access mode

int	ip, n, epa, nchars
pointer	sp, gki, op
int	strlen()
include	"gki.com"

begin
	call smark (sp)

	n = strlen (device)
	call salloc (gki, GKI_OPENWS_LEN + n, TY_SHORT)

	# Pack the device name as a SHORT integer array.
	op = gki + GKI_OPENWS_D - 1
	for (ip=1;  ip <= n;  ip=ip+1) {
	    Mems[op] = device[ip]
	    op = op + 1
	}

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_OPENWS]
	    if (epa != 0)
		call zcall3 (epa, Mems[gki+GKI_OPENWS_D-1], n, mode)
	} else {
	    Mems[gki  ] = BOI
	    Mems[gki+1] = GKI_OPENWS
	    Mems[gki+2] = GKI_OPENWS_LEN + n
	    Mems[gki+GKI_OPENWS_M-1] = mode
	    Mems[gki+GKI_OPENWS_N-1] = n

	    # Send a copy of the open workstation directive to PSIOCTRL in
	    # the CL process to connect the graphics stream to a kernel,
	    # before writing to the graphics stream.  The GKI instruction
	    # must be preceded by the integer value of the stream number.

	    nchars = (GKI_OPENWS_LEN + n) * SZ_SHORT
	    if (IS_FILE(fd) && (fd >= STDGRAPH && fd <= STDPLOT)) {
		call write (PSIOCTRL, fd, SZ_INT32)
		call write (PSIOCTRL, Mems[gki], nchars)
		call flush (PSIOCTRL)
	    }

	    # Now send a copy to the graphics kernel.
	    call write (gk_fd[fd], Mems[gki], nchars)
	}

	call sfree (sp)
end
