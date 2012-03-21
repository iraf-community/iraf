# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_CLOSEWS -- Close workstation.
#
# BOI GKI_CLOSEWS L N D
#    
#        L(i)            4 + N
#        N(i)            number of characters in field D
#        D(Nc)           device name as in graphcap file

procedure gki_closews (fd, device)

int	fd			# output file
char	device[ARB]		# device name

int	epa
int	ip, nchars, n
pointer	sp, gki, op
int	strlen()
include	"gki.com"

begin
	call smark (sp)

	n = strlen (device)
	call salloc (gki, GKI_CLOSEWS_LEN + n, TY_SHORT)

	# Pack the device name as a SHORT integer array.
	op = gki + GKI_CLOSEWS_D - 1
	for (ip=1;  ip <= n;  ip=ip+1) {
	    Mems[op] = device[ip]
	    op = op + 1
	}

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_CLOSEWS]
	    if (epa != 0)
		call zcall2 (epa, Mems[gki+GKI_CLOSEWS_D-1], n)
	} else {
	    Mems[gki  ] = BOI
	    Mems[gki+1] = GKI_CLOSEWS
	    Mems[gki+2] = GKI_CLOSEWS_LEN + n
	    Mems[gki+GKI_CLOSEWS_N-1] = n

	    # Send a copy of the close workstation directive to PSIOCTRL in
	    # the CL process to connect the graphics stream to a kernel,
	    # before writing to the graphics stream.  The GKI instruction
	    # must be preceded by the integer value of the stream number.

	    nchars = (GKI_CLOSEWS_LEN + n) * SZ_SHORT
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
