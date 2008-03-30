# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<gio.h>
include	"gtr.h"

define	LEN_NAME	10


# GTR_STATUS -- Print information summarizing the utilization of resources
# by each of the three graphics streams.

procedure gtr_status (fd)

int	fd			# output file
int	stream, ip
string	names "STDGRAPH:,STDIMAGE:,STDPLOT: "
include	"gtr.com"

begin
	for (ip=1;  names[ip] != EOS;  ip=ip+1)
	    if (names[ip] == ',')
		names[ip] = EOS

	do stream = STDGRAPH, STDPLOT {
	    ip = (stream - STDGRAPH) * LEN_NAME + 1
	    if (trdes[stream] == NULL) {
		call fprintf (fd, "\t%s disconnected\n")
		    call pargstr (names[ip])
	    } else
		call gtr_memusage (fd, stream, names[ip])
	}

	call fprintf (fd, "\n")
	call flush (fd)
end


# GTR_MEMUSAGE -- Print information summarizing the utilization of memory and
# other resources by a graphics stream.

procedure gtr_memusage (fd, stream, name)

int	fd			# output file
int	stream			# graphics stream to be described
char	name[ARB]		# name of graphics stream

pointer	tr, tx
size_t	bufsize
short	sarg
long	fstatl()
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (stream)

	call fprintf (fd, "\t%s kernel=%s, device=%s, page %s\n")
	    call pargstr (name)
	    call pargstr (TR_KERNFNAME(tr))
	    call pargstr (TR_DEVNAME(tr))
	    if (TR_PAGE(tr) == YES)
		call pargstr ("enabled")
	    else
		call pargstr ("disabled")

	bufsize = fstatl (stream, F_BUFSIZE)
	call fprintf (fd,
	    "\t\tmemory=%d (%dfb+%dsb+%dfio), frame=%d+%d words\n")
	    call pargz (TR_LENFRAMEBUF(tr) + TR_LENSCRATCHBUF(tr) + bufsize)
	    call pargz (TR_LENFRAMEBUF(tr))
	    call pargz (TR_LENSCRATCHBUF(tr))
	    call pargz (bufsize)
	    call pargp (TR_OP(tr) - TR_FRAMEBUF(tr))
	    call pargp (TR_OPSB(tr) - TR_SCRATCHBUF(tr))

	call fprintf (fd,
	    "\t\tspool=%s, nopen=%d, pid=%d, in=%d, out=%d, redir=%d, wcs=%d\n")
	    if (TR_SPOOLDATA(tr) == YES)
		call pargstr ("yes")
	    else
		call pargstr ("no")
	    call pargi (TR_NOPEN(tr))
	    call pargi (TR_PID(tr))
	    call pargi (TR_IN(tr))
	    call pargi (TR_OUT(tr))
	    call pargi (TR_REDIR(tr))
	    call pargi (TR_WCS(tr))

	tx = TR_TXAP(tr)
	call fprintf (fd,
	    "\t\ttext size=%g, up=%d, path=%s, hj=%s, vj=%s, color=%d\n")
	    call pargr (TX_SIZE(tx))
	    call pargi (TX_UP(tx))
	    sarg = TX_PATH(tx)
	    call gkp_txparg (sarg)
	    sarg = TX_HJUSTIFY(tx)
	    call gkp_txparg (sarg)
	    sarg = TX_VJUSTIFY(tx)
	    call gkp_txparg (sarg)
	    call pargi (TX_COLOR(tx))

	call fprintf (fd, "\n")
end
