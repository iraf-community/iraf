# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KFIOSF -- Static file driver.  Since the static file driver may permit
# portions of a data file to be mapped into virtual memory, use the binary
# file driver if the file resides on a remote node.

procedure kopnsf (osfn, mode, chan)

char	osfn[ARB]
int	mode, chan

int	server
int	ki_connect(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopnsf (p_sbuf, mode, chan)
	    if (chan != ERR)
		chan = ki_getchan (server, chan)
	} else
	    call kb_zopn (KI_ZFIOBF, osfn, mode, chan)
end


procedure kclssf (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	# Possible if an abort occurs during the open.
	if (chan <= 0) {
	    status = OK
	    return
	}

	if (k_node[chan] == NULL) {
	    call zclssf (k_oschan[chan], status)
	    call ki_freechan (chan)
	} else
	    call kb_zcls (KI_ZFIOBF, chan, status)
end


procedure kardsf (chan, buf, max_bytes, offset)

int	chan
char	buf[ARB]
int	max_bytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zardsf (k_oschan[chan], buf, max_bytes, offset)
	else
	    call kb_zard (KI_ZFIOBF, chan, buf, max_bytes, offset)
end


procedure kawrsf (chan, buf, nbytes, offset)

int	chan
char	buf[ARB]
int	nbytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawrsf (k_oschan[chan], buf, nbytes, offset)
	else
	    call kb_zawr (KI_ZFIOBF, chan, buf, nbytes, offset)
end


procedure kawtsf (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawtsf (k_oschan[chan], status)
	else
	    call kb_zawt (KI_ZFIOBF, chan, status)
end


procedure ksttsf (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsttsf (k_oschan[chan], what, lvalue)
	else
	    call kb_zstt (KI_ZFIOBF, chan, what, lvalue)
end
