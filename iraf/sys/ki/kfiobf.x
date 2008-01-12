# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KFIOBF -- Binary file driver.

procedure kopnbf (osfn, mode, chan)

char	osfn[ARB]
int	mode, chan

int	server
int	ki_connect(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopnbf (p_sbuf, mode, chan)
	    if (chan != ERR)
		chan = ki_getchan (server, chan)
	} else
	    call kb_zopn (KI_ZFIOBF, osfn, mode, chan)
end


procedure kclsbf (chan, status)

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
	    call zclsbf (k_oschan[chan], status)
	    call ki_freechan (chan)
	} else
	    call kb_zcls (KI_ZFIOBF, chan, status)
end


procedure kardbf (chan, buf, max_bytes, offset)

int	chan
char	buf[ARB]
int	max_bytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zardbf (k_oschan[chan], buf, max_bytes, offset)
	else
	    call kb_zard (KI_ZFIOBF, chan, buf, max_bytes, offset)
end


procedure kawrbf (chan, buf, nbytes, offset)

int	chan
char	buf[ARB]
int	nbytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawrbf (k_oschan[chan], buf, nbytes, offset)
	else
	    call kb_zawr (KI_ZFIOBF, chan, buf, nbytes, offset)
end


procedure kawtbf (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawtbf (k_oschan[chan], status)
	else
	    call kb_zawt (KI_ZFIOBF, chan, status)
end


procedure ksttbf (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsttbf (k_oschan[chan], what, lvalue)
	else
	    call kb_zstt (KI_ZFIOBF, chan, what, lvalue)
end
