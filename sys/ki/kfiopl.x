# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	"ki.h"

# KFIOPL -- Plotter driver (NSPP metacode translator).

procedure kopnpl (osfn, mode, chan)

char	osfn[ARB]
int	mode, chan

int	server
int	ki_connect(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopnpl (p_sbuf, mode, chan)
	    if (chan != ERR)
		chan = ki_getchan (server, chan)
	} else
	    call kb_zopn (KI_ZFIOPL, osfn, mode, chan)
end


procedure kclspl (chan, status)

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
	    call zclspl (k_oschan[chan], status)
	    call ki_freechan (chan)
	} else
	    call kb_zcls (KI_ZFIOPL, chan, status)
end


procedure kardpl (chan, buf, max_bytes, offset)

int	chan
char	buf[ARB]
int	max_bytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zardpl (k_oschan[chan], buf, max_bytes, offset)
	else
	    call kb_zard (KI_ZFIOPL, chan, buf, max_bytes, offset)
end


procedure kawrpl (chan, buf, nbytes, offset)

int	chan
char	buf[ARB]
int	nbytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawrpl (k_oschan[chan], buf, nbytes, offset)
	else
	    call kb_zawr (KI_ZFIOPL, chan, buf, nbytes, offset)
end


procedure kawtpl (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawtpl (k_oschan[chan], status)
	else
	    call kb_zawt (KI_ZFIOPL, chan, status)
end


procedure ksttpl (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsttpl (k_oschan[chan], what, lvalue)
	else
	    call kb_zstt (KI_ZFIOPL, chan, what, lvalue)
end
