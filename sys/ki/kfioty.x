# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KFIOTY -- Terminal driver.

procedure kopnty (osfn, mode, chan)

char	osfn[ARB]		# packed os filename
int	mode			# access mode
int	chan			# receives channel code

int	server
int	ki_connect(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopnty (p_sbuf, mode, chan)
	    if (chan != ERR)
		chan = ki_getchan (server, chan)
	} else
	    call kt_zopn (KI_ZFIOTY, osfn, mode, chan)
end


procedure kclsty (chan, status)

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
	    call zclsty (k_oschan[chan], status)
	    call ki_freechan (chan)
	} else
	    call kt_zcls (KI_ZFIOTY, chan, status)
end


procedure kgetty (chan, text, maxch, status)

int	chan
char	text[maxch]
int	maxch, status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zgetty (k_oschan[chan], text, maxch, status)
	else
	    call kt_zget (KI_ZFIOTY, chan, text, maxch, status)
end


procedure kputty (chan, text, nchars, status)

int	chan
char	text[nchars]
int	nchars, status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zputty (k_oschan[chan], text, nchars, status)
	else
	    call kt_zput (KI_ZFIOTY, chan, text, nchars, status)
end


procedure kflsty (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zflsty (k_oschan[chan], status)
	else
	    call kt_zfls (KI_ZFIOTY, chan, status)
end


procedure ksekty (chan, loffset, status)

int	chan
long	loffset
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsekty (k_oschan[chan], loffset, status)
	else
	    call kt_zsek (KI_ZFIOTY, chan, loffset, status)
end


procedure knotty (chan, loffset)

int	chan
long	loffset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call znotty (k_oschan[chan], loffset)
	else
	    call kt_znot (KI_ZFIOTY, chan, loffset)
end


procedure ksttty (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsttty (k_oschan[chan], what, lvalue)
	else
	    call kt_zstt (KI_ZFIOTY, chan, what, lvalue)
end
