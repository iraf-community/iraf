# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KFIOTX -- Text file driver.

procedure kopntx (osfn, mode, chan)

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
	    call zopntx (p_sbuf, mode, chan)
	    if (chan != ERR)
		chan = ki_getchan (server, chan)
	} else
	    call kt_zopn (KI_ZFIOTX, osfn, mode, chan)
end


procedure kclstx (chan, status)

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
	    call zclstx (k_oschan[chan], status)
	    call ki_freechan (chan)
	} else
	    call kt_zcls (KI_ZFIOTX, chan, status)
end


procedure kgettx (chan, text, maxch, status)

int	chan
char	text[maxch]
int	maxch, status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zgettx (k_oschan[chan], text, maxch, status)
	else
	    call kt_zget (KI_ZFIOTX, chan, text, maxch, status)
end


procedure kputtx (chan, text, nchars, status)

int	chan
char	text[nchars]
int	nchars, status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zputtx (k_oschan[chan], text, nchars, status)
	else
	    call kt_zput (KI_ZFIOTX, chan, text, nchars, status)
end


procedure kflstx (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zflstx (k_oschan[chan], status)
	else
	    call kt_zfls (KI_ZFIOTX, chan, status)
end


procedure ksektx (chan, loffset, status)

int	chan
long	loffset
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsektx (k_oschan[chan], loffset, status)
	else
	    call kt_zsek (KI_ZFIOTX, chan, loffset, status)
end


procedure knottx (chan, loffset)

int	chan
long	loffset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call znottx (k_oschan[chan], loffset)
	else
	    call kt_znot (KI_ZFIOTX, chan, loffset)
end


procedure kstttx (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zstttx (k_oschan[chan], what, lvalue)
	else {
	    # Querying the text file status parameters is slow over the net,
	    # and they are essentially constants anyhow, so just return the
	    # expected values.
	    #
	    # call kt_zstt (KI_ZFIOTX, chan, what, lvalue)

	    switch (what) {
	    case FSTT_BLKSIZE:
		lvalue = 1
	    case FSTT_FILSIZE:
		lvalue = 1
	    case FSTT_OPTBUFSIZE:
		lvalue = SZ_LINE
	    case FSTT_MAXBUFSIZE:
		lvalue = 0
	    default:
		lvalue = ERR
	    }
	}
end
