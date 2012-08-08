# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KFIOPR -- IPC driver.  This driver has no open and close entry points since
# these functions are provided by the ZOPCPR and ZCLCPR procedures, which also
# spawn the subprocess.

#	procedure kopnpr (osfn, mode, chan)
#	
#	char	osfn[ARB]
#	int	mode, chan
#	
#	int	server
#	int	ki_connect(), ki_getchan()
#	include	"kii.com"
#	
#	begin
#		server = ki_connect (osfn)
#	
#		if (server == NULL) {
#		    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
#		    call zopnpr (p_sbuf, mode, chan)
#		    if (chan != ERR)
#			chan = ki_getchan (server, chan)
#		} else
#		    call kb_zopn (KI_ZFIOPR, osfn, mode, chan)
#	end
#	
#	
#	procedure kclspr (chan, status)
#	
#	int	chan
#	int	status
#	include	"kichan.com"
#	
#	begin
#		if (k_node[chan] == NULL) {
#		    call zclspr (k_oschan[chan], status)
#		    k_oschan[chan] = NULL
#		} else
#		    call kb_zcls (KI_ZFIOPR, chan, status)
#	end


procedure kardpr (chan, buf, max_bytes, offset)

int	chan
char	buf[ARB]
int	max_bytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zardpr (k_oschan[chan], buf, max_bytes, offset)
	else
	    call kb_zard (KI_ZFIOPR, chan, buf, max_bytes, offset)
end


procedure kawrpr (chan, buf, nbytes, offset)

int	chan
char	buf[ARB]
int	nbytes
long	offset
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawrpr (k_oschan[chan], buf, nbytes, offset)
	else
	    call kb_zawr (KI_ZFIOPR, chan, buf, nbytes, offset)
end


procedure kawtpr (chan, status)

int	chan
int	status
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zawtpr (k_oschan[chan], status)
	else
	    call kb_zawt (KI_ZFIOPR, chan, status)
end


procedure ksttpr (chan, what, lvalue)

int	chan
int	what
long	lvalue
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zsttpr (k_oschan[chan], what, lvalue)
	else
	    call kb_zstt (KI_ZFIOPR, chan, what, lvalue)
end
