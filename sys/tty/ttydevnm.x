# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# TTYDEVNAME -- Extract the logical device name from a full device specification
# of the form  "node ! logical_device ? physical_device".  The node prefix, if
# given, specifies the node from which the device is to be accessed (via the
# network), and the physical device field contains particulars about the
# physical device to be accessed.  Only the logical device field, used to index
# the termcap file, is of any interest to TTY.  The logical device name
# consists of chars chosen from the set [a-zA-Z0-9_+-].

procedure ttydevname (device, ldevice, maxch)

char	device[ARB]		# full device specification
char	ldevice[maxch]		# logical device name
int	maxch

pointer	sp, nodename
int	ip, op, ch
int	ki_extnode()

begin
	call smark (sp)
	call salloc (nodename, SZ_FNAME, TY_CHAR)

	ip = ki_extnode (device, Memc[nodename], maxch, op) + 1

	for (op=1;  device[ip] != EOS;  ip=ip+1) {
	    ch = device[ip]
	    if (!(IS_ALNUM(ch) || ch == '_' || ch == '+' || ch == '-')) {
		ldevice[op] = EOS
		break
	    } else {
		ldevice[op] = ch
		op = op + 1
	    }
	}

	call sfree (sp)
end
