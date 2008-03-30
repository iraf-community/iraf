# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"grc.h"

# GRC_INIT -- Initialize the rcursor descriptor.  Allocate storage for the
# descriptor and initialize all variables and the keystroke mapping.

procedure grc_init (rc)

pointer	rc			#U grc descriptor (pointer)

size_t	sz_val
int	ip, ch
string	keys KEYSTROKES
errchk	malloc

begin
	sz_val = LEN_RCSTRUCT
	if (rc == NULL) {
	    call malloc (rc, sz_val, TY_STRUCT)
	}
	call aclrp (Memp[rc], sz_val)

	# Initialize variables.
	RC_CASE(rc) = YES
	RC_MARKCUR(rc) = NO
	RC_PHYSOPEN(rc) = NO

	# Initialize keystrokes.
	for (ip=1;  keys[ip] != EOS;  ip=ip+1) {
	    ch = keys[ip]
	    RC_KEYS(rc,keys[ip]) = ch
	}
end
