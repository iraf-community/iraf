# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"grc.h"

# GRC_INIT -- Initialize the rcursor descriptor.  Allocate storage for the
# descriptor and initialize all variables and the keystroke mapping.

procedure grc_init (rc)

pointer	rc			#U grc descriptor (pointer)

int	ip, ch
string	keys KEYSTROKES
errchk	malloc

begin
	if (rc == NULL)
	    call malloc (rc, LEN_RCSTRUCT, TY_STRUCT)
	call aclri (Memi[rc], LEN_RCSTRUCT)

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
