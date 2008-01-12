include "sgraph.h"

procedure gg_initdashpat (linepat)

# GG_INITDASHPAT -- Initialize index of dash pattern array

#  9/10/91  Modified to use symbolic constants for patterns.  Z.G. Levay

int	linepat			# Coded line pattern

int	ip
common	/dashinit/ip

begin
	switch (linepat) {
	case LNP_SOLID:
	    ip = 1
	case LNP_DASH:
	    ip = 2
	case LNP_DOT:
	    ip = 3
	case LNP_DOTD:
	    ip = 4
	default:
	    call eprintf ("Unrecognized line type, using 'solid'\n")
	    ip = 1
	}
end
