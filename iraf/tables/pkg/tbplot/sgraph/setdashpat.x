include <gset.h>

procedure gg_setdashpat (gd)

# GG_SETDASHPAT -- Set the current dash pattern to the next one in the 
# available list.

## 8/13/92  Change to use modulo 4.  ZGL

pointer	gd			# Pointer to graphics stream

int	patterns[4]

int	ip
common	/dashinit/ip

data	patterns/GL_SOLID, GL_DASHED, GL_DOTTED, GL_DOTDASH/

begin
	call gseti (gd, G_PLTYPE, patterns[ip])

	ip = mod (ip, 4) + 1
end
