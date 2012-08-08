# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gty.h"

# GTYCAPS -- Return a pointer to the caplist field of an open GTY descriptor.

pointer procedure gtycaps (gty)

pointer gty                     # tty descriptor

begin
        return (P2C (gty + T_OFFCAP))
end
