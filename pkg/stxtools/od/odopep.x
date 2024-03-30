include "od.h"

#---------------------------------------------------------------------------
.help od_open_group 11Jul95 source
.ih
NAME
od_open_group -- Open another "group" of the file
.ih
USAGE
call od_open_group (od, group)
.fi
.ih
ARGUMENTS
.ls od (pointer :input)
The OD I/O descriptor.
.le
.ls group (int :input)
The "group" to open.  For tables, this means the column number to open.
.le
.endhelp
#---------------------------------------------------------------------------
procedure od_open_group (od, group)

pointer od                      # I:  The 1D descriptor.
int     group                   # I:  The group to open.

# Misc.
real    rx                      # Generic.

errchk	gf_opengr, mw_close, od_wcs_open

begin
        switch (OD_TYPE(od)) {
        case OD_TABLE:
            if (group > OD_NGRP(od))
                call error (1, "Attempt to open non-existant column")
            OD_GRP(od) = group

        case OD_IMAGE:
            if (group > OD_NGRP(od))
                call error (1, "Attempt to open non-existant group")

            call mw_close (OD_MW(od))

            if (OD_OLD(od) != NULL)
                call gf_opengr (OD_FD(od), group, rx, rx, OD_FD(OD_OLD(od)))
            else
                call gf_opengr (OD_FD(od), group, rx, rx, NULL)
            OD_GRP(od) = group

            call od_wcs_open (od)
        }       
end
#---------------------------------------------------------------------------
# End of od_open_group
#---------------------------------------------------------------------------
