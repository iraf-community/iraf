include "od.h"

#---------------------------------------------------------------------------
.help od_put Feb93 source
.ih
NAME
od_put -- Put the data in the file.
.ih
USAGE
.nf
call od_putd (od, data)
.fi
.ih
ARGUMENTS
.ls od (input: pointer)
The OD I/O descriptor.
.le
.ls data (input: double[ARB])
The data to put in the OD file.
.le
.endhelp
#---------------------------------------------------------------------------
procedure od_put (od, data)

pointer od                      # I:  The OD I/O descriptor.
double  data[ARB]               # I:  The data.

# Functions
pointer impl1d()

errchk  impl1d, tbcptd

begin
        # Check if a file is actually opened.  If not, do nothing.
        if (od != NULL) {

            # Get data depending on file type.
            switch (OD_TYPE(od)) {
            case OD_TABLE:
                call tbcptd (OD_FD(od), OD_CD(od,OD_GRP(od)), data,
                             1, OD_LEN(od))

            case OD_IMAGE:
                call amovd (data, Memd[impl1d (OD_FD(od))], OD_LEN(od))
            }
        }
end
#---------------------------------------------------------------------------
# End of od_put
#---------------------------------------------------------------------------
