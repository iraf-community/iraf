include "od.h"

#---------------------------------------------------------------------------
.help od_get Feb93 source
.ih
NAME
od_get -- Retrieve data from file.
.ih
USAGE
.nf
call od_getd (od, data)
.fi
.ih
ARGUMENTS
.ls od (pointer :input)
The OD I/O descriptor.
.le
.ls data (double[ARB] :output)
The data from the OD file.
.le
.endhelp
#---------------------------------------------------------------------------
procedure od_get (od, data)

pointer od                      # I:  The OD I/O descriptor.
double  data[ARB]               # O:  The data.

pointer null                    # Null flag array for table IO.

# Functions
pointer imgl1d()

errchk  gf_opengr, imgl1d, malloc, mfree, tbcgtd

begin
        # Check if a file is actually opened.  If not, do nothing.
        if (od != NULL) {

            # Get data depending on file type.
            switch (OD_TYPE(od)) {
            case OD_TABLE:
                call malloc (null, OD_LEN(od), TY_BOOL)
                call tbcgtd (OD_FD(od), OD_CD(od,OD_GRP(od)), data, Memb[null],
                             1, OD_LEN(od))
                call mfree (null, TY_BOOL)

            case OD_IMAGE:

                # Retrieve the data.
                call amovd (Memd[imgl1d(OD_FD(od))], data, OD_LEN(od))
            }
        }
end
#---------------------------------------------------------------------------
# End of od_get
#---------------------------------------------------------------------------
