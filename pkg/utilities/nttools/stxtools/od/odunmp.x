include "od.h"

#---------------------------------------------------------------------------
.help od_unmap Feb93 source
.ih
NAME
od_unmap -- Close the 1D image.
.ih
USAGE
call od_unmap (od)
.ih
ARGUMENTS
.ls od (input/output: pointer)
The OD I/O descriptor.  On return, the value will be NULL.
.le
.endhelp
#---------------------------------------------------------------------------
procedure od_unmap (od)

pointer od                     # I:  The OD I/O descriptor.

errchk  tbtclo, imunmap, mfree

begin
        if (od != NULL) {
            switch (OD_TYPE(od)) {
            case OD_TABLE:
                call tbtclo (OD_FD(od))
                call mfree (OD_CD_PTR(od), TY_POINTER)
            case OD_IMAGE:
                call mw_ctfree (OD_WL(od))
                call mw_ctfree (OD_LW(od))
                call mw_close (OD_MW(od))
                call imunmap (OD_FD(od))
            }

            call mfree (OD_WSYS_PTR(od), TY_CHAR)
            call mfree (OD_NAME_PTR(od), TY_CHAR)
            call mfree (od, TY_STRUCT)
        }
end
#---------------------------------------------------------------------------
# End of od_unmap
#---------------------------------------------------------------------------
