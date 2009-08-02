include <error.h>
include <imhdr.h>
include <imio.h>
include <tbset.h>
include <od.h>

# Define the default column number to retrieve table data from
define  DEFAULT_COL     1

#---------------------------------------------------------------------------
.help od_map Feb93 source
.ih
NAME
.nf
od_map          -- Open a file as either an image or table.

od_image_map    -- Internal: Map an image.
od_table_map    -- Internal: Map a table.
.fi
.ih
USAGE
.nf
od = od_map (name, mode, old)

call od_image_map (name, od)
call od_table_map (name, mode, od)
.fi
.ih
ARGUMENTS
.ls name (char[ARB] :input)
The name of the file to open.
.le
.ls mode (int :input)
The access mode to open the file in.  Same as the standard IRAF open
modes.
.le
.ls old (pointer :input)
If creating a new file, use this as a template.  If NULL, no template will
be assumed.  This is the OD descriptor, not an IMIO or TABLE descriptor.
.le
.ls od (pointer :input)
The OD I/O descriptor.
.le
.ih
RETURNS
An od i/o file descriptor containing the image/table descriptor, a flag
indicating whether it is an image or table, and, if a table, the column
descriptor to retrieve the data from.
.ih
DESCRIPTION
This provides a common interface to retrieve one dimensional data from
either an image or a table.  This is vary basic and is not intended to
handle a full i/o interface.  Just need to open, close, and read data.

Added some syntax to the table name specification.  We will allow the
column names/numbers to be specified in a "section" notation.  An
example:

.nf
        tablename[columnname1,...]
.fi

where columnnameX are either names or numbers.  If no column
specification is used, then it is assumed all columns of the table are
to be used and will be considered with the appropriate "group" of
multigroup input.
.endhelp
#---------------------------------------------------------------------------
pointer procedure od_map(name, mode, old)

char    name[ARB]               # I: The file name to open.
int	mode                    # I:  The mode to open the file in.
pointer old                     # I:  Template OD I/O descriptor as template.

# Declarations.
size_t	sz_val
pointer od                      # OD I/O descriptor.
pointer sp                      # Stack Pointer.
pointer sx                      # Generic string.

# Function prototypes
pointer immap()

errchk  malloc, od_image_map, od_table_map

include	<nullptr.inc>

begin
        call smark (sp)
        sz_val = SZ_LINE
        call salloc (sx, sz_val, TY_CHAR)

        # Allocate the od i/o descriptor.
        sz_val = OD_SZ_OD
        call malloc (od, sz_val, TY_STRUCT)
        sz_val = SZ_LINE
        call malloc (OD_NAME_PTR(od), sz_val, TY_CHAR)
        call malloc (OD_WSYS_PTR(od), sz_val, TY_CHAR)

        # If an old descriptor is given, base what open occurs on
        # its type.
        OD_OLD(od) = old
        if (old != NULL)
            switch (OD_TYPE(old)) {
            case OD_IMAGE:
                OD_FD(od) = immap (name, mode, OD_FD(old))
                call od_image_map (name, od)
            case OD_TABLE:
                call od_table_map (name, mode, OD_FD(old), od)
            }

        # Else, just open up that data file.  If the image call doesn't fail,
        # then assume its an image.
        else ifnoerr (OD_FD(od) = immap (name, mode, NULLPTR))
            call od_image_map (name, od)

        # If it cannot be opened as a table, try changing the extension.
        # If that fails, then give it up.
        else iferr (call od_table_map (name, mode, NULLPTR, od)) {
            call change_ext (name, "c1h", Memc[sx], SZ_LINE)
            iferr (OD_FD(od) = immap (Memc[sx], mode, NULLPTR)) {
                call erract (EA_ERROR)
            }
            call od_image_map (Memc[sx], od)
        }

        # That's all folks.
        call sfree (sp)
        return (od)
end
#---------------------------------------------------------------------------
# End of od_map
#---------------------------------------------------------------------------
procedure od_image_map (name, od)

char    name[ARB]               # I:  Full specified name.
pointer od                      # I:  OD I/O descriptor.

# Declarations.
size_t	sz_val
int	i                       # Generic.

pointer sp                      # Stack pointer.
pointer sx

begin
        call smark (sp)
        sz_val = SZ_LINE
        call salloc (sx, sz_val, TY_CHAR)

        # Fill the OD I/O descriptor.
        OD_TYPE(od) = OD_IMAGE
        OD_CD_PTR(od) = NULL
        OD_LEN(od) = IM_LEN(OD_FD(od),1)
        OD_NGRP(od) = max(1,IM_CLSIZE(OD_FD(od)))
        call strcpy (IM_HDRFILE(OD_FD(od)), OD_NAME(od), SZ_LINE)

        # See whether a specific group was opened.
        call fparse (name, Memc[sx], SZ_LINE, Memc[sx], SZ_LINE, Memc[sx],
                     SZ_LINE, OD_GRP(od), i, Memc[sx], SZ_LINE, Memc[sx],
                     SZ_LINE)
        if (OD_GRP(od) > 0)
            OD_NGRP(od) = 1
        else
            OD_GRP(od) = 1

        # Get world coordinate information.
        call od_wcs_open (od)

        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of od_image_map
#---------------------------------------------------------------------------
procedure od_table_map (name, mode, old, od)

char    name[ARB]               # I:  The specified file name.
int	mode                    # I:  The file access mode.
pointer old                     # I:  Original OD descriptor.
pointer od                      # I:  The OD I/O descriptor.

# Declarations.
size_t	sz_val
int	i, j, k                 # Generic.
int	ic                      # Pointer into section list.

pointer colname                 # Current column name.
pointer section                 # Section specification.
pointer sp                      # Stack pointer.
pointer sx                    # Generic.

# Functions.
int	ctoi(), strlen(), word_count(), word_fetch(), tbpsta()
long	tbpstl()
pointer tbcnum(), tbtopn()

errchk  tbcnum, tbpsta, tbpstl, tbtopn, word_count, word_fetch

begin
        call smark (sp)
        sz_val = SZ_LINE
        call salloc (colname, sz_val, TY_CHAR)
        call salloc (section, sz_val, TY_CHAR)
        call salloc (sx, sz_val, TY_CHAR)

        # Set what type of file.
        OD_TYPE(od) = OD_TABLE

        # Get the base filename and section.
        call od_parse (name, OD_NAME(od), SZ_LINE, Memc[section], SZ_LINE)

        # Open up and get some parameters.
        OD_FD(od) = tbtopn (OD_NAME(od), mode, old)
        OD_LEN(od) = tbpstl (OD_FD(od), TBL_NROWS)
        OD_GRP(od) = 1
        OD_MW(od) = NULL
        OD_WL(od) = NULL
        OD_LW(od) = NULL

        # Now retrieve the columns.  If no columns are specified, then use
        # all the columns.
        if (strlen (Memc[section]) <= 0) {
            OD_NGRP(od) = tbpsta (OD_FD(od), TBL_NCOLS)
            sz_val = OD_NGRP(od)
            call malloc (OD_CD_PTR(od), sz_val, TY_POINTER)
            do i = 1, OD_NGRP(od) {
                OD_CD(od,i) = tbcnum (OD_FD(od), i)
                if (OD_CD(od,i) == NULL) {
                    call sprintf (Memc[sx], SZ_LINE, "Cannot open column %d in table %s")
                    call pargi (i)
                    call pargstr (OD_NAME(od))
                    call error (1, Memc[sx])
                }
            }
        } else {
            OD_NGRP(od) = word_count (Memc[section])
            sz_val = OD_NGRP(od)
            call malloc (OD_CD_PTR(od), sz_val, TY_POINTER)
            i = 0
            ic = 1
            while (word_fetch (Memc[section], ic, Memc[colname], SZ_LINE) > 0) {
                i = i + 1
                k = 1
                if (ctoi (Memc[colname], k, j) > 0)
                    OD_CD(od,i) = tbcnum (OD_FD(od), j)
                else
                    call tbcfnd (OD_FD(od), Memc[colname], OD_CD(od,i), 1)
            }
            if (OD_CD(od,i) == NULL) {
                call sprintf (Memc[sx], SZ_LINE, "Cannot open column %s in table %s")
                call pargstr (Memc[colname])
                call pargstr (OD_NAME(od))
                call error (1, Memc[sx])
            }
        }
        
        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of od_table_map
#---------------------------------------------------------------------------
