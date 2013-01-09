include "tbtables.h"

# This file contains three routines (tbdsav, tbdres, tbdfre) for saving
# and possibly restoring the size information contained in a table
# descriptor.  If some routine needs to change the size of some portion
# of a table (e.g. when adding a new column), then tbdsav should be called
# prior to modifying the table descriptor.  In the event of an error while
# the table is being rewritten, then tbdres should be called to restore
# the size info before calling erract.  If there is no error, then tbdfre
# would be called to deallocate the memory allocated by tbdsav.
#
# Phil Hodge, 25-Apr-1989  Subroutines created.

# tbdsav -- save table descriptor
# This routine saves those values of a table descriptor having to do
# with the sizes of various portions of a table.

procedure tbdsav (tp, tp_save)

pointer tp		# i: pointer to table descriptor
pointer tp_save		# o: pointer to space for saving size info
#--

begin
	call malloc (tp_save, LEN_TBLSTRUCT, TY_STRUCT)

	TB_TYPE(tp_save)    = TB_TYPE(tp)
	TB_NPAR(tp_save)    = TB_NPAR(tp)
	TB_MAXPAR(tp_save)  = TB_MAXPAR(tp)
	TB_NROWS(tp_save)   = TB_NROWS(tp)
	TB_ALLROWS(tp_save) = TB_ALLROWS(tp)
	TB_NCOLS(tp_save)   = TB_NCOLS(tp)
	TB_MAXCOLS(tp_save) = TB_MAXCOLS(tp)
	TB_COLUSED(tp_save) = TB_COLUSED(tp)
	TB_ROWLEN(tp_save)  = TB_ROWLEN(tp)
	TB_BOD(tp_save)     = TB_BOD(tp)
	TB_IOMODE(tp_save)  = TB_IOMODE(tp)
end

# tbdres -- restore table descriptor
# This routine restores those values of a table descriptor having to do
# with the sizes of various portions of a table.  Memory that was allocated
# by tbdsav is deallocated.

procedure tbdres (tp, tp_save)

pointer tp		# i: pointer to table descriptor
pointer tp_save		# io: pointer to space for saving size info
#--

begin
	TB_TYPE(tp)    = TB_TYPE(tp_save)
	TB_NPAR(tp)    = TB_NPAR(tp_save)
	TB_MAXPAR(tp)  = TB_MAXPAR(tp_save)
	TB_NROWS(tp)   = TB_NROWS(tp_save)
	TB_ALLROWS(tp) = TB_ALLROWS(tp_save)
	TB_NCOLS(tp)   = TB_NCOLS(tp_save)
	TB_MAXCOLS(tp) = TB_MAXCOLS(tp_save)
	TB_COLUSED(tp) = TB_COLUSED(tp_save)
	TB_ROWLEN(tp)  = TB_ROWLEN(tp_save)
	TB_BOD(tp)     = TB_BOD(tp_save)
	TB_IOMODE(tp)  = TB_IOMODE(tp_save)

	call tbdfre (tp_save)		# free tp_save
end

# tbdfre -- free scratch space
# This routine deallocates the memory that was allocated by tbdsav.

procedure tbdfre (tp_save)

pointer tp_save		# o: pointer to space for saving size info

begin
	call mfree (tp_save, TY_STRUCT)
end
