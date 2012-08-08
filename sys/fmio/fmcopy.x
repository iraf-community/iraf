# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"fmset.h"

# FM_COPY -- Copy a datafile, preserving all the physical attributes, but
# eliminating waste storage and rendering file structures logically contiguous.

procedure fm_copy (dfname, newname)

char	dfname[ARB]		#I existing datafile
char	newname[ARB]		#I new datafile name

pointer	o_fm, n_fm
pointer	fm_open()
int	fm_stati()
errchk	fm_open, fm_copyo

begin
	# Open the old and new datafiles.
	o_fm = fm_open (dfname, READ_ONLY)
	n_fm = fm_open (newname, NEW_FILE)

	# The child inherits the attributes of the parent.
	call fm_seti (n_fm, FM_PAGESIZE,   fm_stati(o_fm,FM_PAGESIZE))
	call fm_seti (n_fm, FM_MAXLFILES,  fm_stati(o_fm,FM_MAXLFILES))

	# Copy the datafile and clean up.
	iferr (call fm_copyo (o_fm, n_fm)) {
	    call fm_close (o_fm)
	    call fm_close (n_fm)
	    call erract (EA_ERROR)
	} else {
	    call fm_close (o_fm)
	    call fm_close (n_fm)
	}
end
