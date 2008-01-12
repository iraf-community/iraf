include "igi.h"
include <fset.h>

procedure ig_playback (igs)

pointer	igs		# igi parameters structure

int	in		# Input command source descriptor
pointer	ifds		# Input file stack descriptor 
pointer	sp, filename

pointer	open()

begin
	call smark (sp)
	call salloc (filename, SZ_FNAME, TY_CHAR)

	in = INPUT_SOURCE(igs)
	ifds = INPUT_STACK(igs)

	# Push the input on the input file stack
	call spshfd (INPUT_SOURCE(igs), ifds)

	# Find the command buffer file name and close it
	call fstats (CMD_BUFFER(igs), F_FILENAME, Memc[filename], SZ_FNAME)
	call close  (CMD_BUFFER(igs))

	# Reopen the command buffer (to reset to BOF)
	CMD_BUFFER(igs) = open (Memc[filename], READ_WRITE, TEXT_FILE)

	in = CMD_BUFFER(igs)
	INPUT_SOURCE(igs) = in
#	call seek (in, BOF)

	call spshfd (CMD_STATE(igs), STATE_STACK(igs))
	CMD_STATE(igs) = PLAYBACK_MODE

	if (DEBUG_OUTPUT(igs) == YES)
	    call eprintf ("Play back command buffer ")
	
	call sfree (sp)
end
