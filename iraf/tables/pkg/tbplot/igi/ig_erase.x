include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_erase (igs)

pointer	igs		# Parameters structure

begin
	# Erase the screen
	call lcmdcat  (igs, YES)
	call ii_erase (igs)
end


procedure ii_erase (igs)

pointer	igs		# Parameters structure

begin
	call gframe (GIO_GP(igs))
	call gflush (GIO_GP(igs))
end
