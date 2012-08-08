include	<pkg/igsfit.h>

# IGS_INIT -- Initialize the surface fitting parameters.

procedure igs_init (function, xorder, yorder, xmin, xmax, ymin, ymax)

char	function[ARB]		# Function
int	xorder			# X order
int	yorder			# Y order
real	xmin, xmax		# X range
real	ymin, ymax		# Y range

begin
	call igs_sets (IGS_FUNCTION, function)
	call igs_seti (IGS_XORDER, xorder)
	call igs_seti (IGS_YORDER, yorder)
	call igs_setr (IGS_XMIN, xmin)
	call igs_setr (IGS_XMAX, xmax)
	call igs_setr (IGS_YMIN, ymin)
	call igs_setr (IGS_YMAX, ymax)
end
