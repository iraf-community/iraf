# FITS common block

double	bscale		# FITS scaling factor
double	bzero		# FITS offset factor
int	bitpix		# Output bits per pixel
int	len_record	# Record length in FITS bytes
int	long_header	# Print long header?
int	short_header	# Print short header?
int	make_image	# Make a FITS image?
int	scale		# Scale the data with bzero and bscale?
int	autoscale	# Allow program to calculate bscale and bzero?
int	blkfac		# FITS tape blocking factor

common /wfitscom/ bscale, bzero, bitpix, len_record, long_header, short_header,
		  make_image, scale, autoscale, blkfac
