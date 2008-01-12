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
int	extensions	# Allow for extension tables to be appended to a file.
int	bintable	# Create a BINTABLE extension
int	sdasmgcv	# Flag to convert a multigroup file into one
			# plus extra dimension and an attached table.
int	allgroups	# Indicates to process all the groups with 1 open.
int	first_time	# flag to be used in the table extension code.
int	file_number	# Current tape file numnber
int	ieee		# Flag to have fits data in ieee floatin point format.
#int	force_minmax    # Flag to force recalculation of datamin and datamax
int	ext_type	# Xtension type variable: TABLE, BINTABLE, IMAGE
int	maxlen		# max length for a tbale column
int	def_fmt		# applied default precision format when writing to
			# fits file, if set to YES.
int	dads_values	# YES, No, indicates if DADS keywords are different
			# from 'null'.

common /wfitscom/ bscale, bzero, bitpix, len_record, long_header, short_header,
		  make_image, scale, autoscale, blkfac, extensions, bintable,
		  sdasmgcv, allgroups, first_time, file_number, ieee, 
		  ext_type, maxlen, def_fmt, dads_values

#common /wfitscom/ bscale, bzero, bitpix, len_record, long_header, short_header,
#		  make_image, scale, autoscale, blkfac, extensions, bintable,
#		  sdasmgcv, allgroups, first_time, file_number, ieee, 
#		  force_minmax,ext_type, maxlen, def_fmt

