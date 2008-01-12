# CAMERA reader common

int	len_record		# Record length (determined from header)
int	data_type		# Output data type

# Option flags
int	make_image		# Create an IRAF image
int	long_header		# Print a long CAMERA header
int	short_header		# Print a short header (Title and size)
int	lsbf			# Least significant byte first
int	tape			# tape input

common	/rcamcom/ len_record, data_type, make_image, long_header,
	short_header, lsbf, tape
