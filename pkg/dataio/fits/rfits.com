
# FITS reader common

int	len_record		# Length of FITS records in bytes
int	data_type		# Output data type
real	blank			# Blank value

# Option flags
int	make_image		# Create an IRAF image
int	long_header		# Print a long header (FITS header cards)
int	short_header		# Print a short header (Title and size)
int	scale			# Scale the data
int	old_name		# Use old IRAF name?

common	/rfitscom/ len_record, data_type, blank, make_image, long_header,
	short_header, scale, old_name
