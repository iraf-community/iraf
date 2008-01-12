# Table column descriptors

int	ext_type		# Xtension type
int	maxlen			# Maximum nmumber of character in an array
				# column
bool    BIN_DTYNSP		# Binary table, data type not supported

common /ctables/ ext_type, maxlen, BIN_DTYNSP
