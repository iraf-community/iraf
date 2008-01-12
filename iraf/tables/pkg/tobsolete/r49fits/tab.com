# Table column descriptors

char	colname[SZ_COLNAME]	# Column name
char	colunits[SZ_COLUNITS]	# Column units
char	colfmt[SZ_COLFMT]	# Column print format
int	datat, lendata, coln
int     wf2_49

int	ext_type		# Xtension type
int	maxlen			# Maximum nmumber of character in an array
				# column
bool    BIN_DTYNSP		# Binary table, data type not supported

common /ctables/ colname, colunits, colfmt, datat, lendata, coln,
		 ext_type, maxlen, BIN_DTYNSP,wf2_49
