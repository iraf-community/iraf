# IPFCN.H - Include file for the special functions supported by the IMPORT task.

# Format database functions.
define	DB_FUNCTIONS	"|ctocc|ctod|ctoi|ctol|ctor|ctowrd|\
			 |getstr|getb|getu|geti|geti2|geti4|\
			 |getr|getr4|getr8|getn|getn4|getn8|\
			 |locate|line|skip|bswap|parameter|default|\
			 |lsb_host|msb_host|substr|stridx|"

define	CTOCC		1	# Convert character to printable char constant
define	CTOD		2	# Convert string to double precision real
define	CTOI		3 	# Convert string to integer
define	CTOL		4 	# Convert string to long
define	CTOR		5 	# Convert string to single precision real
define	CTOWRD		6 	# Return 1st white-space delimited word from str
# newline
define	GETSTR		8 	# Get a string at offset
define	GETB		9 	# Get a byte at offset
define	GETU		10 	# Get an unsigned short int at offset
define	GETI		11 	# Get a signed int at offset
define	GETI2		12 	# Get a signed int at offset
define	GETI4		13 	# Get a long signed int at offset
# newline
define	GETR		15 	# Get an IEEE fp number at offset
define	GETR4		16 	# Get an IEEE fp number at offset
define	GETR8		17 	# Get an IEEE double precision number at offset
define	GETN		18 	# Get a native fp number at offset
define	GETN4		19 	# Get a native fp number at offset
define	GETN8		20 	# Get a native double precision number at offset
# newline
define	LOCATE		22	# Compute an offset
define	LINE		23	# Offset of line N
define	SKIP		24	# Move offset N-bytes
define	BSWAP		25	# Byte swap the argument
define	PARAMETER	26	# Return current task parameter
define	DEFAULT		27	# Return default task parameter
# newline
define	LSB_HOST	29	# Host is LSB byte ordered machine
define	MSB_HOST	30	# Host is MSB byte ordered machine
define	SUBSTR		31	# Return a substring of the argument
define	STRIDX		32	# Return occurance of a char within a string


# Outbands expression functions.
define	OB_FUNCTIONS	"|gray|grey|flipx|flipy|\
		 	 |red|green|blue|"

define	GRAY		1	# Convert to NTSC grayscale
define	GREY		2	# Convert to NTSC grayscale (alias)
define	FLIPX		3	# Flip image in X
define	FLIPY		4	# Flip image in Y
# newline
define	RED		6	# Get red component of colormap image
define	GREEN		7	# Get green component of colormap image
define	BLUE		8	# Get blue component of colormap image


