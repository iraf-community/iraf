task	ttt

# Example task for creating a table from a text file.  The text file
# is assumed to contain five columns per row, containing an integer
# catalog number, a text string, right ascension, declination, and
# magnitude.  An example of three lines from the text file could be:
#
#    0172  abcd   0:01:17.865  -89:43:17.62   14.7
#    0213  "a b"  0:02:29.775  -84:43:17.64   12.8
#    0490  ""     2:19:21.000  -84:46:22.98   11.5

define	NCOLS		5	# number of columns to create
# column numbers:
define	ID		1	# catalog ID
define	NAME		2	# name
define	RA		3	# right ascension
define	DEC		4	# declination
define	MAG		5	# magnitude

procedure ttt()

char	input[SZ_FNAME]		# name of input file
char	outtable[SZ_FNAME]	# name of output table
#--
pointer tp			# pointer to table struct
pointer cp[NCOLS]		# pointers to column info
pointer tbtopn()

char	name[SZ_FNAME]		# star name
double	ra, dec			# coordinates
double	mag			# magnitude
int	cat_id			# catalog ID
int	row			# row number

char	lbuf[SZ_LINE]		# buffer for reading from input
int	ip, ctoi(), ctod(), ctowrd()
int	fd			# fd for input file
int	open(), getline()

errchk	tbtopn, open

begin
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("outtable", outtable, SZ_FNAME)

	# Open output table (file not created yet, though).
	tp = tbtopn (outtable, NEW_FILE, NULL)		# NULL --> no template

	# Define columns.  The "Name" column is a string up to 20 char long.
	call tbcdef (tp, cp[ID], "catalog_ID", "", "%8d", TY_INT, 1, 1)
	call tbcdef (tp, cp[NAME], "Name", "", "", -20, 1, 1)
	call tbcdef (tp, cp[RA], "RA", "hours", "%12.2h", TY_DOUBLE, 1, 1)
	call tbcdef (tp, cp[DEC], "DEC", "degrees", "%12.1h", TY_DOUBLE, 1, 1)
	call tbcdef (tp, cp[MAG],    "V",    "Vmag",  "%6.2f", TY_REAL, 1, 1)
	#                 ^           ^         ^        ^        ^
	#               output     col name   units    format  datatype

	# Create the output table file.
	call tbtcre (tp)

	# Add a history record.
	call tbhadt (tp, "history", "created as an example")

	# Open the input file.
	fd = open (input, READ_ONLY, TEXT_FILE)

	# Read each line from the input file.
	row = 0					# initialize
	while (getline (fd, lbuf) != EOF) {

	    if (lbuf[1] == '#' || lbuf[1] == '\n' || lbuf[1] == EOS)
		next				# ignore comment or blank lines

	    ip = 1				# beginning of line

	    # Read the catalog ID.
	    if (ctoi (lbuf, ip, cat_id) < 1)
		next				# ignore comment or bad line

	    # Read the star name.
	    if (ctowrd (lbuf, ip, name, SZ_FNAME) < 1)
		name[1] = EOS

	    # Read the right ascension and declination.
	    if (ctod (lbuf, ip, ra) < 1)
		call error (1, "can't read right ascension")
	    if (ctod (lbuf, ip, dec) < 1)
		call error (1, "can't read declination")

	    # Read the magnitude (may be missing).
	    if (ctod (lbuf, ip, mag) < 1)
		mag = INDEFD

	    row = row + 1			# increment row number

	    # Write the information to the table.  The "Vmag" column is
	    # real in the table, but we're passing it a double, so we use
	    # tbeptd instead of tbeptr.
	    call tbepti (tp, cp[ID], row, cat_id)
	    call tbeptt (tp, cp[NAME], row, name)
	    call tbeptd (tp, cp[RA], row, ra)
	    call tbeptd (tp, cp[DEC], row, dec)
	    call tbeptd (tp, cp[MAG], row, mag)
	}

	# Close input file and output table.
	call close (fd)
	call tbtclo (tp)
end
