include	"../lib/apseldef.h"

# DP_TADINIT -- Initializie the column descriptors for the input ST photometry
# table.

procedure dp_tadinit (tp, column)

pointer	tp			# table descriptor
int	column[ARB]		# column pointer array

begin
	# Find the column pointers
	call tbcfnd (tp, ID, column[1], 1)
	if (column[1] == NULL)
	    call tbcfnd (tp, "ID", column[1], 1)
	if (column[1] == NULL)
	    call printf ("Error reading ID.\n")

	call tbcfnd (tp, XCENTER, column[2], 1)
	if (column[2] == NULL)
	    call tbcfnd (tp, "XCENTER", column[2], 1)
	if (column[2] == NULL)
	    call printf ("Error reading XCENTER.\n")

	call tbcfnd (tp, YCENTER, column[3], 1)
	if (column[3] == NULL)
	    call tbcfnd (tp, "YCENTER", column[3], 1)
	if (column[3] == NULL)
	    call printf ("Error reading YCENTER.\n")

	call tbcfnd (tp, MAG, column[4], 1)
	if (column[4] == NULL)
	    call tbcfnd (tp, APMAG, column[4], 1)
   	if (column[4] == NULL)
	    call printf ("Error reading MAG.\n")
end


# DP_TADREAD -- Read a record from the input ST photometry table.

procedure dp_tadread (tp, column, id, x, y, mag, row)

pointer	tp			# table descriptor
int	column[ARB]		# column pointer array
int	id			# output id
real	x			# output x value
real	y			# output y value
real	mag			# output magnitude
int	row			# integer row

bool	nullflag

begin
	call tbrgti (tp, column[1], id, nullflag, 1, row)
	if (nullflag)
	    id = 0
	call tbrgtr (tp, column[2], x, nullflag, 1, row)
	call tbrgtr (tp, column[3], y, nullflag, 1, row)
	call tbrgtr (tp, column[4], mag, nullflag, 1, row)
end


# DP_GCOORDS -- Read the coordinates and magnitudes from a simple text file.

int procedure dp_gcoords (cl, x, y, mag, id)

int	cl			# file descriptor
real	x			# x coordinate centers
real	y			# y coordinate centers
real	mag			# magnitudes
int	id			# id of the star

int	fscan(), nscan()

begin
	while (fscan (cl) != EOF) {
	    call gargr (x)
	    call gargr (y)
	    call gargr (mag)
	    if (nscan () < 3)
		next
	    call gargi (id)
	    if (nscan() < 4)
		id = 0
	    return (1)
	}

	return (EOF)
end
