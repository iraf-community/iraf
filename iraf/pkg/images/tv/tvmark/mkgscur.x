include <gset.h>
include <fset.h>

# MK_GSCUR -- Procedure to fetch x and y positions from a file and move
# the cursor to those positions.

int procedure mk_gscur (sl, gd, xcur, ycur, label, prev_num, req_num, num)

pointer	sl		# pointer to text file containing cursor coords
pointer	gd		# pointer to graphics stream
real	xcur, ycur	# x cur and y cur
char	label[ARB]	# label string
int	prev_num	# previous number
int	req_num		# requested number
int	num		# list number

int	stdin, nskip, ncount
pointer	sp, fname
int	fscan(), nscan(), strncmp()
errchk	greactivate, gdeactivate, gscur

begin
	if (sl == NULL)
	    return (EOF)

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Find the number of objects to be skipped.
	call fstats (sl, F_FILENAME, Memc[fname], SZ_FNAME)
	if (strncmp ("STDIN", Memc[fname], 5) == 0) {
	    stdin = YES
	    nskip = 1
	} else {
	    stdin = NO
	    if (req_num <= prev_num) {
		call seek (sl, BOF)
		nskip = req_num
	    } else
		nskip = req_num - prev_num
	}

	ncount = 0
	num = prev_num
	repeat {

	    # Print the prompt if file is STDIN.
	    if (stdin == YES) {
		call printf ("Type object x and y coordinates: ")
		call flush (STDOUT)
	    }

	    # Fetch the coordinates.
	    if (fscan (sl) != EOF) {
		call gargr (xcur)
		call gargr (ycur)
		call gargwrd (label, SZ_LINE)
		if (nscan () >= 2) {
		    ncount = ncount + 1
		    num = num + 1
		}
	    } else 
		ncount = EOF

	    # Move the cursor.
	    if (gd != NULL && (ncount == nskip || ncount == EOF)) {
		iferr {
		    call greactivate (gd, 0)
	            call gscur (gd, xcur, ycur)
		    call gdeactivate (gd, 0)
		} then
		    ;
	    }

	} until (ncount == EOF || ncount == nskip)

	call sfree (sp)

	if (ncount == EOF) {
	    return (EOF)
	} else if (nskip == req_num) {
	    num = ncount
	    return (ncount)
	} else {
	    return (num)
	}
end
