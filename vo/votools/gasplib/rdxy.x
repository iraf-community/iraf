include <mach.h>

# RDXY -- procedure to get (x,y) pixel values from a line.
#	  The line  can have any number of columns but only up to 162
#	  characters per line.
#	  
procedure rdxy (line, icol, x, y)

char line[SZ_LINE]	# input line with (x,y) in  it
int  icol[2]		# X, Y column numbers within the line
double	x		# X pixel value
double	y		# Y pixel value

pointer	sp, pp
int	i, ip, ic, ctowrd(), ctod()
int	maxcol, nchar

begin

	# find the right most column to read from buffer
	maxcol = max (icol[1], icol[2])
	
	call smark (sp)
	call salloc (pp, maxcol*MAX_DIGITS, TY_CHAR)

	ip = 1
	ic = pp
	do i = 1, maxcol {
	   nchar = ctowrd (line, ip, Memc[ic], MAX_DIGITS)
	   # store word in equal length array
	   call amovkc (" ", Memc[ic+nchar], MAX_DIGITS-nchar)
	   Memc[ic+MAX_DIGITS] = EOS
	   ic = ic + MAX_DIGITS+1
	}

	# get the output parameters

	nchar = ctod (Memc[pp], (icol(1)-1)*(MAX_DIGITS+1)+1, x)
	nchar = ctod (Memc[pp], (icol(2)-1)*(MAX_DIGITS+1)+1, y)

	call sfree (sp)

end
