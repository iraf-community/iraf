include <pkg/mef.h>

# MEF_GNBC -- Get the Number of Blank Cards in a FITS header pointed by
# mef. This is the number of cards available to insert before an expantion by
# one block is required. If the header has not being read and EOF (-2) is
# returned.

int procedure mef_gnbc (mef)

pointer mef

int	len, hd, ip, nbc, hsize, k, ncards
int	strlen(), strncmp()

begin
	if (MEF_HDRP(mef) == NULL)
	    return (EOF)
	    
	hd = MEF_HDRP(mef)
	len = strlen(Memc[hd])

	# Go to the end of buffer and get last line

	ip = hd + MEF_HSIZE(mef) - LEN_CARDNL
	
	# See if line is blank

	nbc = 0
	while (ip > 0) {
	    do k = 0, LEN_CARD-1 
		if (Memc[ip+k] != ' ')
		    break

	    if (k != LEN_CARD && k != 0)   # blank keyw card
		break
	    else if (k == 0) {
		if (strncmp ("END     ", Memc[ip], 8) == 0) {
	    	    ip = ip - LEN_CARDNL
		    next
		} else
		    break
	    } else
		nbc = nbc + 1
	    ip = ip - LEN_CARDNL
	}

	hsize = MEF_HSIZE(mef)
	ncards = (hsize + 80)/81
	
	ncards = ((ncards + 35)/36)*36 - ncards
	nbc = nbc + ncards

	return (nbc)
end

