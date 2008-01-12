include <ctype.h>
include "gf.h"

# GF_GET_GNUM -- Get group number embedded in a kernel section

int procedure gf_get_gnum (ksection)

char	ksection[ARB]		# i: kernel section
#--
int	ic, nc, ilen, istart, gnum
pointer	sp, number

int	ctoi()

begin
	call gf_find_gnum (ksection, istart, ilen)

	if (ilen <= 0)
	    return (-1)

	call smark (sp)
	call salloc (number, ilen, TY_CHAR)

	call strcpy (ksection[istart], Memc[number], ilen)

	ic = 1
	nc = ctoi (Memc[number], ic, gnum)

	call sfree (sp)
	return (gnum)
end

# GF_SWAP_GNUM -- Change the group number in the kernel section

procedure gf_swap_gnum (gnum, ksection, maxch) 

int	gnum			# i: group number
char	ksection[ARB]		# u: kernel section
int	maxch			# i: max length of section
#--
int	istart, ilen
pointer	sp, newsect

begin
	call smark (sp)
	call salloc (newsect, maxch, TY_CHAR)

	call gf_find_gnum (ksection, istart, ilen)

	if (ilen > 0) {
	    ksection[istart] = EOS

	    call sprintf (Memc[newsect], maxch, "%s%d%s")

	    call pargstr (ksection)
	    call pargi (gnum)
	    call pargstr (ksection[istart+ilen])

	    call strcpy (Memc[newsect], ksection, maxch)
	}

	call sfree (sp)
end

# GF_FIND_GNUM -- Find an integer string in a kernel section

procedure gf_find_gnum (ksection, istart, ilen)

char	ksection[ARB]		# i: kernel section
int	istart			# o: starting index of integer
int	ilen			# o: length of integer
#--
bool	two
int	ic

begin
	two = false
	istart = 0
	ilen = 0

	for (ic = 1; ksection[ic] != EOS; ic = ic + 1) {
	    if (IS_DIGIT(ksection[ic])) {
		if (istart == 0) {
		    istart = ic
		} else if (ilen != 0) {
		    two = true
		}

	    } else {
		if (istart != 0 && ilen == 0)
		    ilen = ic - istart
	    }
	}

	if (two) {
	    istart = 0
	    ilen = 0
	}
end
