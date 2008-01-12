# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<chars.h>
include	"tty.h"

.help index caplist
.nf _________________________________________________________________________
TTY_INDEX_CAPS -- Prepare an index into the caplist string, stored in
the tty descriptor.  Each two character capability name maps into a unique
integer code, called the capcode.  We prepare a list of capcodes, keeping
only the first such code encountered in the case of multiple entries.
The offset of the capability in the caplist string is associated with each
capcode.  When these lists have been prepared, they are sorted to permit
a binary search for capabilities at runtime.
.endhelp ____________________________________________________________________

procedure tty_index_caps (tty, t_capcode, t_capindex, ncaps)

pointer	tty
int	t_capcode[ARB], t_capindex[ARB]
int	ncaps

pointer	ip, caplist
int	i, swap, capcode, temp
int	tty_encode_capability()
pointer	coerce()
errchk	syserr

begin
	caplist = coerce (tty + T_OFFCAP, TY_STRUCT, TY_CHAR)
	ip = caplist

	# Scan the caplist and prepare the capcode and capindex lists.
	for (ncaps=0;  ncaps <= MAX_CAPS;  ) {
	    # Advance to the next capability field.  Normal exit occurs
	    # when ':' is followed immediately by EOS.

	    while (Memc[ip] != ':' && Memc[ip] != EOS)
		ip = ip + 1
	    if (Memc[ip+1] == EOS || Memc[ip] == EOS)
		break

	    ip = ip + 1					# skip the ':'
	    capcode = tty_encode_capability (Memc[ip])

	    # Is the capcode already in the list?  If not found, add it to
	    # the list.
	    for (i=1;  i <= ncaps && t_capcode[i] != capcode;  i=i+1)
		;
	    if (i > ncaps) {				# not found
		ncaps = ncaps + 1
		t_capcode[ncaps] = capcode
		t_capindex[ncaps] = ip - caplist + 1
	    }
	}

	if (ncaps > MAX_CAPS)
	    call syserr (SYS_TTYOVFL)

	# A simple interchange sort is sufficient here, even though it would
	# not be hard to interface to qsort.  The longest termcap entries are
	# about 50 caps, and the time req'd to sort such a short list is
	# negligible compared to the time spent searching the termcap file.

	if (ncaps > 1)
	    repeat {
		swap = 0
		do i = 1, ncaps-1
		    if (t_capcode[i] > t_capcode[i+1]) {
			temp = t_capcode[i]
			t_capcode[i] = t_capcode[i+1]
			t_capcode[i+1] = temp
			temp = t_capindex[i]
			t_capindex[i] = t_capindex[i+1]
			t_capindex[i+1] = temp
			swap = 1
		    }
	    } until (swap == 0)
end


# TTY_FIND_CAPABILITY -- Search the caplist for the named capability.
# If found, return the char pointer IP to the first char of the value field,
# and YES as the function value.  If the first char in the capability string
# is '@', the capability "is not present".

int procedure tty_find_capability (tty, cap, ip)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character name of capability
pointer	ip			# pointer to capability string

int	capcode, capnum
int	tty_binsearch(), tty_encode_capability()
pointer	coerce()
errchk	syserr

begin
	if (tty == NULL)
	    call syserr (SYS_TTYINVDES)

	capcode = tty_encode_capability (cap)
	capnum = tty_binsearch (capcode, T_CAPCODE(tty), T_NCAPS(tty))

	if (capnum > 0) {
	    # Add 2 to skip the two capname chars.
	    ip = coerce (tty + T_OFFCAP, TY_STRUCT, TY_CHAR) +
		T_CAPINDEX(tty+capnum-1) - 1 + 2
	    if (Memc[ip] != '@')
		return (YES)
	}

	return (NO)
end


# TTY_BINSEARCH -- Perform a binary search of the capcode array for the
# indicated capability.  Return the array index of the capability if found,
# else zero.

int procedure tty_binsearch (capcode, t_capcode, ncaps)

int	capcode
int	t_capcode[ARB], ncaps
int	low, high, pos, ntrips

begin
	low = 1
	high = ncaps
	pos = 0

	# Cut range of search in half until code is found, or until range
	# vanishes (high - low <= 1).  If neither high or low is the one,
	# code is not found in the list.

	do ntrips = 1, ncaps {
	    pos = (high - low) / 2 + low
	    if (t_capcode[low] == capcode)
		return (low)
	    else if (t_capcode[high] == capcode)
		return (high)
	    else if (pos == low)			# (high-low)/2 == 0
		return (0)				# not found
	    else if (t_capcode[pos] < capcode)
		low = pos
	    else
		high = pos
	} 

	# Search cannot fail to converge unless there is a bug in the software
	# somewhere.

	call syserr (SYS_TTYBINSRCH)
end


# TTY_ENCODE_CAPABILITY -- Encode the two character capability string
# as a unique integer value.

int procedure tty_encode_capability (cap)

char	cap[ARB]

begin
	return (ENCODE(cap))
end
