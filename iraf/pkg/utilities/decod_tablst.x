# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>

define	DEFAULT_FSTSTOP	1
define	DEFAULT_TABSIZE	8

# DECODE_TABLIST -- Procedure to decode a string containing a list of
# tabstops into an integer array of tabs. A tabstop is indicated by
# YES. The string may be blank in which case the default
# tabsize is 8, a series of 1,2 or more arguments separated by blanks
# and/or commas, or two arguments of the form m +n where m specifies
# the first tabstop and n the tabsize.

int procedure decode_tablist (tab_list, tabs, maxtabs)

char	tab_list[ARB]
int	tabs[ARB]
int	maxtabs

bool	noarg, plusarg
int	ip, tp, narg, first_tabstop, tabsize, tabstop

int	decode_tabstop(), strlen(), stridxs(), ctoi()

errchk	decode_tabstop, gstrsettab

begin
	ip = 1

	for (tp = 1;  tp <= maxtabs;  tp = tp+1)
	    tabs[tp] = NO

	noarg = true
	plusarg = false

	if (strlen (tab_list) != 0)
	    noarg = false
	if (stridxs ("+", tab_list) != 0)
	    plusarg = true
	
	for (narg = 1;  narg <= maxtabs;  narg = narg + 1) {

	    while (IS_WHITE(tab_list[ip]) || tab_list[ip] == ',')
		ip = ip + 1

	    if (tab_list[ip] == EOS)
		if (noarg) {
		    first_tabstop = DEFAULT_FSTSTOP
		    tabsize = DEFAULT_TABSIZE
		    break
		} else
		    return (OK)

	    if (plusarg) {

		if (narg == 1) {
		    if (decode_tabstop (tab_list, ip, tabs, maxtabs,
			first_tabstop) == ERR)
			return (ERR)
		} else if (narg == 2) {
		    ip = ip + 1
		    if (ctoi (tab_list, ip, tabsize) == 0)
			return (ERR)
		    else
			break
		} else
		    return (ERR)

	    } else if (decode_tabstop (tab_list, ip, tabs, maxtabs, 
		tabstop) == ERR) {

		return (ERR)
	    }
	}

	if (noarg || plusarg)
	    call gstrsettab (tabs, maxtabs, first_tabstop, tabsize)

	return (OK)
end


# DECODE_TABSTOP -- Procedure to decode tabstops

int procedure  decode_tabstop (tab_list, ip, tabs, maxtabs, tabstop)

char	tab_list[ARB]
int	maxtabs
int	ip
int	tabs[ARB]
int	tabstop

int	ctoi()

begin
	if (ctoi (tab_list, ip, tabstop) == 0)
	    return (ERR)
	else if (tabstop <= maxtabs) {
	    tabs[tabstop] = YES
	    return (OK)
	} else
	    return (ERR)
end
