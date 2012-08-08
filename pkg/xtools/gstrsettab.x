# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GSTRSETTAB -- Procedure to set tabs, using the integer array tabs.
# The first tabstop is set at first_tabstop, with subsequent tabstops
# at tabsize intervals.

procedure gstrsettab (tabs, maxtabs, first_tabstop, tabsize)

int   tabs[ARB], first_tabstop, tabsize
int   i, maxtabs

begin
	for (i=1; i <= maxtabs; i = i + 1) {
	    if (i < first_tabstop)
		tabs[i] = NO
	    else if (i == first_tabstop)
		tabs[i] = YES
	    else if (mod ((i - first_tabstop), tabsize) == 0)
		tabs[i] = YES
	    else
		tabs[i] = NO
	}
end
