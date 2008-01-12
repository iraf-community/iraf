# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FM_ACCESS -- Test to see if the name FMIO datafile exists and is accessible
# with the given permissions.

int procedure fm_access (dfname, mode)

char	dfname[ARB]		#I datafile name
int	mode			#I access mode (0 to just test existence)

int	access()

begin
	return (access (dfname, mode, 0))
end
