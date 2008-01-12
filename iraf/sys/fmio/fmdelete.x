# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FM_DELETE -- Delete a datafile.

procedure fm_delete (dfname)

char	dfname[ARB]		#I datafile name

begin
	call delete (dfname)
end
