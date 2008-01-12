# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FM_RENAME -- Rename (or move) a datafile.

procedure fm_rename (old, new)

char	old[ARB], new[ARB]	#I old, new datafile names

begin
	call rename (old, new)
end
