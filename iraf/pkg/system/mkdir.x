# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MKDIR -- Make a directory. (May be an IRAF VFN or an OS directory spec)

procedure t_mkdir ()

char	newdir[SZ_FNAME]

begin
	call clgstr ("newdir", newdir, SZ_FNAME)
	call fmkdir (newdir)
end
