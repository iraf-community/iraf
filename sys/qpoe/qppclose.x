# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

# QP_PCLOSE -- Close a parameter opened as a file with QP_POPEN.  This
# differs from a simple call to fio.close in that the lfile used to store
# the parameter data is unlocked, as well as closing the file under FIO.

procedure qp_pclose (fd)

int	fd		#I file descriptor of QP_POPEN-ed parameter

int	lfile, type
pointer	sp, lfname, fm
int	fm_lfparse()

begin
	call smark (sp)
	call salloc (lfname, SZ_FNAME, TY_CHAR)

	call fstats (fd, F_FILENAME, Memc[lfname], SZ_FNAME)
	if (fm_lfparse (Memc[lfname], fm, lfile, type) != ERR)
	    call fm_unlock (fm, lfile)

	call close (fd)
	call sfree (sp)
end
