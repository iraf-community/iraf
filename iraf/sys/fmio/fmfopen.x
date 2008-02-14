# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FM_FOPEN -- Open an lfile as an FIO file.  A FIO file descriptor is returned
# for the open file.  CLOSE is used to close the opened file.

int procedure fm_fopen (fm, lfile, mode, type)

pointer	fm			#I FMIO descriptor
int	lfile			#I lfile to be opened
int	mode			#I file access mode
int	type			#I logical file type

int	fd
pointer	sp, lfname
extern	fm_lfopen(), fm_lfclose()
extern	fm_lfaread(), fm_lfawrite(), fm_lfawait(), fm_lfstati()
int	fopnbf()
errchk	fopnbf

begin
	call smark (sp)
	call salloc (lfname, SZ_FNAME, TY_CHAR)

	call fm_lfname (fm, lfile, type, Memc[lfname], SZ_FNAME)
	fd = fopnbf (Memc[lfname], mode, fm_lfopen, fm_lfaread, fm_lfawrite,
	    fm_lfawait, fm_lfstati, fm_lfclose)

	call sfree (sp)
	return (fd)
end
