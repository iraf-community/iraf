# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<fmlfstat.h>
include	<mach.h>
include	"fmset.h"

# ZZDEBUG.X -- Debug routines for the FMIO package.

task	create	= t_create,
	enter	= t_enter,
	extract	= t_extract,
	mkfile	= t_mkfile,
	type	= t_type,
	show	= t_show,
	copy	= t_copy,
	rebuild	= t_rebuild,
	fcache	= t_fcache


# CREATE -- Create a new, empty datafile.

procedure t_create()

pointer	fm
char	datafile[SZ_FNAME]
int	pagesize, nlfiles, maxptpages

int	clgeti()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	pagesize   = clgeti ("pagesize")
	nlfiles    = clgeti ("nlfiles")
	maxptpages = clgeti ("maxptpages")

	fm = fm_open (datafile, NEW_FILE)
	if (pagesize > 0)
	    call fm_seti (fm, FM_PAGESIZE, pagesize)
	if (nlfiles > 0)
	    call fm_seti (fm, FM_MAXLFILES, nlfiles)
	if (maxptpages > 0)
	    call fm_seti (fm, FM_MAXPTPAGES, maxptpages)
	call fm_close (fm)
end


# ENTER -- Copy a regular disk file into an lfile.

procedure t_enter()

pointer	fm
int	lfile, type, fd, lf
char	datafile[SZ_FNAME], fname[SZ_FNAME]
int	clgeti(), open(), fm_fopen(), access()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	call clgstr ("fname", fname, SZ_FNAME)
	lfile = clgeti ("lfile")

	if (access (fname, 0, TEXT_FILE) == YES)
	    type = TEXT_FILE
	else
	    type = BINARY_FILE

	fm = fm_open (datafile, READ_WRITE)
	fd = open (fname, READ_ONLY, type)
	lf = fm_fopen (fm, lfile, NEW_FILE, type)
	call fcopyo (fd, lf)

	call close (lf)
	call close (fd)
	call fm_close (fm)
end


# EXTRACT -- Copy an lfile out into a disk file.

procedure t_extract()

pointer	fm
int	lfstat[LEN_LFSTAT]
int	lfile, type, fd, lf
char	datafile[SZ_FNAME], fname[SZ_FNAME]
int	clgeti(), open(), fm_fopen(), fm_lfstat()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	call clgstr ("fname", fname, SZ_FNAME)
	lfile = clgeti ("lfile")

	fm = fm_open (datafile, READ_ONLY)

	if (fm_lfstat (fm, lfile, lfstat) == ERR)
	    call error (1, "cannot stat lfile")
	else if (and (LFU_FLAGS(lfstat), LFB_TEXTFILE) != 0)
	    type = TEXT_FILE
	else
	    type = BINARY_FILE

	lf = fm_fopen (fm, lfile, READ_ONLY, type)
	fd = open (fname, NEW_FILE, type)
	call fcopyo (lf, fd)

	call close (lf)
	call close (fd)
	call fm_close (fm)
end


# MKFILE -- Create a file of the given size (in kilobytes) containing all
# zero data.

procedure t_mkfile()

pointer	fm
int	lfile, lf, kb, i
char	datafile[SZ_FNAME], buf[1024/SZB_CHAR]
int	clgeti(), fm_fopen()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	lfile = clgeti ("lfile")
	kb = clgeti ("kb")

	fm = fm_open (datafile, READ_WRITE)
	lf = fm_fopen (fm, lfile, NEW_FILE, BINARY_FILE)

	do i = 1, kb
	    iferr (call write (lf, buf, 1024/SZB_CHAR)) {
		call erract (EA_WARN)
		break
	    }

	call close (lf)
	call fm_close (fm)
end


# TYPE -- Print the contents of an lfile on the standard output.

procedure t_type()

pointer	fm
int	lfile, fd
char	datafile[SZ_FNAME]
int	clgeti(), fm_fopen()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	lfile = clgeti ("lfile")

	fm = fm_open (datafile, READ_WRITE)
	fd = fm_fopen (fm, lfile, READ_ONLY, TEXT_FILE)
	call fcopyo (fd, STDOUT)

	call close (fd)
	call fm_close (fm)
end


# SHOW -- Print the datafile status.

procedure t_show()

pointer	fm
char	datafile[SZ_FNAME]
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)

	fm = fm_open (datafile, READ_WRITE)
	call fm_debug (fm, STDOUT, FMD_ALL)
	call fm_close (fm)
end


# COPY -- Copy a datafile.

procedure t_copy()

char	df_src[SZ_FNAME]
char	df_dst[SZ_FNAME]

begin
	call clgstr ("source", df_src, SZ_FNAME)
	call clgstr ("destination", df_dst, SZ_FNAME)
	call fm_copy (df_src, df_dst)
end


# REBUILD -- Rebuild a datafile.

procedure t_rebuild()

char	datafile[SZ_FNAME]

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	call fm_rebuild (datafile)
end


# Test the file cache package.
# -------------------------------

define	GETFD		1
define	RETFD		2
define	LOCKOUT		3
define	UNLOCK		4
define	LOCKED		5
define	SYNC		6
define	DEBUG		7
#
define	FCDEBUG		9
define	PFILE		10
define	BYE		11

define	KEYWORDS	"|getfd|retfd|lockout|unlock|locked|sync|debug|\
			 |fcdebug|pfile|bye|"


# FCACHE -- Test the file cache package.

procedure t_fcache()

pointer	fm
int	lfile, mode, type, fd
char	datafile[SZ_FNAME], keyword[SZ_FNAME], junk[SZ_FNAME]
int	strdic(), fscan(), fm_getfd()
bool	fm_locked()
pointer	fm_open()

begin
	call clgstr ("datafile", datafile, SZ_FNAME)
	fm = fm_open (datafile, READ_WRITE)

	call printf ("* ")
	call flush (STDOUT)
	while (fscan (STDIN) != EOF) {
	    call gargwrd (keyword, SZ_FNAME)
	    if (IS_ALPHA(keyword[1]))
		switch (strdic (keyword, junk, SZ_FNAME, KEYWORDS)) {
		case GETFD:
		    call gargi (lfile)
		    call gargi (mode)
		    call gargi (type)
		    iferr (fd = fm_getfd (fm, lfile, mode, type))
			call erract (EA_WARN)
		    else {
			call printf ("fd = %d\n")
			    call pargi (fd)
		    }
		case RETFD:
		    call gargi (lfile)
		    call fm_retfd (fm, lfile)

		case LOCKOUT:
		    call gargi (lfile)
		    iferr (call fm_lockout (fm, lfile))
			call erract (EA_WARN)
		case UNLOCK:
		    call gargi (lfile)
		    iferr (call fm_unlock (fm, lfile))
			call erract (EA_WARN)
		case LOCKED:
		    call gargi (lfile)
		    call printf ("locked = %b\n")
			call pargb (fm_locked (fm, lfile))

		case SYNC:
		    call fm_fcsync (fm)
		case DEBUG:
		    call fm_debug (fm, STDOUT, FMD_ALL)
		case FCDEBUG:
		    call fm_fcdebug (fm, STDOUT, FCD_ALL)
		case PFILE:
		    call gargi (lfile)
		    fd = fm_getfd (fm, lfile, READ_ONLY, TEXT_FILE)
		    iferr (call fcopyo (fd, STDOUT))
			call erract (EA_WARN)
		    call fm_retfd (fm, lfile)
		case BYE:
		    break
		default:
		    call eprintf ("commands: %s\n")
			call pargstr (KEYWORDS)
		}

	    call printf ("* ")
	    call flush (STDOUT)
	}

	call fm_close (fm)
end
