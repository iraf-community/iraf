# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<error.h>
include	<syserr.h>
include	<config.h>
include	<fset.h>
include	<diropen.h>
include	<fio.h>

define	MAX_OPENDIR	20

# DIROPEN -- Open a directory file for reading.  Directories are opened
# as read only text files.  Writing, seeking, etc. are not permitted.
# The machine dependent OSFN's returned by the kernel are converted to
# VFN's and hidden files are skipped.  Skipping of hidden files may be
# overriden (i.e., all filenames may be passed) as a option.

int procedure diropen (fname, mode)

char	fname[ARB]		# directory file to be opened
int	mode			# pass or skip hidden filenames

int	fd, dirf
bool	first_time
int	dirmode[MAX_OPENDIR]
int	oschan[MAX_OPENDIR]
pointer	vfnptr[MAX_OPENDIR], vp, sp, osfn

pointer	vfnopen()
int	fopntx(), fstati(), errcode()
extern	fopdir(), fgtdir(), fptdir(), ffldir(), fstdir(), fcldir()
extern	fskdir(), fntdir()
errchk	fopntx, vfnopen, syserrs
common	/dircom/ dirmode, oschan, vfnptr
data	first_time /true/

begin
	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	# Free up all descriptor slots.
	if (first_time) {
	    do dirf = 1, MAX_OPENDIR
		oschan[dirf] = 0
	    first_time = false
	}

	# The file name must be mapped explicitly because FIO will not map
	# filenames opened on special devices (when FOPNTX is called).

	call fmapfn (fname, Memc[osfn], SZ_PATHNAME)
	call strupk (Memc[osfn], Memc[osfn], SZ_PATHNAME)

	# Open the VFN database, used to unmap filenames.
	vp = vfnopen (Memc[osfn], VFN_UNMAP)

	# Open the file.  We call FIO which eventually calls FOPDIR.

	iferr {
	    fd = fopntx (Memc[osfn], READ_ONLY,
		fopdir, fgtdir, fptdir, ffldir, fstdir, fcldir, fskdir, fntdir)
	} then {
	    call vfnclose (vp, VFN_NOUPDATE)
	    if (errcode() == SYS_FOPENDEV)
		call syserrs (SYS_FOPENDIR, fname)
	    else
		call erract (EA_ERROR)
	}

	# Get the channel number (index into dirmode and oschan) assigned
	# by FOPDIR.  Save the mode and vp for later.
	
	dirf = fstati (fd, F_CHANNEL)
	dirmode[dirf] = mode
	vfnptr[dirf]  = vp

	call sfree (sp)
	return (fd)
end


# FOPDIR -- Open a directory; this is the "zopntx" routine called by FIO.
# Allocate a directory descriptor and call the kernel to physically open
# the directory.

procedure fopdir (osfn, mode, channel)

char	osfn[ARB]		# packed OS filename of directory file
int	mode			# file access mode (always read_only)
int	channel			# we return index into oschan

int	dirf
int	dirmode[MAX_OPENDIR]
int	oschan[MAX_OPENDIR]
int	vfnptr[MAX_OPENDIR]
common	/dircom/ dirmode, oschan, vfnptr

begin
	channel = ERR
	if (mode != READ_ONLY)
	    return

	# Allocate a descriptor.
	for (dirf=1;  dirf <= MAX_OPENDIR;  dirf=dirf+1)
	    if (oschan[dirf] == 0)
		break

	# Open the physical directory file and return directory file index
	# as the channel number.  Free the slot if ZOPDIR returns ERR.

	if (dirf <= MAX_OPENDIR) {
	    call zopdir (osfn, oschan[dirf])
	    if (oschan[dirf] == ERR)
		oschan[dirf] = 0
	    else
		channel = dirf
	}
end


# FCLDIR -- Close a directory previously opened with FOPDIR.

procedure fcldir (channel, status)

int	channel			# index into oschan
int	status

int	dirmode[MAX_OPENDIR]
int	oschan[MAX_OPENDIR]
int	vfnptr[MAX_OPENDIR]
common	/dircom/ dirmode, oschan, vfnptr

begin
	if (channel < 1 || channel > MAX_OPENDIR)
	    status = ERR
	else if (oschan[channel] == 0)
	    status = ERR
	else {
	    call zcldir (oschan[channel], status)
	    oschan[channel] = 0
	    iferr (call vfnclose (vfnptr[channel], VFN_NOUPDATE))
		status = ERR
	}
end


# FGTDIR -- Get the next "line of text", i.e. VFN, from a directory.  Since we
# are being accessed as a text file we must return an unpacked string delimited
# by a newline.  OS filenames are converted to virtual filenames and hidden
# files are skipped if desired.  Raw mode is not supported.

procedure fgtdir (chan, outline, maxch, status)

int	chan			# oschan index
char	outline[maxch]		# buffer which receives the VFN
int	maxch			# maxchars to return
int	status

int	nchars
pointer	vp, sp, osfn
int	dirmode[MAX_OPENDIR]
int	oschan[MAX_OPENDIR]
int	vfnptr[MAX_OPENDIR]

int	vfnunmap(), vfn_is_hidden_file()
errchk	vfnunmap, vfn_is_hidden_file
common	/dircom/ dirmode, oschan, vfnptr
define	done_ 91

begin
	call smark (sp)
	call salloc (osfn, SZ_FNAME, TY_CHAR)

	status = ERR
	if (chan < 1 || chan > MAX_OPENDIR)
	    goto done_
	if (oschan[chan] == 0)
	    goto done_
	vp = vfnptr[chan]

	repeat {
	    call zgfdir (oschan[chan], Memc[osfn], SZ_FNAME, nchars)
	    if (nchars > 0) {
		nchars = vfnunmap (vp, Memc[osfn], outline, maxch)
		if (nchars > 0 && nchars < maxch) {
		    if (dirmode[chan] == SKIP_HIDDEN_FILES)
			if (outline[1] == '.' ||
			    vfn_is_hidden_file (outline) == YES) {
			    nchars = 0
			    next
			}
		    outline[nchars+1] = '\n'
		    nchars = nchars + 1
		    outline[nchars+1] = EOS
		}
	    }
	} until (nchars != 0)

	# FIO expects to read 0 chars when EOF is reached.
	if (nchars == EOF)
	    status = 0
	else
	    status = nchars
done_
	call sfree (sp)
end


# FPTDIR -- Put a line to a directory.  This function is illegal on directories.
# In principle FIO will never permit us to be called.

procedure fptdir (chan, line, nchars, status)

int	chan, nchars, status
char	line[ARB]

begin
	status = ERR
end


# FFLDIR -- Flush output to a directory.  This function is illegal on
# directories.  In principle FIO will never permit us to be called.

procedure ffldir (chan, status)

int	chan, status

begin
	status = ERR
end


# FSTDIR -- Get file status for a directory file/device.  This is a legal
# function, used to get the buffer size.

procedure fstdir (chan, param, lvalue)

int	chan			# not used
int	param			# parameter for which status is desired
long	lvalue			# returned value

begin
	switch (param) {
	case FSTT_BLKSIZE:
	    lvalue = 1
	case FSTT_FILSIZE:
	    lvalue = 0
	case FSTT_OPTBUFSIZE:
	    lvalue = SZ_LINE
	case FSTT_MAXBUFSIZE:
	    lvalue = 0
	default:
	    lvalue = ERR
	}
end


# FSKDIR -- Seek on a directory file.  Ignore seek to BOF since ZOPDIR
# opens at BOF automatically (FIO will call us to seek to BOF when the
# file is opened).

procedure fskdir (chan, offset, status)

int	chan, status
long	offset

begin
	switch (offset) {
	case BOFL:
	    status = OK
	default:
	    status = ERR
	}
end


# FNTDIR -- Note position on a directory file.  Seeking is illegal on
# directories so we merely return ERR.

procedure fntdir (chan, offset)

int	chan
long	offset

begin
	offset = ERR
end
