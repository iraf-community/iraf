# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<config.h>
include	"mtio.h"

# MT_UPDATE_LOCKFILE -- Update the current file position in the lockfile.
# This information is returned by the host specific driver at close time.
# We are called from a z-routine so we must access the lockfile using only
# low level OS interface routines to avoid recursion.  We may be called during
# error recovery (as well as from a z-routine), so any errors are fatal.

procedure mt_update_lockfile (mt)

int	mt			#I device slot

extern	mt_sync()
pointer	sp, lockfile, tempfile, lbuf, ip, op, extn
int	old_lockfile, new_lockfile, junk, status, nlines
errchk	fmapfn
include	"mtio.com"
define	oline_ 91
define	err_ 92

begin
	call smark (sp)
	call salloc (lockfile, SZ_PATHNAME, TY_CHAR)
	call salloc (tempfile, SZ_PATHNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Catch any errors in the following section and convert them
	# into fatal errors.

	iferr {
	    # Try to avoid generating any non-host legal filenames in the
	    # following code, to avoid any need to access the VFN mapping
	    # file.  Generate temp file in the same directory on the same
	    # node as the lockfile so that we can easily rename the tempfile
	    # to be the new lockfile.

	    #call mt_lockname (MT_LKNAME(mt), Memc[lockfile], SZ_PATHNAME)
	    call strcpy (MT_LKNAME(mt), Memc[lockfile], SZ_PATHNAME)

	    # Make tempfile name.
	    extn = NULL
	    op   = tempfile
	    for (ip=lockfile;  Memc[ip] != EOS;  ip=ip+1) {
		if (Memc[ip] == '.')
		    extn = op
		Memc[op] = Memc[ip]
		op = op + 1
	    }

	    if (extn == NULL)
		extn = op
	    call strcpy (".tlk", Memc[extn], 4)

	    # Map the filenames.
	    call fmapfn (Memc[tempfile], Memc[tempfile], SZ_PATHNAME)
	    call fmapfn (Memc[lockfile], Memc[lockfile], SZ_PATHNAME)

	} then
	    goto err_

	# Overwrite any existing tempfile.
	call zfdele (Memc[tempfile], junk)
	call zopntx (Memc[tempfile], NEW_FILE, new_lockfile)
	if (new_lockfile == ERR)
	    goto err_

	# Open old lockfile, if any, and copy the comments section.
	call zopntx (Memc[lockfile], READ_ONLY, old_lockfile)
	if (old_lockfile == ERR) {
oline_	    call strcpy ("# Magtape unit ", Memc[lbuf], SZ_LINE)
	    call strcat (MT_DEVICE(mt), Memc[lbuf], SZ_LINE)
	    call strcat (" status\n", Memc[lbuf], SZ_LINE)
	    call mt_putline (new_lockfile, Memc[lbuf])
	} else {
	    nlines = 0
	    repeat {
		call zgettx (old_lockfile, Memc[lbuf], SZ_LINE, status)
		if (status <= 0) {
		    if (nlines == 0) {
			call zclstx (old_lockfile, status)
			goto oline_
		    } else
			break
		} else
		    nlines = nlines + 1
		Memc[lbuf+status] = EOS
		if (Memc[lbuf] == '#')
		    call mt_putline (new_lockfile, Memc[lbuf])
	    } until (Memc[lbuf] != '#')
	    call zclstx (old_lockfile, status)
	}

	# Everything else we write from here on is new stuff.  Discard rest
	# of old lockfile.

	# Save current file and record numbers.
	if (MT_FILNO(mt) == -1)
	    MT_RECNO(mt) = -1
	call mt_savekeyword (new_lockfile, "file", MT_FILNO(mt))
	    if (MT_NFILES(mt) > 0 && (MT_FILNO(mt) == MT_NFILES(mt) + 1))
		call mt_putline (new_lockfile, " (EOT)")
	    call mt_putline (new_lockfile, "\n")
	call mt_savekeyword (new_lockfile, "record", MT_RECNO(mt))
	    call mt_putline (new_lockfile, "\n")
	call mt_savekeyword (new_lockfile, "nfiles", MT_NFILES(mt))
	    call mt_putline (new_lockfile, "\n")
	call mt_savekeyword (new_lockfile, "tapeused", MT_TAPEUSED(mt))
	    call mt_putline (new_lockfile, "\n")
	call mt_savekeyword (new_lockfile, "pflags", MT_PFLAGS(mt))
	    call mt_putline (new_lockfile, "\n")

	# Install the new lockfile.
	call zflstx (new_lockfile, status)
	if (status == ERR)
	    goto err_
	call zclstx (new_lockfile, status)
	if (status == ERR)
	    goto err_

	call zfdele (Memc[lockfile], status)
	call zfrnam (Memc[tempfile], Memc[lockfile], status)
	if (status == ERR)
	    goto err_

	call sfree (sp)
	return

err_
	# If an error of any sort occurs, it is fatal.
	call onerror_remove (mt_sync)
	call zfdele (Memc[tempfile], status)
	call zfdele (Memc[lockfile], status)
	call fatal (0, "Fatal error writing magtape device lockfile")
end


# MT_SAVEKEYWORD -- Write a "keyword = value" status line into the lockfile.

procedure mt_savekeyword (fd, keyword, value)

int	fd			# output file
char	keyword[ARB]		# name of keyword
int	value			# value of keyword
char	numbuf[MAX_DIGITS]
int	junk, itoc()

begin
	junk = itoc (value, numbuf, MAX_DIGITS)

	call mt_putline (fd, keyword)
	call mt_putline (fd, " = ")
	call mt_putline (fd, numbuf)
end


# MT_PUTLINE -- Put a text string to the lockfile.  Do not write line
# to lockfile until a newline is seen.

procedure mt_putline (fd, text)

int	fd
char	text[ARB]

extern	mt_sync()
char	lbuf[SZ_LINE]
int	ip, op, status
data	op /1/

begin
	for (ip=1;  text[ip] != EOS;  ip=ip+1) {
	    lbuf[op] = text[ip]
	    op = min (SZ_LINE, op) + 1
	    if (text[ip] == '\n') {
		call zputtx (fd, lbuf, op-1, status)
		if (status == ERR) {
		    call onerror_remove (mt_sync)
		    call fatal (0,
			"Fatal error writing magtape device lockfile")
		}
		op = 1
	    }
	}
end
