# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<config.h>
include	"mtio.h"

# MT_UPDATE_LOCKFILE -- Update the current file position in the lockfile.
# We also record some information describing the type of access just completed.
# We are called from a z-routine so we must access the lockfile using only
# low level OS interface routines to avoid recursion.  We may be called during
# error recovery (as well as from a z-routine), so any errors are fatal.

procedure mt_update_lockfile (mt)

int	mt
pointer	sp, lockfile, tempfile, lbuf, ip, op, extn
int	old_lockfile, new_lockfile, junk, status, itoc()
errchk	salloc, fmapfn
include	"mtio.com"
define	err_ 91

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

	    call mt_lockname (MT_DRIVE(mt), Memc[lockfile], SZ_PATHNAME)

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

	# Open old and new lockfiles.
	call zopntx (Memc[lockfile], READ_ONLY, old_lockfile)
	if (old_lockfile == ERR)
	    goto err_

	# Overwrite any existing tempfile.
	call zfdele (Memc[tempfile], junk)
	call zopntx (Memc[tempfile], NEW_FILE, new_lockfile)
	if (new_lockfile == ERR)
	    goto err_

	# Copy the comments section.  Must be at least one comment line.
	repeat {
	    call zgettx (old_lockfile, Memc[lbuf], SZ_LINE, status)
	    if (status <= 0)
		goto err_
	    Memc[lbuf+status] = EOS
	    if (Memc[lbuf] == '#')
		call mt_putline (new_lockfile, Memc[lbuf])
	} until (Memc[lbuf] != '#')


	# Everything else we write from here on is new stuff.  Discard rest
	# of old lockfile.

	# Save current file and record numbers.
	if (MT_FILE(mt) == -1)
	    MT_RECORD(mt) = -1
	call mt_savekeyword (new_lockfile, "file", MT_FILE(mt))
	    call mt_putline (new_lockfile, "\n")
	call mt_savekeyword (new_lockfile, "record", MT_RECORD(mt))

	if (MT_FILE(mt) == 1 && MT_RECORD(mt) == 1)
	    call mt_putline (new_lockfile, " (rewound)\n")
	else if (MT_ATEOT(mt) == YES)
	    call mt_putline (new_lockfile, " (EOT)\n")
	else if (MT_ATEOF(mt) == YES)
	    call mt_putline (new_lockfile, " (EOF)\n")
	else
	    call mt_putline (new_lockfile, "\n")

	# Add some info on last access: number of records read or written,
	# did we see EOF or EOT on a read, and so on.

	if (MT_FILE(mt) == -1) {
	    call mt_putline (new_lockfile, "tape position is undefined\n")

	} else if (MT_NRECORDS(mt) > 0) {
	    junk = itoc (MT_NRECORDS(mt), Memc[lbuf], SZ_LINE)
	    call mt_putline (new_lockfile, Memc[lbuf])
	    call mt_putline (new_lockfile, " records ")

	    if (MT_ACMODE(mt) == READ_ONLY) {
		call mt_putline (new_lockfile, "read")
		if (MT_ATEOF(mt) != YES)
		    call mt_putline (new_lockfile, " (EOF not reached)")
	    } else
		call mt_putline (new_lockfile, "written, file left at EOT")
	    call mt_putline (new_lockfile, "\n")
	}

	# Close lockfiles, delete old one, rename new lockfile as the lockfile
	# for the device.

	call zclstx (new_lockfile, status)
	call zclstx (old_lockfile, status)

	call zfdele (Memc[lockfile], status)
	if (status == ERR)
	    goto err_

	call zfrnam (Memc[tempfile], Memc[lockfile], status)
	if (status == ERR)
	    goto err_

	iferr (call sfree (sp))
	    goto err_

	return

err_
	# If an error of any sort occurs, it is fatal.
	call fatal (0, "Fatal error while closing magtape file")
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
char	lbuf[SZ_LINE]
int	ip, op, status
data	op /1/

begin
	for (ip=1;  text[ip] != EOS;  ip=ip+1) {
	    lbuf[op] = text[ip]
	    op = min (SZ_LINE, op) + 1
	    if (text[ip] == '\n') {
		call zputtx (fd, lbuf, op-1, status)
		if (status == ERR)
		    call fatal (0, "Fatal error writing device lockfile")
		op = 1
	    }
	}
end
