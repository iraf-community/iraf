# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<ctype.h>
include	"mtio.h"

define	SZ_EOTSTR	3

# MT_PARSE -- Decode a virtual magtape file specification, returning the
# fields "drive", "density", and "file_number".  The latter two fields are
# set to -1 if absent.  The drive name field is required.
# 
# Filespec syntax:
#
#    [node!]"mt"(drive)(800|1600|6250)?("["(fileno)?[,(recno)?]|"eot""]")?
#
# We do not actually know what densities are legal for each tape drive; all we
# do is look for the integer constant, convert it, and return the value.  We
# do check that the drive unit number is legal.  Extra characters anywhere are
# an error.

procedure mt_parse (filespec, drive, maxch, density, filenum, recordnum)

char	filespec[ARB]		# filespec to be decoded
char	drive[maxch]		# drive name
int	maxch			# max chars in drive name
int	density			# desired density or -1 if don't care
int	filenum			# desired filenumber or -1 if current pos
int	recordnum		# desired record number or 1 if not given

int	ip, op, nchars
char	eotstr[SZ_EOTSTR], ch
bool	streq()
int	ctoi(), strncmp(), ki_extnode(), delim()
define	badspec_ 91

begin
	# Extract the node name, if any, from the filespec.  IP is left
	# pointing to the first char following the node prefix.

	ip = ki_extnode (filespec, drive, maxch, nchars) + 1
	op = nchars + 1

	# The remainder of the filespec must be a conventional MTIO magtape
	# name.

	if (strncmp (filespec[ip], "mt", 2) != 0)
	    goto badspec_

	# Skip the prefix "mt".
	ip = ip + 2

	# Extract the drive name field.  If the first character following
	# the "mt" is nonalphanumeric that character is taken as the drive
	# name field delimiter and is not included in the drive name string,
	# thus a delimiter may be used to mark drive name fields containing
	# arbitrary data.  If no delimiter character is given the drive
	# name field is assumed to be an alphabetic sequence.  Hence, old
	# syntax magtape file names such as "mta1600[1]" are still permitted.

	if (IS_ALPHA (filespec[ip])) {
	    for (;  IS_ALPHA (filespec[ip]);  ip=ip+1) {
		drive[op] = filespec[ip]
		op = op + 1
	    }

	    # Skip an unmatched delimiter, as in "mta.1600", rather than
	    # "mt.*.1600", which puts us in the else clause, below.

	    if (filespec[ip] == '.')
		ip = ip + 1

	} else {
	    delim = filespec[ip]
	    for (ip=ip+1;  filespec[ip] != EOS;  ip=ip+1)
		if (filespec[ip] == delim) {
		    ip = ip + 1
		    break
		} else if (op > maxch || filespec[ip] == '[') {
		    break
		} else {
		    drive[op] = filespec[ip]
		    op = op + 1
		}
	}

	if (op == 1)
	    goto badspec_
	else
	    drive[op] = EOS

	# Get the density, if any.  Default to zero.  We do not know what
	# densities are legal (that is machine dependent) so any positve
	# number is ok.

	if (ctoi (filespec, ip, density) > 0) {
	    if (density < 0)				# negative density?
		goto badspec_
	} else {					# not a number
	    if (filespec[ip] != '[' && filespec[ip] != EOS)
		goto badspec_				# mtaNNjunk
	    else
		density = 0
	}

	# Get the file number and record numbers, if any.  "[eot]" is a
	# special case, referencing the end of tape.

	filenum = -1
	recordnum = 1

	if (filespec[ip] == '[') {
	    ip = ip + 1
	    ch = filespec[ip]

	    if (ctoi (filespec, ip, filenum) > 0) {
		if (filenum < 0)
		    goto badspec_			# [-nn]
	    } else if (IS_ALPHA(ch)) {
		call strcpy (filespec[ip], eotstr, SZ_EOTSTR)
		call strlwr (eotstr)
		if (streq (eotstr, "eot")) {
		    filenum = EOT
		    ip = ip + SZ_EOTSTR
		} else
		    goto badspec_			# string not 'eot'
	    }

	    if (filespec[ip] == ',') {			# record num field
		ip = ip + 1
		if (filespec[ip] == ']')
		    recordnum = 0
		else if (ctoi (filespec, ip, recordnum) > 0) {
		    if (recordnum < 0)
			goto badspec_			# [nn,junk]
		}
	    }

	    if (filespec[ip] == ']')
		ip = ip + 1
	    else
		goto badspec_				# no trailing ']'
	}

	if (filespec[ip] != EOS)
	    goto badspec_				# extra junk at end

	return

badspec_
	call syserrs (SYS_MTFILSPEC, filespec)
end
