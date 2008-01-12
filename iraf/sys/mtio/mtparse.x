# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<ctype.h>
include	"mtio.h"

# MTPARSE -- Decode a virtual magtape file specification, returning the device
# name, file and record to which the drive is to be positioned, and any special
# device attributes (these will override the device defaults).  The file and
# record fields are returned as ERR if missing.  Only the drive name field is
# required.
# 
# Magtape device syntax:
#
#	[node!] mtX [ '[' file[.record] [:attr-list] ']' ]
#
# for example,
#
#	mtexb1[4:nb:se@:ts=1200:so=/dev/ttya8]
#
# The "mt" prefix is required for the object to be considered a magtape
# device reference.  The device name returned is "mtX" as shown above; there
# must be an entry for device mtX in the tapecap file in DEV.
#
# The file and record numbers are optional.  Files and records are numbered
# starting with 1.  A sequence such as "mtX[eot]" will cause the tape to be
# positioned to end of tape.  "mtX[0]" causes the tape to be opened at the
# current position, i.e., without being moved.
#
# The optional attr-list field consists of a sequence of colon-delimited
# tapecap fields.  These will override any values given in the tapecap
# entry for the device.  The syntax for attr-list is the same as in tapecap.

procedure mtparse (mtname, device, sz_device, file, record, attrl, sz_attrl)

char	mtname[ARB]		#I device specification
char	device[ARB]		#O device name as in tapecap
int	sz_device		#I max chars in device name
int	file			#O file number or -1
int	record			#O record number or -1
char	attrl[ARB]		#O attribute list
int	sz_attrl		#I max char in attribute list

char	eotstr[3]
int	ip, op, nchars, ival
int	ctoi(), strncmp(), ki_extnode()
bool	streq()
define	bad_ 91

begin
	# Extract the node name, if any, from the mtname.
	ip = ki_extnode (mtname, device, sz_device, nchars) + 1
	op = nchars + 1

	# Verify that this is a magtape device specification.
	if (strncmp (mtname[ip], "mt", 2) != 0)
	    goto bad_

	# Extract the device name field.
	while (mtname[ip] != EOS && mtname[ip] != '[') {
	    device[op] = mtname[ip]
	    op = min (sz_device, op + 1)
	    ip = ip + 1
	}
	device[op] = EOS

	file = ERR
	record = ERR
	attrl[1] = EOS

	# Process the [...] part of the device specification.
	if (mtname[ip] == '[') {
	    ip = ip + 1

	    # Get the file number.
	    if (ctoi (mtname, ip, ival) > 0) {
		file = ival
		if (file < 0)
		    goto bad_
	    } else if (IS_ALPHA(mtname[ip])) {
		call strcpy (mtname[ip], eotstr, 3)
		call strlwr (eotstr)
		if (streq (eotstr, "eot")) {
		    file = EOT
		    ip = ip + 3
		} else
		    goto bad_
	    }

	    # Get the record number.
	    if (mtname[ip] == '.' || mtname[ip] == ',') {
		ip = ip + 1
		if (mtname[ip] == ']')
		    record = ERR
		else if (ctoi (mtname, ip, ival) > 0) {
		    record = ival
		    if (record < 0)
			goto bad_
		}
	    }

	    # Get the device attribute list.
	    op = 1
	    if (mtname[ip] == ':') {
		attrl[op] = mtname[ip]
		op = max(1, min(sz_attrl, op + 1))
		ip = ip + 1

		while (mtname[ip] != EOS && mtname[ip] != ']') {
		    attrl[op] = mtname[ip]
		    op = max(1, min(sz_attrl, op + 1))
		    ip = ip + 1
		}
	    }
	    attrl[op] = EOS

	    # Check for the ']' terminator.
	    if (mtname[ip] != ']')
		goto bad_
	}

	return
bad_
	call syserrs (SYS_MTFILSPEC, mtname)
end
