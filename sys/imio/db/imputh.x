# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	<imio.h>
include	"idb.h"

define	LEN_HISTSTR	70	# length of a history string on a FITS card

# IMPUTH -- Add a FITS-like history/comment field to the image header.
# Only keywords HISTORY, COMMENT, or "        " (eight spaces) are allowed!
# (At least for the present - in the future this routine will probably
# append FITS cards to a distinct FITS-table appearing as a table parameter
# in the generalized image header.  Also, since it is not yet decided how
# image history will be handled in the future, there is no guarantee that
# this routine will remain unchanged - it may change or be obsoleted.)

procedure imputh (im, key, text)

pointer	im			#I image descriptor
char	key[ARB]		#I name of the new parameter
char	text[ARB]		#I the history string to be added

pointer	sp, keyname, instr, outstr, ua
int	fd, max_lenuserarea, curlen, buflen, nchars
int	ip, op, in_last_blank, out_last_blank

bool	streq()
int	stropen(), strlen(), idb_filstr()
errchk	syserrs, stropen, fprintf

begin
	call smark (sp)
	call salloc (instr, SZ_LINE, TY_CHAR)
	call salloc (keyname, SZ_FNAME, TY_CHAR)
	call salloc (outstr, LEN_HISTSTR, TY_CHAR)

	# FITS format requires that the keyword name be upper case.
	call strcpy (key, Memc[keyname], SZ_FNAME)
	call strupr (Memc[keyname])

	# Only standard FITS HISTORY keywords are allowed.
	if (!(streq(Memc[keyname],"HISTORY") ||
	    streq(Memc[keyname],"COMMENT") ||
	    streq(Memc[keyname],"        "))) {

	    call eprintf ("IMPUTH: Invalid history keyword `%s' ignored\n")
		call pargstr (key)
	    call sfree (sp)
	    return
	}
	
	# Open the user area string for appending.  'buflen' is the malloc-ed
	# buffer length in struct units; IMU is the struct offset to the user
	# area, i.e., the size of that part of the image descriptor preceding
	# the user area.  If the buffer fills we must allow one extra char for
	# the EOS delimiter; since storage for the image descriptor was
	# allocated in struct units the storage allocator will not have
	# allocated space for the extra EOS char.

	ua = IM_USERAREA(im)
	curlen = strlen (Memc[ua])
	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1

	# If the user area is not empty the last character must be the newline
	# record delimiter, else the new record we add will be invalid.

	if (curlen > 0 && Memc[ua+curlen-1] != '\n')
	    if (curlen >= max_lenuserarea)
		call syserrs (SYS_IDBOVFL, key)
	    else {
		Memc[ua+curlen] = '\n'
		curlen = curlen + 1
		Memc[ua+curlen] = EOS
	    }

	# Open a file descriptor on the userarea buffer.
	fd = stropen (Memc[ua+curlen], max_lenuserarea-curlen, APPEND)

	# Filter the input string to remove any undesirable characters.
	nchars = idb_filstr (text, Memc[instr], SZ_LINE)

	# Append the HISTORY or COMMENT record to the user area.
	iferr {
	    if (nchars <= LEN_HISTSTR ) {
		# This is the easy case: the HISTORY string will fit in
		# one record.

		call fprintf (fd, "%-8s  %s%*t\n")
		    call pargstr (Memc[keyname])
		    call pargstr (Memc[instr])
		    call pargi (IDB_LENSTRINGRECORD + 1)

	    } else {
		# Not the simple case; break up the string into pieces that 
		# will fit into LEN_HISTSTR, preferably on word boundaries.

		for (ip=1;  Memc[instr+ip-1] != EOS;  ) {
		    # If no blanks are found in HISTORY string, make sure
		    # all of it gets output anyway.

		    in_last_blank = ip + LEN_HISTSTR - 1
		    out_last_blank = LEN_HISTSTR

		    # Copy the string to the output buffer, marking the
		    # last blank found.

		    do op = 1, LEN_HISTSTR {
			if (IS_WHITE (Memc[instr+ip-1])) {
			    in_last_blank = ip
			    out_last_blank = op
			} else if (Memc[instr+ip-1] == EOS)
			    break

			Memc[outstr+op-1] = Memc[instr+ip-1]
			ip = ip + 1
		    }

		    # The output string is full; close it off properly
		    # and get ready for the next round (if any).

		    Memc[outstr+op-1] = EOS
		    if (Memc[instr+ip-1] != EOS) {
			# Break at last word boundary if in a word.
			if (!IS_WHITE (Memc[instr+ip-1])) {
			    Memc[outstr+out_last_blank] = EOS
			    ip = in_last_blank + 1
			}

			# Skip leading whitespace on next line.
			while (IS_WHITE(Memc[instr+ip-1]))
			    ip = ip + 1
		    }

		    # Write out the FITS HISTORY card.
		    call fprintf (fd, "%-8s  %s%*t\n")
			call pargstr (Memc[keyname])
			call pargstr (Memc[outstr])
			call pargi (IDB_LENSTRINGRECORD + 1)
		}
	    }

	} then {
	    # Out of space in the user area.  Discard the truncated card
	    # at the end of the buffer by backing up to the last newline and
	    # writing an EOS.

	    call close (fd)
	    for (ip=ua+max_lenuserarea-1;  ip > ua;  ip=ip-1)
		if (Memc[ip] == '\n') {
		    Memc[ip+1] = EOS
		    break
		}
	    call syserrs (SYS_IDBOVFL, key)
	}

	call close (fd)
	call sfree (sp)
end
