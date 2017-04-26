# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	<imio.h>
include	"idbc.h"

define	LEN_HISTSTR	71	# length of a history string on a FITS card
define  CLEN            81

# IMPUTXTF -- Insert a text file in the user area with HISTORY card.
# The file cannot have control characters in it; only the FITS standard
# character set is supported.  The text is broken in records long enough
# to fit words; i.e. it tries not to split words. The file can have 
# imbedded tabs and they will be expanded.

procedure imputextf (im, file, pkey, baf)

pointer	im			#I image descriptor
char	file[ARB]		#I the text file to be inserted and appended
char    pkey[ARB]               #I Pivot keyword to insert 'key'
int     baf                     #I Insert BEFORE or AFTER

pointer	ua, rp, piv, ip, op
int	max_lenuserarea, curlen, buflen, jump, nlines
int	old_curlen, k, nshift
char    blk

int	strlen(), idb_findrecord()
errchk	syserrs

begin
	# FITS format requires that the keyword name be upper case.

	ua = IM_USERAREA(im)
	curlen = strlen (Memc[ua])
	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1

	# Determine the number of lines before inserting into the UA
	call imrartxt (ua, file, nlines, NO)

        old_curlen=curlen
        curlen = curlen + nlines*CLEN
        if (curlen+81 >= max_lenuserarea) {
            IM_HDRLEN(im)  = LEN_IMHDR  +
                    (curlen + 10*36*CLEN + SZ_STRUCT-1) / SZ_STRUCT
            IM_LENHDRMEM(im) = IM_HDRLEN(im)  + (SZ_UAPAD / SZ_STRUCT)
            call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
            buflen = LEN_IMDES + IM_LENHDRMEM(im)
            max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1
	    ua = IM_USERAREA(im)
        }

	blk=' '
	# Find pivot keyword 
        if (idb_findrecord (im, pkey, rp) == 0) {
            # Keyw not found. Append the new keywords.
            piv = ua + old_curlen
        } else {
            # Shift cards after or before pivot.
            if (baf == AFTER)
                piv = rp + CLEN
            else
                piv = rp 
        
            jump=nlines*CLEN

	    # Shift cards down from the pivot point. 
	    nshift = (ua+old_curlen - piv)/CLEN 
	    ip = ua + old_curlen 
            do k = 1, nshift {
	        ip = ip - CLEN
		op = jump + ip
		call amovc (Memc[ip], Memc[op], CLEN)
	    }
        }

	# Append the HISTORY records to the user area.
	call imrartxt (piv, file, nlines, YES)

end


# IMRARTXT -- Internal routines to count the number of lines transfered to the
# UA as HISTORY records.

procedure imrartxt (piv, fname, nlines, insert)

pointer piv	#I UA address to start inserting kw
char	fname[ARB]
int	nlines
int	insert

char	line[IDB_RECLEN+1], blk, lf
pointer	sp, ln, buf, urp
int 	ip, op, fd, in_last_blank, out_last_blank, blen, len, w, k
int	save_ip
int	open(), getline(), strlen()

begin
	call smark(sp)
	call salloc (ln, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	fd = open(fname, READ_ONLY, TEXT_FILE)
        nlines= 0
	blk=' '
	lf='\12'
	call strcpy ("HISTORY  ", Memc[buf], 9)
	Memc[buf+IDB_LENSTRINGRECORD]='\n'
	Memc[buf+IDB_LENSTRINGRECORD+1]=EOS
        urp = piv
	while(getline(fd, Memc[ln]) != EOF) {
		for (ip=1;  Memc[ln+ip-1] != EOS;  ) {
		    # If no blanks are found in HISTORY string, make sure
		    # all of it gets output anyway.

		    in_last_blank = ip + LEN_HISTSTR - 1
		    out_last_blank = LEN_HISTSTR

		    # Copy the string to the output buffer, marking the
		    # last blank found.

		    for (op=1; op <= LEN_HISTSTR; op=op+1) {
			if (Memc[ln+ip-1] == lf) {
			    ip=ip+1
			}    
			if (IS_WHITE (Memc[ln+ip-1])) {
			    # Detab input text.
			    if (Memc[ln+ip-1] == '\t') {
				if(ip-save_ip == 1)
				   w=8
				else
				   w=9-op+(op/9)*8
				for(k=0;k<w;k=k+1) {
				    line[op+k] = blk
				}
				save_ip=ip
				op = op + w - 1
				ip = ip + 1
				in_last_blank = ip
				out_last_blank = op
				next
			    }
			    in_last_blank = ip
			    out_last_blank = op
			} else if (Memc[ln+ip-1] == EOS) 
			    break
			line[op] = Memc[ln+ip-1]
			ip = ip + 1
		    }
		    # The output string is full; close it off properly
		    # and get ready for the next round (if any).
		    line[op] = EOS
		    if (Memc[ln+ip-1] != EOS) {
			# Break at last word boundary if in a word.
			if (!IS_WHITE (Memc[ln+ip-1])) {
			    line[out_last_blank+1] = EOS
			    ip = in_last_blank + 1
			}

			# Skip leading whitespace on next line.
			while (IS_WHITE(Memc[ln+ip-1]))
			    ip = ip + 1
		    }
		    nlines = nlines + 1

		    if (insert == YES) {
			# Write out the FITS HISTORY card.
			len = strlen(line)
			blen = IDB_LENSTRINGRECORD - len - 9 
			call amovc (line, Memc[buf+9], len)
			call amovkc (blk, Memc[buf+9+len], blen)

			call amovc (Memc[buf], Memc[urp], IDB_RECLEN+1)
			urp = urp + IDB_RECLEN + 1
		    }
		}
	}

        call close(fd)
        call sfree(sp)
end
