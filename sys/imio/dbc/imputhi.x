# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<fset.h>
include	<imhdr.h>
include	<imio.h>
include	"idbc.h"

# IMPHIS -- Insert a user field in the image header after the specified 
# keyword.  It is an error if the named field already exists.

procedure imphis (im, key, text, pkey, baf)

pointer	im			#I image descriptor
char	key[ARB]		#I name of the new parameter
char    text[ARB]               #I the history string to be added
char	pkey[ARB]		#I 'key' will be inserted bef/after pkey
int     baf			# I Insert BEFORE or AFTER

pointer	rp, sp, keyname, ua, ip, instr
int	max_lenuserarea, curlen, buflen, nchars, piv
int	idb_findrecord()
bool    streq()
int	strlen(), idb_filstr(), nowhite()
char	card[IDB_RECLEN+1]
errchk	syserrs, sprintf, pargstr, pargi

begin
	call smark (sp)
	call salloc (keyname, SZ_FNAME, TY_CHAR)
	call salloc (instr, SZ_LINE, TY_CHAR)

	nchars = idb_filstr (key, Memc[keyname], IDB_SZFITSKEY)
	nchars = nowhite (Memc[keyname], Memc[keyname], IDB_SZFITSKEY)
	call strupr (Memc[keyname])

	# Only standard FITS HISTORY keywords are allowed.
        if (!(streq(Memc[keyname],"HISTORY") ||
            streq(Memc[keyname],"COMMENT") ||
            streq(Memc[keyname],"ADD_BLAN"))) {
            call sfree (sp)
            return
        }

        if (streq(Memc[keyname],"ADD_BLAN")) {
           call strcpy ("        ", Memc[keyname],  SZ_FNAME)
	}
	   
	# Open the user area string for appending.  'buflen' is the malloc-ed
	# buffer length in struct units; IMU is the struct offset to the user
	# area, i.e., the size of that part of the image descriptor preceding
	# the user area. 

	ua = IM_USERAREA(im)
	curlen = strlen (Memc[ua])
	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1

	if (curlen+81 >= max_lenuserarea) {
	    IM_HDRLEN(im)  = LEN_IMHDR  + 
	            (curlen + 10*36*81 + SZ_STRUCT-1) / SZ_STRUCT
	    IM_LENHDRMEM(im) = IM_HDRLEN(im)  + (SZ_UAPAD / SZ_STRUCT)
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	    buflen = LEN_IMDES + IM_LENHDRMEM(im)
	    max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1
	}

	# If the user area is not empty the last character must be the newline
	# record delimiter, else the new record we add will be invalid.

	if (curlen > 0 && Memc[ua+curlen-1] != '\n')
	    if (curlen >= max_lenuserarea) {
		call syserrs (SYS_IDBOVFL, key)
	    } else {
		Memc[ua+curlen] = '\n'
		curlen = curlen + 1
		Memc[ua+curlen] = EOS
	    }

        # Find keyw_after
	if (idb_findrecord (im, pkey, rp) == 0) {
            # Keyw not found. Append the new keyword.
            rp = ua+curlen
            baf = BEFORE
        } else {
            # Shift cards after pivot or before pivot
	    if (baf == AFTER)
	        piv = rp
            else
	        piv = rp - IDB_RECLEN - 1
	    for (ip= ua+curlen-IDB_RECLEN-1; ip>=piv; ip=ip-IDB_RECLEN-1) {
	        call amovc (Memc[ip], Memc[ip+IDB_RECLEN+1], IDB_RECLEN)
            }
        }
        Memc[ua+curlen+IDB_RECLEN]='\n'
	Memc[ua+curlen+IDB_RECLEN+1]=EOS

        # Filter the input string to remove any undesirable characters.
        nchars = idb_filstr (text, Memc[instr], SZ_LINE)

	# Form a card with keyword name and placeholder for value.
	call sprintf (card, IDB_RECLEN+10, "%-8s %-71s\n")
	    call pargstr (Memc[keyname])
	    call pargstr (Memc[instr])

        # Replace keyword at the position rp+81.
	if (baf == AFTER)
            call amovc (card, Memc[rp+IDB_RECLEN+1], IDB_RECLEN)
        else
            call amovc (card, Memc[rp], IDB_RECLEN)

	call sfree (sp)
end
