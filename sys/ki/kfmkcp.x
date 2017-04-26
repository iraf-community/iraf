# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"


# KFMKCP -- Make a null length copy of a file.  The new file inherits the
# attributes of the existing file.  This works provided both files are on
# the same node; since the kernel routine is atomic and must access both
# files, and the attributes are OS dependent, there is no way to inherit
# the attributes if the files reside on different nodes.

procedure kfmkcp (old_osfn, new_osfn, status)

char	old_osfn[ARB]		# packed os filename of existing file
char	new_osfn[ARB]		# packed os filename of new file
int	status			# answer; ok or err

pointer	sp, fname
int	server1, server2, chan, junk, old, new
int	ki_connect(), ki_sendrcv(), strlen()
include	"kii.com"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	server2 = ki_connect (new_osfn)
	call strcpy (p_sbuf[p_arg[1]], Memc[fname], SZ_FNAME)
	server1 = ki_connect (old_osfn)
	old = p_arg[1]

	if (server1 == NULL && server2 == NULL) {
	    # Both files reside on the local node.

	    call strpak (p_sbuf[old], p_sbuf[old], SZ_SBUF)
            call strpak (Memc[fname], Memc[fname], SZ_FNAME)
	    call zfmkcp (p_sbuf[old], Memc[fname], status)

	} else if (server1 == server2) {
	    # Both files reside on the same remote node.

	    new = old + strlen(p_sbuf[old])+1 + 1
	    if (new + strlen(Memc[fname])+1 > SZ_SBUF)
		status = ERR
	    else {
		call strcpy (Memc[fname], p_sbuf[new], SZ_SBUF-new+1)
	    
		p_arg[2]  = new
		p_sbuflen = SZ_SBUF

		if (ki_sendrcv (server1, KI_ZFMKCP, 0) == ERR)
		    status = ERR
		else
		    status = p_arg[1]
	    }

	} else if (server1 != NULL && server2 != NULL) {
	    # Both files are remote.  Cannot transfer all attributes;
	    # the best we can do is create either a text or binary file.

	    call kfacss (old_osfn, 0, TEXT_FILE, status)
            call strpak (Memc[fname], Memc[fname], SZ_FNAME)

	    if (status == YES) {
		# Create a text file.
		call kopntx (new_osfn, NEW_FILE, chan)
		if (chan != ERR) {
		    call kclstx (chan, junk)
		    status = chan
		} else
		    status = ERR
	    } else {
		# Create a binary file.
		call kopnbf (new_osfn, NEW_FILE, chan)
		if (chan != ERR) {
		    call kclsbf (chan, junk)
		    status = chan
		} else
		    status = ERR
	    }

	} else if (server1 != NULL) {
	    # The existing file is remote.  Cannot transfer all attributes;
	    # the best we can do is create either a text or binary file.
	    # Call ZFACSS to determine the file type of the existing file
	    # and create a null length text or binary file.

	    call kfacss (old_osfn, 0, TEXT_FILE, status)
            call strpak (Memc[fname], Memc[fname], SZ_FNAME)

	    if (status == YES) {
		# Create a text file.
		call zopntx (Memc[fname], NEW_FILE, chan)
		if (chan != ERR) {
		    call zclstx (chan, junk)
		    status = chan
		} else
		    status = ERR
	    } else {
		# Create a binary file.
		call zopnbf (Memc[fname], NEW_FILE, chan)
		if (chan != ERR) {
		    call zclsbf (chan, junk)
		    status = chan
		} else
		    status = ERR
	    }

	} else {
	    # The new file is remote.

	    call strpak (p_sbuf[old], p_sbuf[old], SZ_SBUF)
	    call zfacss (p_sbuf[old], 0, TEXT_FILE, status)

	    if (status == YES) {
		# Create a text file.
		call kopntx (new_osfn, NEW_FILE, chan)
		if (chan != ERR) {
		    call kclstx (chan, junk)
		    status = chan
		} else
		    status = ERR
	    } else {
		# Create a binary file.
		call kopnbf (new_osfn, NEW_FILE, chan)
		if (chan != ERR) {
		    call kclsbf (chan, junk)
		    status = chan
		} else
		    status = ERR
	    }
	}

	call sfree (sp)
end
