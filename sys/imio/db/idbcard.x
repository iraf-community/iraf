# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"idb.h"

.help IDBCARD
.nf -------------------------------------------------------------------------
Card i/o package, for reading through the FITS area of the image header.

	         idb = idb_open (im, ualen)
       recno|EOF = idb_nextcard (idb, rp)
		      idb_close (idb)

This is a very simple package, used only to hide the details of how to
access successive image header cards.  The main routine returns a char
pointer to successive cards until the end of the header is reached.
This is convenient for efficient read access to the header; direct i/o
to the image header may be accomplished by using STROPEN to open the
header buffer on a file descriptor.

This entire interface assumes that the header is stored in FITS format,
which is an implementation detail of the current IMIO interface.  Hence,
this interface is internal to IMIO.
.endhelp --------------------------------------------------------------------

define	LEN_IDB		6
define	IDB_IM		Memi[$1]		# image descriptor
define	IDB_UA		Memi[$1+1]		# pointer to user area
define	IDB_UALEN	Memi[$1+2]		# length of user area
define	IDB_RECPTR	Memi[$1+3]		# current record pointer
define	IDB_RECNO	Memi[$1+4]		# current record number
define	IDB_BLOCKED	Memi[$1+5]		# cards blank filled?


# IDB_OPEN -- Open the FITS area for for card i/o.

pointer procedure idb_open (im, ualen)

pointer	im			#I image descriptor
int	ualen			#O size of storage area

int	n
pointer	idb, ip
errchk	malloc

begin
	call malloc (idb, LEN_IDB, TY_STRUCT)

	IDB_IM(idb) = im
	IDB_UA(idb) = IM_USERAREA(im)
	IDB_UALEN(idb) = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
	IDB_RECPTR(idb) = IM_USERAREA(im)
	IDB_RECNO(idb) = 1

	if (IM_UABLOCKED(im) < 0) {
	    # At image open time this flag is set by IMMAP to -1 to indicate
	    # that the user area record type is not known.  An IKI kernel may
	    # subsequently set the flag to yes/no, else we determine the
	    # record type by inspection the first time we are called.  If the
	    # user area is empty the record type is set to blocked; IDB always
	    # writes blocked records.

	    IM_UABLOCKED(im) = YES
	    for (ip=IM_USERAREA(im);  Memc[ip] != EOS;  ip=ip+1) {
		for (n=0;  Memc[ip] != EOS;  n=n+1) {
		    if (Memc[ip] == '\n')
			break
		    ip = ip + 1
		}
		if (n != IDB_RECLEN) {
		    IM_UABLOCKED(im) = NO
		    break
		}
	    }
	}

	IDB_BLOCKED(idb) = IM_UABLOCKED(im)
	ualen = IDB_UALEN(idb)
	return (idb)
end


# IDB_NEXTCARD -- Return a pointer to the next card in the FITS header.
# EOF is returned at the end of the header.

int procedure idb_nextcard (idb, recptr)

pointer	idb			#I pointer to IDB descriptor
pointer	recptr			#O pointer to card

int	recno
pointer	ip, i

begin
	# Reference current card.
	recno = IDB_RECNO(idb)
	recptr = IDB_RECPTR(idb)

	# Advance to the next card.
	ip = recptr
	if (IDB_BLOCKED(idb) == NO) {
	    if (Memc[ip] != EOS)		# skip blank lines
		ip = ip + 1
	    do i = ip, ip+IDB_RECLEN
		if (Memc[i] == EOS) {
		    ip = i
		    break
		} else if (Memc[i] == '\n') {
		    ip = i + 1
		    break
		}
	} else
	    ip = ip + IDB_RECLEN + 1

	IDB_RECNO(idb) = recno + 1
	IDB_RECPTR(idb) = ip

	if (Memc[recptr] == EOS || recptr >= IDB_UA(idb) + IDB_UALEN(idb))
	    return (EOF)
	else
	    return (recno)
end


# IDB_CLOSE -- Free the IDB descriptor.

procedure idb_close (idb)

pointer	idb			#I pointer to IDB descriptor

begin
	call mfree (idb, TY_STRUCT)
end
