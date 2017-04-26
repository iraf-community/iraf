# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include <ctype.h>
include	<imio.h>
include	"mwcs.h"
include	"imwcs.h"

# IW_RFITS -- Read a FITS image header into an IMWCS (FITS oriented) world
# coordinate system descriptor.  For reasons of efficiency (especially due
# to the possibly of large sampled WCS arrays) this is done with a single
# pass through the header to get all the WCS data, with interpretation of
# the data being a separate independent step.  A pointer to an IMWCS descriptor
# is returned as the function value.  When no longer needed, this should be
# freed with IW_CLOSE.  The dimensionality of the WCS is determined first
# from the image dimensionality (which may be zero) and then overridden
# if there is a WCSDIM card.  If the final dimensionality is zero then
# the maximum axis of the WCS cards sets the dimensionality.


pointer procedure iw_rfits (mw, im, mode)

pointer	mw			#I pointer to MWCS descriptor
pointer	im			#I pointer to image header
int	mode			#I RF_REFERENCE or RF_COPY

double	dval
bool	omit, copy
pointer	iw, idb, rp, cp, fp
int	ndim, recno, ualen, type, axis, index, ip, temp, i

pointer	idb_open()
int	idb_nextcard(), iw_cardtype(), ctod(), ctoi()
errchk	calloc, realloc, syserrs

begin
	ndim = max (IM_NDIM(im), IM_NPHYSDIM(im))
	copy = (mode == RF_COPY)

	# Allocate and initialize the FITS-WCS descriptor.
	call calloc (iw, LEN_IMWCS, TY_STRUCT)
	call calloc (IW_CBUF(iw), LEN_CDES * DEF_MAXCARDS, TY_STRUCT)

	# Allocate string buffer if we must keep a local copy of the data.
	if (copy) {
	    call calloc (IW_SBUF(iw), SZ_SBUF, TY_CHAR)
	    IW_SBUFLEN(iw) = SZ_SBUF
	    IW_SBUFOP(iw) = 0
	}

	IW_MAXCARDS(iw) = DEF_MAXCARDS
	IW_NDIM(iw) = ndim
	IW_IM(iw) = im

	# Scan the image header, examining successive cards to see if they
	# are WCS specification cards, making an entry for each such card
	# in the IMWCS descriptor.  The values of simple scalar valued cards
	# are interpreted immediately and used to modify the default WCS
	# data values established above.  For the array valued parameters we
	# merely record the particulars for each card, leaving reconstruction
	# of the array until all the cards have been located.

	idb = idb_open (im, ualen)
	recno = 0
	while (idb_nextcard (idb, rp) != EOF) {
	    recno = recno + 1
	    if (iw_cardtype (Memc[rp], type, axis, index) <= 0)
		next


	    # Has this card already been seen?
	    omit = false
	    do i = 1, IW_NCARDS(iw) {
		cp = IW_CARD(iw,i)
		if (C_TYPE(cp) != type)
		    next
		if (C_AXIS(cp) != axis)
		    next
		if (C_INDEX(cp) != index)
		    next
		omit = true
		break
	    }

	    # Ignore duplicate cards.
	    if (omit)
		next

	    # Get another card descriptor.
	    IW_NCARDS(iw) = IW_NCARDS(iw) + 1
	    if (IW_NCARDS(iw) > IW_MAXCARDS(iw)) {
		IW_MAXCARDS(iw) = IW_MAXCARDS(iw) + INC_MAXCARDS
		call realloc (IW_CBUF(iw),
		    IW_MAXCARDS(iw) * LEN_CDES, TY_STRUCT)
		cp = IW_CARD(iw,IW_NCARDS(iw))
		call aclri (Memi[cp],
		    (IW_MAXCARDS(iw) - IW_NCARDS(iw) + 1) * LEN_CDES)
	    }
	    cp = IW_CARD(iw,IW_NCARDS(iw))

	    C_TYPE(cp) = type
	    C_AXIS(cp) = axis
	    C_INDEX(cp) = index
	    C_CARDNO(cp) = recno

	    ndim = max (ndim, axis)

	    # The FITS data must be copied into local storage if the header
	    # will be edited, since otherwise the cards may move, invalidating
	    # the pointer.  Always save whole cards; don't bother with an EOS
	    # or newline between cards.

	    if (copy) {
		if (IW_SBUFOP(iw) + SZ_CARD > IW_SBUFLEN(iw))
		    call syserrs (SYS_MWFITSOVFL, IM_NAME(im))
		C_RP(cp) = IW_SBUF(iw) + IW_SBUFOP(iw)
		call strcpy (Memc[rp], Memc[C_RP(cp)], SZ_CARD)
		IW_SBUFOP(iw) = IW_SBUFOP(iw) + SZ_CARD
	    } else
		C_RP(cp) = rp

	    # Decode the card value. 
	    ip = IDB_STARTVALUE
	    switch (type) {
	    case TY_CTYPE:
		fp = C_RP(cp) + ip
		while (IS_WHITE(Memc[fp]) || Memc[fp] == '\'')
		    fp = fp + 1
		IW_CTYPE(iw,axis) =  fp
	    case TY_CDELT:
		if (ctod (Memc[rp], ip, IW_CDELT(iw,axis)) <= 0)
		    IW_CDELT(iw,axis) = 0.0
	    case TY_CROTA:
		if (ctod (Memc[rp], ip, dval) > 0)
		    IW_CROTA(iw) = dval
	    case TY_CRPIX:
		if (ctod (Memc[rp], ip, IW_CRPIX(iw,axis)) <= 0)
		    IW_CRPIX(iw,axis) = 0.0
	    case TY_CRVAL:
		if (ctod (Memc[rp], ip, IW_CRVAL(iw,axis)) <= 0)
		    IW_CRVAL(iw,axis) = 0.0
	    case TY_CD:
		if (ctod (Memc[rp], ip, IW_CD(iw,axis,index)) <= 0)
		    IW_CD(iw,axis,index) = 0.0
	    case TY_LTV:
		if (ctod (Memc[rp], ip, IW_LTV(iw,axis)) <= 0)
		    IW_LTV(iw,axis) = 0.0
	    case TY_LTM:
		if (ctod (Memc[rp], ip, IW_LTM(iw,axis,index)) <= 0)
		    IW_LTM(iw,axis,index) = 0.0
	    case TY_WSVLEN:
		if (ctoi (Memc[rp], ip, IW_WSVLEN(iw,axis)) <= 0)
		    IW_WSVLEN(iw,axis) = 0
	    case TY_WCSDIM:
		if (ctoi (Memc[rp], ip, temp) > 0)
		    IW_NDIM(iw) = temp
	    }
	}

	# Set dimension to the maximum axis seen.
	if (IW_NDIM(iw) == 0)
	    IW_NDIM(iw) = ndim

	call idb_close (idb)
	return (iw)
end
