# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	<gset.h>
include	"../lib/ids.h"

# IDS_RESET -- Reset the state of the transform common, i.e., in response to
# a clear or a cancel.  Initialize all attribute packets to their default
# values and set the current state of the device to undefined, forcing the
# device state to be reset when the next output instruction is executed.
# Clear the image, graphics, and luts only if reset is "hard" enough.

procedure ids_reset(hardness)

short	hardness

pointer	pl, pm, fa, tx

include	"../lib/ids.com"

begin
	# Set pointers to attribute substructures.
	pl = IDS_PLAP(i_kt)
	pm = IDS_PMAP(i_kt)
	fa = IDS_FAAP(i_kt)
	tx = IDS_TXAP(i_kt)

	# Initialize the attribute packets.
	PL_LTYPE(pl)	= 1
	PL_WIDTH(pl)	= GKI_PACKREAL(1.)
	PL_COLOR(pl)	= 1
	PM_LTYPE(pm)	= 1
	PM_WIDTH(pm)	= GKI_PACKREAL(1.)
	PM_COLOR(pm)	= 1
	FA_STYLE(fa)	= 1
	FA_COLOR(fa)	= 1
	TX_UP(tx)	= 90
	TX_SIZE(tx)	= GKI_PACKREAL(1.)
	TX_PATH(tx)	= GT_RIGHT
	TX_HJUSTIFY(tx)	= GT_LEFT
	TX_VJUSTIFY(tx)	= GT_BOTTOM
	TX_FONT(tx)	= GT_ROMAN
	TX_COLOR(tx)	= 1
	TX_SPACING(tx)	= 0.0

	# Set the device attributes to undefined, forcing them to be reset
	# when the next output instruction is executed.

	IDS_TYPE(i_kt)		= -1
	IDS_WIDTH(i_kt)		= -1
	IDS_COLOR(i_kt)		= -1
	IDS_TXSIZE(i_kt)	= -1
	IDS_TXFONT(i_kt)	= -1

	call zreset(hardness)
end
