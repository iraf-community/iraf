# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	<gset.h>
include	"ccp.h"

# CCP_RESET -- Reset the state of the transform common, i.e., in response to
# a clear or a cancel.  Initialize all attribute packets to their default
# values and set the current state of the device to undefined, forcing the
# device state to be reset when the next output instruction is executed.

procedure ccp_reset()

pointer	pl, pm, fa, tx
include	"ccp.com"

begin
	# Set pointers to attribute substructures.
	pl = CCP_PLAP(g_cc)
	pm = CCP_PMAP(g_cc)
	fa = CCP_FAAP(g_cc)
	tx = CCP_TXAP(g_cc)

	# Initialize the attribute packets.
	PL_LTYPE(pl)	= GL_SOLID
	PL_WIDTH(pl)	= GKI_PACKREAL(PL_SINGLE)
	PL_COLOR(pl)	= 1
	PM_LTYPE(pm)	= GL_SOLID
	PM_WIDTH(pm)	= GKI_PACKREAL(PL_SINGLE)
	PM_COLOR(pm)	= 1
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

	CCP_LTYPE(g_cc)		= -1
	CCP_WIDTH(g_cc)		= -1
	CCP_COLOR(g_cc)		= -1
	CCP_TXSIZE(g_cc)	= -1
	CCP_TXFONT(g_cc)	= -1
end
