# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"stdgraph.h"

# STG_RESET -- Reset the state of the stdgraph common, i.e., in response to
# a clear or a cancel.  Initialize all attribute packets to their default
# values and set the current state of the device to undefined, forcing the
# device state to be reset when the next output instruction is executed.

procedure stg_reset()

pointer	pl, pm, fa, tx
include	"stdgraph.com"

begin
	# Set pointers to attribute substructures.
	pl = SG_PLAP(g_sg)
	pm = SG_PMAP(g_sg)
	fa = SG_FAAP(g_sg)
	tx = SG_TXAP(g_sg)

	# Initialize the attribute packets.
	PL_LTYPE(pl)	  = 1
	PL_WIDTH(pl)	  = 1
	PL_COLOR(pl)	  = 1
	PM_COLOR(pm)	  = 1
	FA_STYLE(fa)	  = 1
	FA_COLOR(fa)	  = 1
	TX_UP(tx)	  = 90
	TX_SIZE(tx)	  = 1
	TX_PATH(tx)	  = GT_RIGHT
	TX_HJUSTIFY(tx)	  = GT_LEFT
	TX_VJUSTIFY(tx)	  = GT_BOTTOM
	TX_FONT(tx)	  = GT_ROMAN
	TX_COLOR(tx)	  = 1
	TX_SPACING(tx)	  = 0.0

	# Set the device attributes to undefined, forcing them to be reset
	# when the next output instruction is executed.

	SG_COLOR(g_sg)	  = -1
	SG_TXSIZE(g_sg)	  = -1
	SG_TXFONT(g_sg)	  = -1
	SG_PLTYPE(g_sg)   = -1
	SG_FASTYLE(g_sg)  = -1
	SG_PLWIDTH(g_sg)  = -1
	g_lastx		  = -1
	g_lasty		  = -1
	g_keycol	  = 1
	g_keyline	  = 1
	g_message	  = NO
	g_msglen	  = 0
end
