# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	"stdgraph.h"

# STG_TXSET -- Set the text drawing attributes.

procedure stg_txset (gki)

short	gki[ARB]		# attribute structure
pointer	tx
include	"stdgraph.com"

begin
	tx = SG_TXAP(g_sg)

	TX_UP(tx)	= gki[GKI_TXSET_UP] 
	TX_PATH(tx)	= gki[GKI_TXSET_P ] 
	TX_HJUSTIFY(tx)	= gki[GKI_TXSET_HJ] 
	TX_VJUSTIFY(tx)	= gki[GKI_TXSET_VJ] 
	TX_FONT(tx)	= gki[GKI_TXSET_F ]
	TX_QUALITY(tx)	= gki[GKI_TXSET_Q ] 
	TX_COLOR(tx)	= gki[GKI_TXSET_CI] 

	# Unpack the packed-real character spacing parameter.
	TX_SPACING(tx)	= GKI_UNPACKREAL (gki[GKI_TXSET_SP])

	# The character size is left as a packed real as we must defer the
	# decision to use a discreet hardware character size or to draw
	# characters in software.

	TX_SIZE(tx)	= gki[GKI_TXSET_SZ]
end
