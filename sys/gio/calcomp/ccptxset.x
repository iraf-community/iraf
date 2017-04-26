# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	"ccp.h"

# CCP_TXSET -- Set the text drawing attributes.

procedure ccp_txset (gki)

short	gki[ARB]		# attribute structure

pointer	tx
include	"ccp.com"

begin
	tx = CCP_TXAP(g_cc)

	TX_UP(tx)	= gki[GKI_TXSET_UP] 
	TX_PATH(tx)	= gki[GKI_TXSET_P ] 
	TX_HJUSTIFY(tx)	= gki[GKI_TXSET_HJ] 
	TX_VJUSTIFY(tx)	= gki[GKI_TXSET_VJ] 
	TX_FONT(tx)	= gki[GKI_TXSET_F ]
	TX_QUALITY(tx)	= gki[GKI_TXSET_Q ] 
	TX_COLOR(tx)	= gki[GKI_TXSET_CI] 

	TX_SPACING(tx)	= GKI_UNPACKREAL (gki[GKI_TXSET_SP])
	TX_SIZE(tx)     = gki[GKI_TXSET_SZ]
end
