# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GKI_EXECUTE -- Execute a metacode instruction.  The instruction is decoded
# and a graphics kernel driver subroutine is called to execute the instruction.
# If the device driver does not include a procedure for the instruction the
# instruction is discarded.  Integer and real parameters are unpacked from
# their short integer metacode representation.  Character data is passed by
# reference, i.e., as a SHORT integer array (not EOS delimited char!!), along
# with the character count.  Attribute packets are passed to the set attribute
# procedure by reference as a short integer array.

procedure gki_execute (gki, dd)

short	gki[ARB]		# graphics kernel instruction
int	dd[ARB]			# device driver

int	kp			# kernel procedure
int	m, n, cn, fn, dummy, flags
int	x, y, x1, y1, x2, y2

begin
	switch (gki[GKI_HDR_OPCODE]) {

	case GKI_OPENWS:
	    kp = dd[GKI_OPENWS]
	    if (kp != NULL) {
		m = gki[GKI_OPENWS_M]
		n = gki[GKI_OPENWS_N]
		call zcall3 (kp, gki[GKI_OPENWS_D], n, m)
	    }
	case GKI_CLOSEWS:
	    kp = dd[GKI_CLOSEWS]
	    if (kp != NULL) {
		n = gki[GKI_CLOSEWS_N]
		call zcall2 (kp, gki[GKI_CLOSEWS_D], n)
	    }
	case GKI_REACTIVATEWS:
	    kp = dd[GKI_REACTIVATEWS]
	    if (kp != NULL) {
		flags = gki[GKI_REACTIVATEWS_F]
		call zcall1 (kp, flags)
	    }
	case GKI_DEACTIVATEWS:
	    kp = dd[GKI_DEACTIVATEWS]
	    if (kp != NULL) {
		flags = gki[GKI_DEACTIVATEWS_F]
		call zcall1 (kp, flags)
	    }
	case GKI_MFTITLE:
	    kp = dd[GKI_MFTITLE]
	    if (kp != NULL) {
		n = gki[GKI_MFTITLE_N]
		call zcall2 (kp, gki[GKI_MFTITLE_T], n)
	    }
	case GKI_CLEAR:
	    kp = dd[GKI_CLEAR]
	    if (kp != NULL) {
		call zcall1 (kp, dummy)
	    }
	case GKI_CANCEL:
	    kp = dd[GKI_CANCEL]
	    if (kp != NULL) {
		call zcall1 (kp, dummy)
	    }
	case GKI_FLUSH:
	    kp = dd[GKI_FLUSH]
	    if (kp != NULL) {
		call zcall1 (kp, dummy)
	    }
	case GKI_POLYLINE:
	    kp = dd[GKI_POLYLINE]
	    if (kp != 0) {
		n = gki[GKI_POLYLINE_N]
		call zcall2 (kp, gki[GKI_POLYLINE_P], n)
	    }
	case GKI_POLYMARKER:
	    kp = dd[GKI_POLYMARKER]
	    if (kp != 0) {
		n = gki[GKI_POLYMARKER_N]
		call zcall2 (kp, gki[GKI_POLYMARKER_P], n)
	    }
	case GKI_TEXT:
	    kp = dd[GKI_TEXT]
	    if (kp != NULL) {
		x = gki[GKI_TEXT_P]
		y = gki[GKI_TEXT_P+1]
		n = gki[GKI_TEXT_N]
		call zcall4 (kp, x, y, gki[GKI_TEXT_T], n)
	    }
	case GKI_FILLAREA:
	    kp = dd[GKI_FILLAREA]
	    if (kp != 0) {
		n = gki[GKI_FILLAREA_N]
		call zcall2 (kp, gki[GKI_FILLAREA_P], n)
	    }
	case GKI_PUTCELLARRAY:
	    kp = dd[GKI_PUTCELLARRAY]
	    if (kp != NULL) {
		x1 = gki[GKI_PUTCELLARRAY_LL]
		y1 = gki[GKI_PUTCELLARRAY_LL+1]
		x2 = gki[GKI_PUTCELLARRAY_UR]
		y2 = gki[GKI_PUTCELLARRAY_UR+1]
		m  = gki[GKI_PUTCELLARRAY_NC]
		n  = gki[GKI_PUTCELLARRAY_NL]
		call zcall7 (kp, gki[GKI_PUTCELLARRAY_P], m, n, x1,y1, x2,y2)
	    }
	case GKI_SETCURSOR:
	    kp = dd[GKI_SETCURSOR]
	    if (kp != NULL) {
		cn = gki[GKI_SETCURSOR_CN]
		x  = gki[GKI_SETCURSOR_POS]
		y  = gki[GKI_SETCURSOR_POS+1]
		call zcall3 (kp, x, y, cn)
	    }
	case GKI_PLSET:
	    kp = dd[GKI_PLSET]
	    if (kp != NULL) {
		call zcall1 (kp, gki)
	    }
	case GKI_PMSET:
	    kp = dd[GKI_PMSET]
	    if (kp != NULL) {
		call zcall1 (kp, gki)
	    }
	case GKI_TXSET:
	    kp = dd[GKI_TXSET]
	    if (kp != NULL) {
		call zcall1 (kp, gki)
	    }
	case GKI_FASET:
	    kp = dd[GKI_FASET]
	    if (kp != NULL) {
		call zcall1 (kp, gki)
	    }
	case GKI_GETCURSOR:
	    kp = dd[GKI_GETCURSOR]
	    if (kp != NULL) {
		cn = gki[GKI_GETCURSOR_CN]
		call zcall1 (kp, cn)
	    }
	case GKI_GETCELLARRAY:
	    kp = dd[GKI_GETCELLARRAY]
	    if (kp != NULL) {
		x1 = gki[GKI_GETCELLARRAY_LL]
		y1 = gki[GKI_GETCELLARRAY_LL+1]
		x2 = gki[GKI_GETCELLARRAY_UR]
		y2 = gki[GKI_GETCELLARRAY_UR+1]
		m  = gki[GKI_GETCELLARRAY_NC]
		n  = gki[GKI_GETCELLARRAY_NL]
		call zcall6 (kp, m, n, x1,y1, x2,y2)
	    }
	case GKI_ESCAPE:
	    kp = dd[GKI_ESCAPE]
	    if (kp != NULL) {
		fn = gki[GKI_ESCAPE_FN]
		n  = gki[GKI_ESCAPE_N]
		call zcall3 (kp, fn, gki[GKI_ESCAPE_DC], n)
	    }
	case GKI_SETWCS:
	    kp = dd[GKI_SETWCS]
	    if (kp != NULL) {
		n = gki[GKI_SETWCS_N]
		call zcall2 (kp, gki[GKI_SETWCS_WCS], n)
	    }
	case GKI_GETWCS:
	    kp = dd[GKI_SETWCS]
	    if (kp != NULL) {
		n = gki[GKI_SETWCS_N]
		call zcall2 (kp, gki[GKI_SETWCS_WCS], n)
	    }
	default:
	    kp = dd[GKI_UNKNOWN]
	    if (kp != NULL)
		call zcall1 (kp, gki)
	}
end
