# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_INIT -- Initialize the GIOTR data structures for a graphics stream.  These
# data structures are initialized only once, when the first i/o occurs on the
# stream.  Thereafter our only function is to fault the workstation
# transformation parameters into the cache (the gtr common).

pointer procedure gtr_init (stream)

int	stream			# graphics stream

int	i, len_fb, len_sb
pointer	tr, tx, ap, w
bool	first_time
int	btoi(), envgeti()
data	first_time /true/
errchk	calloc, malloc
include	"gtr.com"

begin
	if (first_time) {
	    call amovki (NULL, trdes, MAX_PSEUDOFILES)
	    tr_stream = NULL
	    first_time = false
	}

	tr = trdes[stream]

	if (tr == NULL) {
	    # This is the first time the stream has been accessed.

	    # Allocate descriptor.
	    call calloc (tr, LEN_TRSTRUCT, TY_STRUCT)

	    # Don't need a frame buffer for STDPLOT, but make a dummy one
	    # anyhow so that the stream looks like the interactive ones.

	    if (stream == STDPLOT) {
		len_fb = 1
		len_sb = 1
	    } else {
		len_fb = DEF_LENFRAMEBUF
		len_sb = DEF_LENSCRATCHBUF
	    }

	    call malloc (TR_FRAMEBUF(tr),   len_fb, TY_SHORT)
	    call malloc (TR_SCRATCHBUF(tr), len_sb, TY_SHORT)

	    trdes[stream] = tr
	    TR_IP(tr) = TR_FRAMEBUF(tr)
	    TR_OP(tr) = TR_FRAMEBUF(tr)
	    TR_OPSB(tr) = TR_SCRATCHBUF(tr)
	    TR_LENFRAMEBUF(tr) = len_fb
	    TR_LENSCRATCHBUF(tr) = len_sb
	    TR_SPOOLDATA(tr) = btoi (stream != STDPLOT)
	    TR_WAITPAGE(tr) = NO
	    TR_PAGE(tr) = YES

	    # Set text drawing attributes for annotating plots.
	    tx = TR_TXAP(tr)
	    TX_UP(tx) = 90
	    TX_SIZE(tx) = 1.0
	    TX_PATH(tx) = GT_RIGHT
	    TX_SPACING(tx) = 0
	    TX_HJUSTIFY(tx) = GT_LEFT
	    TX_VJUSTIFY(tx) = GT_BOTTOM
	    TX_FONT(tx) = GT_ROMAN
	    TX_QUALITY(tx) = GT_NORMAL
	    TX_COLOR(tx) = 1

	    # Set default polyline attributes for axis drawing.
	    ap = TR_PLAP(tr)
	    PL_LTYPE(ap) = GL_SOLID
	    PL_WIDTH(ap) = 1.0
	    PL_COLOR(ap) = 1

	    # The user can override the default maximum frame buffer length
	    # if they wish, permitting spooling of frames of any size.

	    iferr (TR_MAXLENFRAMEBUF(tr) = envgeti ("cmbuflen"))
		TR_MAXLENFRAMEBUF(tr) = DEF_MAXLENFRAMEBUF

	    if (tr_stream != NULL) {
		# Save the workstation transformation parameters for the
		# stream currently in the cache, if any.

		call amovi (startcom, TR_GTRCOM(trdes[tr_stream]), LEN_GTRCOM)
		call amovi (TR_GTRCOM(tr), startcom, LEN_GTRCOM)
	    }

	    # Initialize the transformation parameters for the new stream.
	    tr_stream = stream
	    xscale = 1.0
	    yscale = 1.0
	    mx2 = GKI_MAXNDC
	    my2 = GKI_MAXNDC
	    vx2 = 1.0
	    vy2 = 1.0

	    # Initialize the WCS in case someone tries to read the cursor
	    # before there are any graphics.

	    do i = 1, MAX_WCS {
		w = TR_WCSPTR(tr,i)
		WCS_SX1(w) = 0.0
		WCS_SX2(w) = 1.0
		WCS_SY1(w) = 0.0
		WCS_SY2(w) = 1.0

		WCS_WX1(w) = 0.0
		WCS_WX2(w) = 1.0
		WCS_WY1(w) = 0.0
		WCS_WY2(w) = 1.0
	    }

	} else if (stream != tr_stream) {
	    # The stream has already been initialized.

	    # If the cache is currently validated for some different stream
	    # move the data for that stream out into its descriptor.

	    if (tr_stream != NULL)
		call amovi (startcom, TR_GTRCOM(trdes[tr_stream]), LEN_GTRCOM)

	    # Load the data for the new stream into the cache.
	    call amovi (TR_GTRCOM(tr), startcom, LEN_GTRCOM)
	    tr_stream = stream
	}

	return (tr)
end
