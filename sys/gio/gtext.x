# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GTEXT -- Draw text.  All textual output via GIO is via this primitive.  Unlike
# polyline, polymarker, fill area, and cell array output, textual output is not
# subjected to clipping by GIO.  Clipping may be performed at the kernel level
# if a workstation viewport is defined.  Our task here is principally to parse
# the format string and set up the text attributes, then insert the text drawing
# instruction into the GKI instruction stream.  The real work of text generation
# is very device dependent and is therefore left to the kernel.

procedure gtext (gp, x, y, text, format)

pointer	gp			# graphics descriptor
real	x, y			# position at which text is to be drawn
char	text[ARB]		# text to be drawn
char	format[ARB]		# text drawing parameters

int	ip, i
real	mx, my
pointer	sp, ap, tx
bool	text_attributes_modified

begin
	call smark (sp)
	call salloc (ap, LEN_TX, TY_STRUCT)

	# Set up pointers to text attribute packets and initialize the
	# new packet to the default values.  Two text attribute packets
	# are maintained in GP: TXAP, the default packet, and TXAPCUR,
	# the packet last sent to the device.  In what follows, AP is
	# the new packet and TX is the packet last sent to the device.
	# We start by initializing the new packet at AP to the default
	# text drawing parameters.

	call amovi (Memi[GP_TXAP(gp)], Memi[ap], LEN_TX)
	tx = GP_TXAPCUR(gp)

	# Parse the format string and set the text attributes.  The code is
	# more general than need be, i.e., the entire attribute name string
	# is extracted but only the first character is used.  Whitespace is
	# permitted and ignored.

	ip = 1
	call gtxset (ap, format, ip)

	# If the old text attribute packet was never fixed always fix the
	# new packet, otherwise determine whether or not any text attributes
	# were actually modified and only fix the new packet if it is
	# different.

	text_attributes_modified = false
	for (i=2;  i <= LEN_TX;  i=i+1)
	    if (Memi[ap+i-1] != Memi[tx+i-1]) {
		text_attributes_modified = true
		break
	    }

	# Flush any buffered polyline output, and transform the text coordinates
	# to GKI device coordinates.

	call gpl_flush()
	call gpl_wcstogki (gp, x, y, mx, my)

	# Update text attributes if necessary.
	if (text_attributes_modified || TX_STATE(tx) != FIXED) {
	    call amovi (Memi[ap], Memi[tx], LEN_TX)
	    call gki_txset (GP_FD(gp), tx)
	    TX_STATE(tx) = FIXED
	}

	# Output text drawing instruction.
	call gki_text (GP_FD(gp), nint(mx), nint(my), text)

	call sfree (sp)
end
