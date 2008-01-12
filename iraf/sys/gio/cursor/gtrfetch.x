# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_FETCH_NEXT_INSTRUCTION -- Return a pointer to the next GKI metacode
# instruction in the input buffer.  Only complete instructions resident in
# a contiguous section of memory are returned.  EOF is returned when the
# end of the current buffer is reached, or when the last instruction in the
# frame buffer is not yet complete.  EOF does not signify the end of the
# metacode stream.

int procedure gtr_fetch_next_instruction (tr, gki)

pointer	tr			# pointer to giotr descriptor
pointer	gki			# pointer to next instruction (output)

int	nleft, length
pointer	ip, itop

begin
	ip   = TR_IP(tr)
	itop = TR_OP(tr)

	# Search for the beginning of the next instruction.
	while (Mems[ip] != BOI && ip < itop)
	    ip = ip + 1

	nleft = itop - ip
	if (nleft < 3) {
	    # The length field of the next instruction is not yet present.
	    TR_IP(tr) = ip
	    return (EOF)
	} else {
	    length = Mems[ip+GKI_HDR_LENGTH-1]
	    if (length > nleft) {
		# Entire instruction is not yet present in buffer.
		TR_IP(tr) = ip
		return (EOF)
	    } else {
		# Entire instruction is present in buffer.
		TR_IP(tr) = ip + length
		gki = ip
		return (length)
	    }
	}
end
