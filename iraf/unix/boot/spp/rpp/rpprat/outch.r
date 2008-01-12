include  defs

# outch - put one character into output buffer

subroutine outch (c)

character c, splbuf(SZ_SPOOLBUF+1)
integer i, ip, op, index
include COMMON_BLOCKS
external index
string	break_chars " ),.+-*/("

	# Process a continuation card.  Try to break the card at a whitespace
	# division, operator, or punctuation mark.

	if (outp >= 72) {
	    if (index (break_chars, c) > 0)		# find break point
		ip = outp
	    else {
		for (ip=outp;  ip >= 1;  ip=ip-1) {
		    if (index (break_chars, outbuf(ip)) > 0)
			break
		}
	    }

	    if (ip != outp & (outp-ip) < SZ_SPOOLBUF) {
		op = 1
		for (i=ip+1;  i <= outp;  i=i+1) {	# save chars
		    splbuf(op) = outbuf(i)
		    op = op + 1
		}
		splbuf(op) = EOS
		outp = ip
	    } else
		splbuf(1) = EOS

	    call outdon
	    
	    for (op=1;  op < col;  op=op+1)
		outbuf(op) = BLANK
	    outbuf(6) = STAR
	    outp = col
	    for (ip=1;  splbuf(ip) != EOS;  ip=ip+1) {
		outp = outp + 1
		outbuf(outp) = splbuf(ip)
	    }
	}

	outp = outp + 1				# output character
	outbuf(outp) = c
end
