#-h-  outdon			  257  local   12/01/80  15:54:31
# outdon - finish off an output line
    include  defs

    subroutine outdon

    include COMMON_BLOCKS

    integer allblk
    integer itoc, ip, op, i
    character obuf(80)
    string s_line "#line "

    # If dbgout is enabled output the "#line" statement.
    if (dbgout == YES) {
	if ((body == YES | dbglev != level) & linect (level) > 0) {
	    op = 1
	    for (ip=1;  s_line(ip) != EOS;  ip=ip+1) {
		obuf(op) = s_line(ip)
		op = op + 1
	    }

	    op = op + itoc (linect, obuf(op), 80-op+1)
	    obuf(op) = BLANK
	    op = op + 1
	    obuf(op) = DQUOTE
	    op = op + 1

	    for (i=fnamp-1;  i >= 1;  i=i-1)
		if (fnames(i-1) == EOS | i == 1) {	 # print file name
		    for (ip=i;  fnames(ip) != EOS;  ip=ip+1) {
			obuf(op) = fnames(ip)
			op = op + 1
		    }
		    break
		}

	    obuf(op) = DQUOTE
	    op = op + 1
	    obuf(op) = NEWLINE
	    op = op + 1
	    obuf(op) = EOS
	    op = op + 1

	    call putlin (obuf, STDOUT)
	    dbglev = level
	}
    }

    # Output the program statement.
    outbuf (outp + 1) = NEWLINE
    outbuf (outp + 2) = EOS
    if (allblk (outbuf) == NO)
	call putlin (outbuf, STDOUT)
    outp = 0

    return
    end
