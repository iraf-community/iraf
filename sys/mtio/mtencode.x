# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MTENCODE -- Construct a full magtape device specification.  This routine is
# the opposite of MTPARSE.  If the file and record numbers are to be omitted
# from the output mtname they should be passed as ERR.

procedure mtencode (outstr, maxch, device, fileno, recno, attrl)

char	outstr[ARB]			#O magtape device specification
int	maxch				#I max chars out
char	device[ARB]			#I device name (incl node)
int	fileno, recno			#I file and record numbers, or ERR
char	attrl[ARB]			#I tapecap attributes

int	op
int	gstrcpy()
int	itoc()

begin
	if (fileno != ERR || recno != ERR || attrl[1] != EOS) {
	    op = gstrcpy (device, outstr, maxch) + 1
	    outstr[op] = '[';  op = op + 1 
	    if (fileno != ERR) {
		if (fileno == EOT)
		    op = op + gstrcpy ("EOT", outstr[op], maxch-op+1)
		else
		    op = op + itoc (fileno, outstr[op], maxch-op+1)
	    }
	    if (recno != ERR) {
		outstr[op] = '.';  op = op + 1
		op = op + itoc (recno, outstr[op], maxch-op+1)
	    }
	    if (attrl[1] != EOS) {
		if (attrl[1] != ':') {
		    outstr[op] = ':'
		    op = op + 1
		}
		op = op + gstrcpy (attrl, outstr[op], maxch-op+1)
	    }
	    outstr[op] = ']';  op = op + 1
	    outstr[op] = EOS
	} else
	    call strcpy (device, outstr, maxch)
end
