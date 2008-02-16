# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

define	LEN_DEFIBUF	2048
define	ONEWORD		SZ_SHORT
define	TWOWORDS	(2*SZ_SHORT)

# Header fields of a GKI instruction.
define	I_BOI		Mems[$1+GKI_HDR_BOI-1]
define	I_OPCODE	Mems[$1+GKI_HDR_OPCODE-1]
define	I_LENGTH	Mems[$1+GKI_HDR_LENGTH-1]
define	I_DATA		Mems[$1+GKI_DATAFIELDS-1]

# GKI_FETCH_NEXT_INSTRUCTION -- Fetch the next GKI metacode instruction from the
# input stream.  A pointer to a short integer buffer containing the instruction
# is returned as an output argument.  EOF is returned as the function value
# when EOF is seen on the input stream.  The instruction buffer may be
# deallocated by our caller at any time, if desired.  A new buffer will be
# created automatically when next we are called.

int procedure gki_fetch_next_instruction (fd, instruction)

int	fd			# input file containing metacode
pointer	instruction		# pointer to instruction (output)

int	len_ibuf, nchars
pointer	ibuf
int	read()
errchk	read, malloc, realloc
data	ibuf /NULL/

begin
	# Allocate a default sized instruction buffer.  We can reallocate
	# a larger buffer later if necessary.

	if (ibuf == NULL) {
	    call malloc (ibuf, LEN_DEFIBUF, TY_SHORT)
	    len_ibuf = LEN_DEFIBUF
	}

	# Advance to the next instruction.  Nulls and botched portions of
	# instructions are ignored.  Read the instruction header to determine
	# the length of the instruction, and then read the rest of instruction
	# into buffer.  If the entire instruction cannot be read we have a
	# botched instruction and must try again.

	repeat {
	    repeat {
		if (read (fd, I_BOI(ibuf), ONEWORD) == EOF)
		    return (EOF)
	    } until (I_BOI(ibuf) == BOI)

	    if (read (fd, I_OPCODE(ibuf), TWOWORDS) == EOF)
		return (EOF)
	    
	    # Make instruction buffer large enough to hold instruction.
	    # Compute length of remainder of instruction in chars.

	    if (I_LENGTH(ibuf) > len_ibuf) {
		len_ibuf = I_LENGTH(ibuf)
		call realloc (ibuf, len_ibuf, TY_SHORT)
	    }

	    nchars = (I_LENGTH(ibuf) - LEN_GKIHDR) * SZ_SHORT
	    if (nchars == 0)
		break

	} until (read (fd, I_DATA(ibuf), nchars) == nchars)

	instruction = ibuf

	# Check for a soft end of file, otherwise return the length of the
	# instruction as the function value.

	if (I_OPCODE(ibuf) == GKI_EOF)
	    return (EOF)
	else
	    return (I_LENGTH(ibuf))
end
