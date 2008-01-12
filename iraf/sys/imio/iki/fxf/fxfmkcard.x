# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
# FXF_MK_CARD --  Fetch a single line from a string parameter, padding it to
# a maximum of maxcols characters and trimmimg the delim character.

procedure fxf_make_card (instr, ip, card, col_out, maxcols, delim)

char	instr[ARB]	#I input string
int	ip		#U input string pointer, updated at each call
char	card[ARB]	#O FITS card image
int	col_out		#I pointer to column in card
int	maxcols		#I maximum columns in card
int	delim		#I 1 character string delimiter

int	op

begin
	op = col_out

	# Copy string
	while (op <= maxcols && instr[ip] != EOS && instr[ip] != delim) {
	    card[op] = instr[ip]
	    ip = ip + 1
	    op = op + 1
	}

	# Fill remainder of card with blanks
	while (op <= maxcols ) {
	    card[op] = ' '
	    op = op + 1
	}

	if (instr[ip] == delim)
	    ip = ip + 1
end
