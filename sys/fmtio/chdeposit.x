# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CHDEPOSIT -- Deposit a character in a string at the offset OP.  Bump OP,
# taking care not to overflow the string.

procedure chdeposit (ch, str, maxch, op)

char	ch			# character to be deposited
char	str[ARB]		# output string
int	maxch			# maxch chars in output string
int	op			# pointer into output string

begin
	str[op] = ch
	if (op < maxch)
	    op = op + 1
end
