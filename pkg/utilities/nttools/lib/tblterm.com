int	jumpbuf[LEN_JUMPBUF]
common	/jmpcom/	jumpbuf

bool	constant
int	nterm, irow, iterm
pointer	table
common	/opcom/		constant, nterm, irow, iterm, table
