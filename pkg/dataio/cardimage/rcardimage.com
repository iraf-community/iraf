int	card_length
int	max_line_length
int	trim
int	entab
int	ebcdic
int	ibm
char	contn_string[SZ_LINE]

common /rcardcom/ card_length, max_line_length, trim, entab, ebcdic, ibm,
		  contn_string
