int	card_length
int	cards_per_blk
int	detab
int	ebcdic
int	ibm
char	contn_string[SZ_LINE]

common /wcardcom/ card_length, cards_per_blk, detab, ebcdic, ibm, contn_string
