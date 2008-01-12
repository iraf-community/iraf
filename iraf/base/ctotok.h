# CTOTOK.H -- Tokens recognized by CTOTOK.

define	TOK_IDENTIFIER		1	# [A-Za-z][A-Za-z0-9_.$]*
define	TOK_NUMBER		2	# [0-9][-+0-9.:xXa-fA-F]*
define	TOK_OPERATOR		3	# all other printable sequences
define	TOK_PUNCTUATION		4	# [:,;] or any control character
define	TOK_STRING		5	# "..."
define	TOK_CHARCON		6	# '\n', etc.
define	TOK_EOS			7	# end of string
define	TOK_NEWLINE		8	# end of line
define	TOK_UNKNOWN		9	# control characters
