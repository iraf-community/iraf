# COMMAND.COM -- Global variables used by commands

int	direction		# Search direction
char	search_exp[SZ_LINE]	# Search expression

common /search/  direction, search_exp
