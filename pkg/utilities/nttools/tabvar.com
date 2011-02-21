# TCALC.COM -- Common block used for comunicating btw tcalc and tabvar

double	nullval		# Substituted for null columns in expressions
pointer	tabptr		# Table descriptor
int	firstrow	# First row to evaluate
int	lastrow		# Last row to evaluate

common	/tcalc/  nullval, tabptr, firstrow, lastrow

