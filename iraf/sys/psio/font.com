# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FONT.COM -- Character width font tables.  Each array contains the width
# of ASCII chars 32 (space) thru 126 (~) expressed as 10 times the width 
# in pixels as drawn on a 300dpi page.  This allows us to compute fractional
# pixels when placing strings.
#
# The default font chosen is a 10-point Times-Roman in normal, bold and italic.
# Other font sizes chosen will be scaled from these values.  Fixed width fonts
# don't need width tables and are defined in the PSTOOLS.H file.

# Set an alias for the width of a space char so we can change it easily
# in the package include and not forget about it here.
define	SW	SPACE_WIDTH

# Declare the width tables.
int	i
short	roman[96], bold[96], italic[96]	


# Times-Roman 10-point normal.
data	(roman(i), i= 1, 7)  /  SW,  33,  41,  50,  50,  83,  78/
data	(roman(i), i= 8,14)  /  33,  33,  33,  50,  56,  25,  33/
data	(roman(i), i=15,21)  /  25,  28,  50,  50,  50,  50,  50/
data	(roman(i), i=22,28)  /  50,  50,  50,  50,  50,  28,  28/
data	(roman(i), i=29,35)  /  56,  56,  56,  44,  92,  72,  67/
data	(roman(i), i=36,42)  /  67,  72,  61,  56,  72,  72,  33/
data	(roman(i), i=43,49)  /  39,  72,  61,  89,  72,  72,  56/
data	(roman(i), i=50,56)  /  72,  67,  56,  61,  72,  72,  94/
data	(roman(i), i=57,63)  /  72,  72,  61,  33,  28,  33,  47/
data	(roman(i), i=64,70)  /  50,  33,  44,  50,  44,  50,  44/
data	(roman(i), i=71,77)  /  33,  50,  50,  28,  28,  50,  28/
data	(roman(i), i=78,84)  /  78,  50,  50,  50,  50,  33,  39/
data	(roman(i), i=85,91)  /  28,  50,  50,  72,  50,  50,  44/
data	(roman(i), i=92,96)  /  48,  20,  48,  54, 0/

# Times-Roman 10-point bold.
data	(bold(i), i= 1, 7)   /  SW,  33,  56,  50,  50, 100,  83/
data	(bold(i), i= 8,14)   /  33,  33,  33,  50,  57,  25,  33/
data	(bold(i), i=15,21)   /  25,  28,  50,  50,  50,  50,  50/
data	(bold(i), i=22,28)   /  50,  50,  50,  50,  50,  33,  33/
data	(bold(i), i=29,35)   /  57,  57,  57,  50,  93,  72,  67/
data	(bold(i), i=36,42)   /  72,  72,  67,  61,  78,  78,  39/
data	(bold(i), i=43,49)   /  50,  78,  67,  94,  72,  78,  61/
data	(bold(i), i=50,56)   /  78,  72,  56,  67,  72,  72, 100/
data	(bold(i), i=57,63)   /  72,  72,  67,  33,  28,  33,  58/
data	(bold(i), i=64,70)   /  50,  33,  50,  56,  44,  56,  44/
data	(bold(i), i=71,77)   /  33,  50,  56,  28,  33,  56,  28/
data	(bold(i), i=78,84)   /  83,  56,  50,  56,  56,  44,  39/
data	(bold(i), i=85,91)   /  33,  56,  50,  72,  50,  50,  44/
data	(bold(i), i=92,96)   /  39,  22,  39,  52, 0/

# Times-Roman 10-point italic.
data	(italic(i), i= 1, 7) /  SW,  33,  42,  50,  50,  83,  78/
data	(italic(i), i= 8,14) /  33,  33,  33,  50,  68,  25,  33/
data	(italic(i), i=15,21) /  25,  28,  50,  50,  50,  50,  50/
data	(italic(i), i=22,28) /  50,  50,  50,  50,  50,  33,  33/
data	(italic(i), i=29,35) /  68,  68,  68,  50,  92,  61,  61/
data	(italic(i), i=36,42) /  67,  72,  61,  61,  72,  72,  33/
data	(italic(i), i=43,49) /  44,  67,  56,  83,  67,  72,  61/
data	(italic(i), i=50,56) /  72,  61,  50,  56,  72,  61,  83/
data	(italic(i), i=57,63) /  61,  56,  56,  39,  28,  39,  42/
data	(italic(i), i=64,70) /  50,  33,  50,  50,  44,  50,  44/
data	(italic(i), i=71,77) /  28,  50,  50,  28,  28,  44,  28/
data	(italic(i), i=78,84) /  72,  50,  50,  50,  50,  39,  39/
data	(italic(i), i=85,91) /  28,  50,  44,  67,  44,  44,  39/
data	(italic(i), i=92,96) /  40,  27,  40,  54, 0/

