# Common block for look up tables used by idsmtn format reader.  This table
# contains powers of 2 used to reassemble floating point numbers.  It is
# initialized in t_ridsmtn.x.

double	tbl[255]
common	/po2/tbl
