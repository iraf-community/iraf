# Common block for look up tables used by idsmtn format reader.  These tables
# are used to evaluate each byte of both integer and floating point varian
# numbers.  They are initialized in t_ridsmtn.x.

int	neg_lut6[256], neg_lut7[256], neg_lut8[256]
common	/lut/neg_lut6, neg_lut7, neg_lut8
