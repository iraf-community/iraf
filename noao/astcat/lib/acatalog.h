# The builtin astrometry catalog definitions

# Define the maximum length of an array column definition.

define	AT_MAX_NRANGES		10

# Define the currently supported file types.

#define	AT_FTYPES	"|text|btext|"
#define	AT_TEXT		1
#define	AT_BTEXT	2

# Define the standard fields.

define	AT_NSTDCOLS	28

define	AT_CATNAMES     "|id|ra|dec|era|edec|pmra|pmdec|epmra|epmdec|px|rv|\
epx|erv|catsystem|equinox|epoch|mag|color|emag|ecolor|xp|yp|xc|yc|exc|eyc|\
imag|eimag|"

# Define the default data types of the standard fields

define	AT_CATTYPES "|c|d|d|d|d|d|d|d|d|d|d|d|d|c|c|c|r|r|r|r|d|d|d|d|d|d|r|r|"

# Define the default units of the standard fields

define	AT_CATUNITS	"|INDEF|hours|degrees|asecs|asecs|masecs/yr|masecs/yr|\
masecs/yr|masecs/yr|msecs|km/sec|msecs|km/sec|INDEF|INDEF|INDEF|mags|mags|mags|\
mags|pixels|pixels|pixels|pixels|pixels|pixels|mags|mags|"

# Define the default formats of the standard fields

define	AT_CATFORMATS	"|%20s|%11.2h|%11.1h|%6.3f|%6.3f|%7.3f|%7.3f|\
%7.3f|%7.3f|%6.3f|%6.3f|%6.3f|%6.3f|%15s|%15s|%15s|%8.3f|%8.3f|%8.3f|\
%8.3f|%9.3f|%9.3f|%9.3f|%9.3f|%9.3f|%9.3f|%8.3f|%8.3f|"

# Define some useful defaults.

define	DEF_CATSYSTEM	"J2000"
