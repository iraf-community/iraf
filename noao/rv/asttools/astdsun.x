include	<math.h>

# AST_DSUN -- Distance to Sun in AU
# Taken from Astronomical Almanac 1984, page C24.

double procedure ast_dsun (epoch)

double	epoch			# Epoch desired

double	n, g, r
double	ast_julday()

begin
	n = ast_julday (epoch) - 2451545
	g = DEGTORAD (357.528 + 0.9856003 * n)
	r = 1.00014 - 0.01671 * cos (g) - 0.00014 * cos (2 * g)

	return (r)
end
