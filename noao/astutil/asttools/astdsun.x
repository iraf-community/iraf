include	<math.h>

# AST_DSUN -- Distance to Sun in AU
# Taken from Astronomical Almanac 1984, page C24.

double procedure ast_dsun (epoch)

double	epoch			# Epoch desired

double	n, g, r
double	ast_julday()

begin
	n = ast_julday (epoch) - 2451545d0
	g = DEGTORAD (357.528d0 + 0.9856003d0 * n)
	r = 1.00014d0 - 0.01671d0 * cos (g) - 0.00014d0 * cos (2 * g)

	return (r)
end
