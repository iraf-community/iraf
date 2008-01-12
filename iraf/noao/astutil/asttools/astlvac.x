# AST_LVAC -- Convert air wavelength to vacuum wavelength.
#
#     Convert LAMBDA(air) to LAMBDA(vacuum) using the formulae
#
#     (n-1)*1e8 = 8342.13 + 2406030/(130-s**2) + 15997/(38.9-s**2)
#
#                 where  s = 1/lambda(air)   (for lambda in MICRONS)
#
#     lambda(vac) = n * lambda(air)
#
#     NOTE: lambda is given in ANGSTROMS for this program.
#

procedure ast_lvac (lair, lvac, npts)

double	lair[npts]		#I Air wavelength (Angstroms)
double	lvac[npts]		#O Vacuum wavelength (Angstroms)
int	npts			#I Number of points

int	i
double	s2, n

begin
	do i = 1, npts {
	    s2 = 1D8 * (1. / lair[i]) ** 2
	    n = 1 + ((8342.13 + 2406030 / (130-s2) + 15997 / (38.9-s2)) / 1D8)
	    lvac[i] = n * lair[i]
	}
end
