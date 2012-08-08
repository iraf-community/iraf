# AIRVAC -- Convert dispersion coordinates to air or vacuum values.
# The index of refraction formulas used are from Allen's Astrophysical
# Quantities (1973).
#
# This task takes in a file of wavelengths in Angstroms and converts between
# air and vacuum to an output file of wavelengths in Angstroms.  It will
# pass any comments and data after the first wavelength column to the
# output.
#
# To install and use copy the script to your IRAF home directory with the
# name airvac.cl.  Then interactively (for use during the current IRAF session)
# or in your loginuser.cl (for use during future IRAF sessions) enter
# "task airvac=home$airvac.cl".

procedure airvac (input, output)

file	input		    {prompt="Input file of wavelengths in Angstroms"}
file	output		    {prompt="Output file of wavelengths in Angstroms"}
string	type = "air2vac"    {prompt="Conversion (air2vac|vac2air)",
			 	enum = "air2vac|vac2air"}
real	t = 15.		    {prompt="Temperature (C)"}
real	p = 760.	    {prompt="Pressure (mmHg)"}
real	f = 4.		    {prompt="Water vapour pressure (mmHg)"}

struct	*inlist

begin
	file	in, out
	struct	label
	int	n
	real	a, b, w

	in = input
	out = output

	printf ("# airvac (%s, %s, type=%s, t=%.6g, p=%.6g, f=%.6g)\n",
	    in, out, type, t, p, f, > out)

	inlist = input
	while (fscan (inlist, line) != EOF) {
	    n = fscan (line, w, label)
	    if (n == 0) {
		print (line, >> out)
		next
	    }

	    b = (10000. / w) ** 2
	    a = 64.328 + 29498.1 / (146 - b) + 255.4 / (41 - b)
	    a = a * p * (1 + (1.049 - 0.0157 * t) * 1e-6 * p) /
		(720.883 * (1 + 0.003661 * t))
	    a = a - (0.0624 - 0.000680 * b) / (1 + 0.003661 * t) * f
	    a = 1 + a / 1e6
	    if (type == "air2vac")
		w = a * w
	    else if (type == "vac2air")
		w = w / a

	    print (w, label, >> out)
	}
	inlist = ""
end
