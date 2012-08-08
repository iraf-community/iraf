# VOIGT -- Compute the real (Voigt function) and imaginary parts of the
# complex function w(z)=exp(-z**2)*erfc(-i*z) in the upper half-plane
# z=x+iy.  The maximum relative error of the real part is 2E-6 and the
# imaginary part is 5E-6.
#
# From: Humlicek, J. Quant. Spectrosc. Radiat. Transfer, V21, p309, 1979.

procedure voigt (xarg, yarg, wr, wi)

real	xarg		#I Real part of argument
real	yarg		#I Imaginary part of argument
real	wr		#O Real part of function
real	wi		#O Imaginary part of function

int	i
real	x, y, y1, y2, y3, d, d1, d2, d3, d4, r, r2
real	t[6], c[6], s[6]

data	t/.314240376,.947788391,1.59768264,2.27950708,3.02063703,3.8897249/
data	c/1.01172805,-.75197147,1.2557727e-2,1.00220082e-2,-2.42068135e-4,
	  5.00848061e-7/
data	s/1.393237,.231152406,-.155351466,6.21836624e-3,9.19082986e-5,
	  -6.27525958e-7/

begin
	x = xarg
	y = abs (yarg)
	wr = 0.
	wi = 0.
	y1 = y + 1.5
	y2 = y1 * y1

	# Region II
	if (y < 0.85 && abs(x) > 18.1*y+1.65) {
	    if (abs(x) < 12)
		wr = exp (-x * x)
	    y3 = y + 3
	    do i = 1, 6 {
		r = x - t[i]
		r2 = r * r
		d = 1 / (r2 + y2)
		d1 = y1 * d
		d2 = r * d
		wr = wr + y * (c[i] * (r * d2 - 1.5 * d1) + s[i] * y3 * d2) /
		    (r2 + 2.25)
		r = x + t[i]
		r2 = r * r
		d = 1 / (r2 + y2)
		d3 = y1 * d
		d4 = r * d
		wr = wr + y * (c[i] * (r * d4 - 1.5 * d3) - s[i] * y3 * d4) /
		    (r2 + 2.25)
		wi = wi + c[i] * (d2 + d4) + s[i] * (d1 - d3)
	    }

	# Region I
	} else {
	    do i = 1, 6 {
		r = x - t[i]
		d = 1 / (r * r + y2)
		d1 = y1 * d
		d2 = r * d
		r = x + t[i]
		d = 1 / (r * r + y2)
		d3 = y1 * d
		d4 = r * d
		wr = wr + c[i] * (d1 + d3) - s[i] * (d2 - d4)
		wi = wi + c[i] * (d2 + d4) + s[i] * (d1 - d3)
	    }
	}
end
