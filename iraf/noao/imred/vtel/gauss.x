procedure gauss (x, a, ymod, dyda, ma)

real	x, a[ma], ymod, dyda[ma]
int	ma

real	arg, ex, fac

begin
	arg = (x - a(2))/a(3)
	ex = exp(-arg**2)
	fac = a(1)*ex*2.0*arg
	ymod = a(1)*ex
	dyda(1) = ex
	dyda(2) = fac/a(3)
	dyda(3) = fac*arg/a(3)
end
