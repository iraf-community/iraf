c subroutine fderiv.f (non-analytical)
c
c source
c   Bevington, page 242.
c
c purpose
c   evaluate derivatives of function for least-squares search
c      for arbitrary function given by functn
c
c usage
c   call fderiv (x, i, a, deltaa, nterms, deriv)
c
c description of parameters
c   x	   - array of data points for independent variable
c   i	   - index of data points
c   a	   - array of parameters
c   deltaa - array of parameter increments
c   nterms - number of parameters
c   deriv  - derivatives of function
c
c subroutines and function subprograms required
c   functn (x, i, a)
c      evaluates the fitting function for the ith term
c
	subroutine fderiv (x,i,a,deltaa,nterms,deriv)
	dimension x(1),a(1),deltaa(1),deriv(1)
        real FUNCTN
        external FUNCTN

11	do 18 j=1,nterms
	aj=a(j)
	delta=deltaa(j)
	a(j)=aj+delta
	yfit=functn(x,i,a)
	a(j)=aj-delta
	deriv(j)=(yfit-functn(x,i,a))/(2.*delta)
18	a(j)=aj
	return
	end
