# The array XA is searched for an element KLO such that
#      xa(klo) <= x <= xa(klo+1)
# If X < XA(1) then KLO is set to zero; if X > XA(N) then KLO is set to N.
# That is, KLO = 0 or N is a flag indicating X is out of bounds.
#
# KLO must be set to an initial guess on input; it will then be replaced
# by the correct value on output.
#
# Copyright (c) 2017 Ole Streicher
#
procedure tuhunt (xa, n, x, klo)
int	n	# i: number of elements in each array
int	klo	# i: array of independent-variable values
double	xa[n]	# i: the value to be bracketed by elements in XA
double	x	# io: the lower index in XA that brackets X

int low, up
begin
    if (xa[n] > xa[1]) {	# increasing
        if (xa[1] > x) {
            klo = 0
	    return
        }
        if (xa[n] < x) {
            klo = n
            return
        }
        klo = min(max(klo, 1), n-1)
        low = 1
        up = n-1
        while (true) {
            if (x < xa[klo]) {
                up = klo-1
                klo = (up + low)/2
            } else if (x > xa[klo+1]) {
                low = klo+1
                klo = (up + low)/2
            } else {
                return
            }
        }
    } else {			# decreasing
        if (xa[1] < x) {
            klo = 0
	    return
        }
        if (xa[n] > x) {
            klo = n
            return
        }
        klo = min(max(klo, 1), n-1)
        low = 1
        up = n-1
        while (true) {
            if (x > xa[klo]) {
                up = klo-1
                klo = (up + low)/2
            } else if (x < xa[klo+1]) {
                low = klo+1
                klo = (up + low)/2
            } else {
                return
            }
        }
    }
end
