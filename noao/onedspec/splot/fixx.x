# FIXX - Check for bounds on x's

procedure fixx (eqx1, eqx2, eqy1, eqy2, x1, x2)

real	eqx1, eqx2, eqy1, eqy2, x1, x2

real	temp

begin
	if ((x1 - x2) * (eqx1 - eqx2) < 0.) {
	    temp = eqx2
	    eqx2 = eqx1
	    eqx1 = temp
	
	    temp = eqy2
	    eqy2 = eqy1
	    eqy1 = temp
	}

	eqx1 = max (min (x1, x2), min (max (x1, x2), eqx1))
	eqx2 = max (min (x1, x2), min (max (x1, x2), eqx2))
end
