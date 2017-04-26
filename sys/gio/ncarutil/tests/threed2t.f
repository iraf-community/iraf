	subroutine tst3d2 ()
	real eye(3)
	dimension u(50), v(50), w(50)
	data eye /5., -10., 4./
	isiz = 36
	xs = 90. / 1024.
	xe = 1010. / 1024.
	ys = 90. / 1024.
	ye = 1010. / 1024.
	call tick43 (24, 16, 24, 16, 24, 16)
c	call set3 (90, 1010, 90, 1010, 0., 2., -1., 1., 0., 1., eye)
	call set3 (xs, xe, ys, ye, 0., 2., -1., 1., 0., 1., eye)
	do 1 i = 1, 50
	    u(i) = float(i) * .04
	    v(i) = sin (u(i) * 6.) * float (80 - i) / 80.
	    w(i) = .5 + sin (u(i) *3.141592) * .5
  1	continue
	call perim3 (2,5,1,5,1,0.)
	call perim3 (2,5,1,5,2,-1.)
	call perim3 (2,5,2,5,3,0.)
	call pwrzt (2.1, -1., 0., 3hU->, 3, isiz, 1,3,-1)
	call pwrzt (0., 1.1, 0., 3hV->, 3, isiz, 2,3,0)
	call pwrzt (0., -1., 1.1, 2hW , 2, isiz, 3, -1, 0)
	call fence3 (u, v, w, 50, 3, 0.)
	end

