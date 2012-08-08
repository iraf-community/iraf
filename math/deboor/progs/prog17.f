c  main program for example in chapter xv.
c  from  * a practical guide to splines *  by c. de boor
c  solution of a second order nonlinear two point boundary value
c  problem  on (0., 1.) , by collocation with pp functions having  4
c  pieces of order  6 .  2  passes through newnot are to be made,
c  without any knots being added. newton iteration is to be stopped
c  when two iterates agree to  6  decimal places.
       call colloc(0.,1.,4,6,2,0.,1.e-6)
					stop
       end
