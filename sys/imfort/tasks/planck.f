c PLANCK -- Compute the Planck blackbody radiation distribution for a
c given temperature and wavelength region.
c
c       usage:  planck temperature lambda1 lambda2
c
c The temperature is specified in degrees Kelvin and the wavelength
c region in microns (1u=10000A).  100 [x,y] data points defining the
c curve are output.
c ----------------------------------------------------------------------

        program planck

	character*80	errmsg
        integer         nargs, ier, i
        real            w1, w2, dw, cm, t
        real            xv(100), yv(100)

c --- Get the temperature in degrees kelvin.
        call clargr (1, t, ier)
        if (ier .ne. 0) then
            write (*, '('' temperature (degrees kelvin): '',$)')
            read (*,*) t
        endif

c --- Get the wavelength region to be computed.
        call clnarg (nargs)
        if (nargs .ge. 3) then
            call clargr (2, w1, ier)
            if (ier .ne. 0) goto 91
            call clargr (3, w2, ier)
            if (ier .ne. 0) goto 91
        else
            write (*, '('' start wavelength (microns): '',$)')
            read (*,*) w1
            write (*, '('' end wavelength (microns): '',$)')
            read (*,*) w2
        endif

c --- Compute the blackbody curve.
        dw = (w2 - w1) / 99.0
        do 10 i = 1, 100
            xv(i) = ((i-1) * dw) + w1
            cm = xv(i) * 1.0E-4
            yv(i) = (3.74185E-5 * (cm ** -5)) /
     *          (2.71828 ** (1.43883 / (cm * t)) - 1.0)
 10	continue

c --- Print the curve as a table.
        do 20 i = 1, 100
	    write (*, '(1x, f7.4, g12.4)') xv(i), yv(i)
 20	continue

	stop

c --- Error exit. 
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg
	stop
	end
