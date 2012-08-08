c ARGS -- Test the command line argument interface.
c
c	usage:  args [arg1 [arg2 ...]]
c ------------------------------------------------------------------------

	program args

	character*80	argstr
	integer		nargs, ier, i

c --- Test raw command line access.
	call clrawc (argstr, ier)
	if (ier .ne. 0) then
	    write (*, '('' clrawc returns status '', i3)') ier
	else
	    write (*, '('' clrawc: '', a80)') argstr
	endif

c --- Test parsed command line access.
	call clnarg (nargs)
	write (*, '('' nargs = '', i3)') nargs

	do 10 i = 1, nargs
	    call clargc (i, argstr, ier)
	    if (ier .ne. 0) then
		write (*, '('' unexpected error '', i3)') ier
	    else
		write (*, '(i4, 2x, a70)') i, argstr
	    endif
 10	continue

	stop
	end
