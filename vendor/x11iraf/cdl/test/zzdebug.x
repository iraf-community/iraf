include	"../cdlspp.h"

#  ZZDEBUG -- Quickie demo tasks of the CDL SPP language binding.


task display = t_display,
     tvmark  = t_tvmark,
     rimcur  = t_rimcur


procedure t_display ()
int	ier
begin
	call cdl_open ("", ier)
	call cdl_displayIRAF ("/iraf/iraf/dev/pix.imh", 1, 1, 1, 1, ier)
	call cdl_close ()	
end

procedure t_tvmark ()
int	ier
begin
	call cdl_open ("", ier)
	call cdl_displayIRAF ("/iraf/iraf/dev/pix.imh", 1, 1, 1, 1, ier)
	call cdl_markCoordsFile ("coords", M_PLUS, 11, C_GREEN, YES, ier)
	call cdl_close ()	
end

procedure t_rimcur ()
int	ier
char	key
real	x, y
begin
	call cdl_open ("", ier)
	key = 'a'
	while (key != 'q' && key != EOS) {
	    call cdl_readCursor (0, x, y, key, ier)
	    call printf ("x=%.2g  y=%.2g  key='%c'  ier=%d\n")
		call pargr (x)
		call pargr (y)
		call pargc (key)
		call pargi (ier)
	}
	call cdl_close ()	
end
