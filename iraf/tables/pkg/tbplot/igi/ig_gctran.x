include	<gio.h>

#---------------------------------------------------------------------------
.help ig_gctran Aug95 source
.ih
NAME
ig_gctran -- Igi graphics wcs convert
.ih
DESCRIPTION
This routine is a utility routine to get around a bug in GIO.  gctran
caches its wcs information, however it does not have a reliable method
of determining when the cache is out of date.  This routine forces the
a know out-of-date condition so the gctran operates correctly.
.endhelp
#---------------------------------------------------------------------------
procedure ig_gctran (gp, x1, y1, x2, y2, wcs_a, wcs_b)

pointer	gp			# I:  graphics descriptor
real	x1, y1			# I:  coords of point in WCS_A (input)
real	x2, y2			# O:  coords of point in WCS_B (output)
int	wcs_a			# I:  input WCS
int	wcs_b			# I:  output WCS

# Declarations
int	o			# Old wcs state

begin
	o = GP_WCSSTATE(gp)
	GP_WCSSTATE(gp) = MODIFIED
	call gctran (gp, x1, y1, x2, y2, wcs_a, wcs_b)
	GP_WCSSTATE(gp) = o
end
#---------------------------------------------------------------------------
# End of ig_gctran
#---------------------------------------------------------------------------
