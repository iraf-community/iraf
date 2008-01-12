include <gset.h>
include "igi.h"

# 21 May 1997 - WJH 
#
# Create the Postscript string which will define the 
# user-specified font.  
# This command will automatically set 
#	'FONTSET' to 'HARD_FONTS' or 'GIO_FONTS' to write out 
#	the POSTSCRIPT definition string for the font.
#
# This function is based on MGOSTR, but hard-wires
# the necessary settings for this particular command
# Since this is creating a POSTSCRIPT command, and not a 
# text string to be printed on the page, the following settings
# are fixed for this command:
#      XPOS,YPOS = 0.0,0.0
#      ANGLE = 0.0
#      SIZE = 1.0
#      JUSTIFICATION = 6 
# This routine only uses the functionality which handles 
# 'FONTSET=HARD' conditions, therefore, this command can be 
# given anywhere in the IGI script prior to usage of the
# USER font in an label.  
  
procedure ig_psfont (igs)

pointer	igs		# igi parameters structure

pointer	sp, line, psfont

begin
	call lcmdcat (igs, YES)

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (psfont, SZ_LINE, TY_CHAR)

	call igstarg (igs, Memc[line], SZ_LINE)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("User font:  %s ")
		call pargstr (Memc[line])
	}
	
	call ii_psfnt (igs, Memc[line], Memc[psfont])

	call cmdcat (igs, YES)
	call sfree  (sp)
end


procedure ii_psfnt (igs, label, font)

pointer	igs		# igi parameters structure
char	label[ARB]
char	font[ARB]

pointer		gp
real	wl, wr, wb, wt
real	vl, vr, vb, vt

begin
	call gseti (GIO_GP(igs), G_CLIP, NO)

	call setltype (igs, SOLID_LINE)

	# The following code comes from 'MGOSTR' with the 
	# settings assumed to be those shown in the call.		
	#call mgostr (igs, 0.0, 0.0, label, 1.0, 0.0, 6, 4)

	gp = GIO_GP(igs)

	call ggview (gp, vl, vr, vb, vt)
	call ggwind (gp, wl, wr, wb, wt)

	# Don't clip at viewport boundary;  allow labels outside axes
	call gseti (gp, G_CLIP, NO)

	# Send the string '/PF /<font-name> def' directly to Postscript
	#  	The escape sequence will be picked up in 'psitx.x'
	# 	and sent to 'ps_out' without being modified.
	call sprintf(font, SZ_LINE, "%s\%s%sfU/PF /%s def")
		call pargstr(TEXT_ESCAPE)
		call pargstr(TEXT_ESCAPE)
		call pargstr(TEXT_ESCAPE)
		call pargstr(label)

	# Use GIO (possibly hardware) characters
	call giostr (gp, font, 0.0, 0.0, 6, 1.0, 0.0)
	
	call gamove (GIO_GP(igs), 0.0, 0.0)
	call gflush (GIO_GP(igs))
end
