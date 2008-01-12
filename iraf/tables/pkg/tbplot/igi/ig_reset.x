include "igi.h"

#  IG_RESET -- Set the igi parameters to default values.

#  8/20/91  Removed ^Ls. ZGL
#  9/17/91  Changed default marker type to cross.  ZGL
#  11/4/91  Added color index initialization.  ZGL
#  6/17/92  Added font set structure initialization.  ZGL
#  6/19/92  Added pixmap structure initialization.  ZGL
## 6/25/92  Added Z range initialization.  ZGL
## 7/17/92  Added fill pattern initialization.  ZGL
## 7/27/92  Assign MG_ZNPTS.  ZGL

procedure ig_reset (igs)

pointer	igs		# igi structure

begin
	call ii_reset (igs)
end


procedure ii_reset (igs)

# II_RESET -- Set all settable igi parameters to their defaults.  Many
# are INDEF or null strings.
#
# 2/11/91 Added the default null string MG_TICKFMT.  ZGL
# 10/7/92 Use symbol for fill pattern default (HOLLOW_FILL).  ZGL

pointer	igs		# igi structure

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	# Data source
	MG_FILE_NAME(igps) = EOS
	MG_DATASRC(igps)   = NO_DATA
	MG_COLNAME(igps)   = EOS
	MG_COLNUM(igps)    = INDEFI

	# Range of input data rows
	MG_FROW(igps) = INDEFI
	MG_LROW(igps) = INDEFI

	# Data vectors
	MG_XDATAP(igps) = NULL
	MG_XNPTS(igps)  = 0
	MG_YDATAP(igps) = NULL
	MG_YNPTS(igps)  = 0
	MG_ZDATAP(igps) = NULL
	MG_ZNPTSX(igps) = 0
	MG_ZNPTSY(igps) = 0
	MG_ZNPTS(igps)  = 0
	MG_EDATAP(igps) = NULL
	MG_ENPTS(igps)  = 0
	MG_PDATAP(igps) = NULL
	MG_PNPTS(igps)  = 0
	MG_LDATAP(igps) = NULL
	MG_LNPTS(igps)  = 0
	MG_SDATAP(igps) = NULL
	MG_SNPTS(igps)  = 0

	MG_NPTS(igps)   = 0

	# Use image WCS for X vector?
	MG_IMGWCS(igps) = NO

	MG_DRAW(igps)   = YES

	# Line style
	call strcpy ("solid", MG_LTYPE(igps), SZ_LINE)
	MG_LTYPEN(igps)   = 0
	MG_LWEIGHT(igps)  = 1.0
	MG_CHARSIZE(igps) = MG_DEF_CHARSIZE

	# Color Index
	call ii_color (igs, 1)

	# Global atributes
	call ii_expand (igs, 1.0001)
	call ii_angle (igs, 0.0)
	call ii_justify (igs, 6)

	# Text attributes
	MG_SLANT(igps)   = MG_DEF_SLANT
	MG_SUPFRAC(igps) = MG_DEF_SUPFRAC

	# Use igi fonts
	MG_FONTSET(igps) = IGI_FONTS

	# Point style;  defaults to cross
	MG_PTYPN(igps)   = 4
	MG_PTYPS(igps)   = SKELETAL_MARKER
	call ptypic (MG_PTYPS(igps), MG_PTYPE(igps), SZ_LINE)

	MG_PNTSIZE(igps) = MG_DEF_PNTSIZE
	MG_PNTFILL(igps) = MG_DEF_PNTFILL / MG_LWEIGHT(igps)  
	MG_STELLAR(igps) = MG_DEF_STELLAR

	MG_XPOS(igps) = INDEFR
	MG_YPOS(igps) = INDEFR

	MG_XLOG(igps) = NO
	MG_YLOG(igps) = NO

	# Default virtual page
	call ii_vpage (igs, 0.0, 1.0, 0.0, 1.0)
	call ii_location (igs, 0.1, 0.95, 0.1, 0.95)

	# Reset the GIO viewport
	call vpage (igs)

	# Plot scale and axis labeling
	call ii_limits (igs, 0.0, 1.0, 0.0, 1.0)
	call ii_ticksize (igs, INDEFR, INDEFR, INDEFR, INDEFR)
	call ii_notation (igs, INDEFR, INDEFR, INDEFR, INDEFR)

	# Major tick spacing (set by BOX)
	MG_GSTEP(igps)  = INDEF
	MG_GXSTEP(igps) = INDEF
	MG_GYSTEP(igps) = INDEF

	# Sexagesimal notation
	MG_SEXAGX(igps) = NO
	MG_SEXAGY(igps) = NO
	MG_SEXAGS(igps) = NO

	MG_NDECMX(igps) = INDEFI
	MG_NDECMY(igps) = INDEFI
	MG_NDECIM(igps) = INDEFI

	# Axis tick label format
	MG_TICKFMT[igps] = EOS

	# Plot labels
	MG_XLABEL(igps) = EOS
	MG_YLABEL(igps) = EOS
	MG_TITLE(igps)  = EOS

	# Z data scaling
	MG_ZMIN(igps)   = INDEF
	MG_ZMAX(igps)   = INDEF
	MG_CMAPP(igps)  = NULL
	MG_CMNPTS(igps) = 0
	MG_ZFUNC(igps)  = MG_PMF_NONE

	# Fill pattern;  default to outline (hollow)
	MG_FILLPAT(igps) = HOLLOW_FILL

	# Range of color map indexes to use
	MG_CMMIN(igps) = 0
	MG_CMMAX(igps) = 255
end
