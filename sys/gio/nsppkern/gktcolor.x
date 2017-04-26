# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gkt.h"

# nspp particulars
# colors
define	BLACK		1
define	WHITE		2
define	RED		3
define	GREEN		4
define	BLUE		5

# GKT_COLOR  set the color option in the nspp world

procedure gkt_color(index)

int	index		# index for color switch statement
include	"gkt.com"

begin
	switch (index) {
	    case WHITE:
		call optn (*"co", *"white")
	    case RED:
		call optn (*"co", *"red")
	    case GREEN:
		call optn (*"co", *"green")
	    case BLUE:
		call optn (*"co", *"blue")
	    default:
		call optn (*"co", *"black")
	}
end
