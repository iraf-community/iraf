# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>

# PRPSLOAD -- Must be called at process startup time to initialize pseudofile
# i/o for the graphics streams, if any i/o to the graphics pseudofiles is
# expected.  The arguments are the LOCPR entry point addresses of the graphics
# driver (gio.cursor) procedures to be called to process i/o requests on the
# graphics streams.

procedure prpsload (giotr, control, gflush, writep, readtty, writetty)

extern	giotr()			#I gio.cursor driver procedures
extern	control()		#I	"		"
extern	gflush()		#I	"		"
extern	writep()		#I	"		"
extern	readtty()		#I	"		"
extern	writetty()		#I	"		"

int	locpr()
include	"prc.com"

begin
	epa_giotr	= locpr (giotr)
	epa_control	= locpr (control)
	epa_gflush	= locpr (gflush)
	epa_writep	= locpr (writep)
	epa_readtty	= locpr (readtty)
	epa_writetty	= locpr (writetty)
end
