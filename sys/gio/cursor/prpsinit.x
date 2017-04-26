# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PRPSINIT -- Load the gio.cursor graphics driver for pseudofile i/o to the
# graphics streams.

procedure prpsinit()

extern	giotr()
extern	gtr_control(), gtr_gflush(), gtr_writep()
extern	stg_readtty(), stg_writetty()

begin
	call prpsload (giotr, gtr_control, gtr_gflush, gtr_writep,
	    stg_readtty, stg_writetty)
end
