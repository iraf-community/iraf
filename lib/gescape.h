# GESCAPE.H -- Escape codes for the GIO gescape routine.  These are GKI
# functions not part of the standard GIO/GKI drawing function set.  Usually
# only specific graphics kernels will respond to these escape codes.
# Other kernels ignore these escapes.

# Plotter escapes.
# --------------------------------

define  GSC_X_GKITODEV		1	# x scale; devunits/GKI_MAXNDC
define  GSC_Y_GKITODEV		2	# y scale; devunits/GKI_MAXNDC


# Gterm widget imaging functions.
# --------------------------------

define	GIM_RASTERINIT		10	# initialize imaging subsystem
define	GIM_RASTERINIT_LEN	0

define	GIM_CREATERASTER	11	# create or resize a raster
define	GIM_CREATERASTER_LEN	5
define	GIM_CREATERASTER_RN	1
define	GIM_CREATERASTER_RT	2
define	GIM_CREATERASTER_NX	3
define	GIM_CREATERASTER_NY	4
define	GIM_CREATERASTER_BP	5

define	GIM_DESTROYRASTER	12	# destroy a raster
define	GIM_DESTROYRASTER_LEN	1
define	GIM_DESTROYRASTER_RN	1

define	GIM_QUERYRASTER		13	# get raster type and size
define	GIM_QUERYRASTER_LEN	1
define	GIM_QUERYRASTER_RN	1

define	GIM_RET_QRAS_LEN	5	# return value for above
define	GIM_RET_QRAS_EX		1
define	GIM_RET_QRAS_RT		2
define	GIM_RET_QRAS_NX		3
define	GIM_RET_QRAS_NY		4
define	GIM_RET_QRAS_BP		5

define	GIM_SETRASTER		14	# set raster used for drawing context
define	GIM_SETRASTER_LEN	1
define	GIM_SETRASTER_RN	1

define	GIM_WRITEPIXELS		15	# write to a raster
define	GIM_WRITEPIXELS_LEN	7
define	GIM_WRITEPIXELS_RN	1
define	GIM_WRITEPIXELS_EC	2
define	GIM_WRITEPIXELS_X1	3
define	GIM_WRITEPIXELS_Y1	4
define	GIM_WRITEPIXELS_NX	5
define	GIM_WRITEPIXELS_NY	6
define	GIM_WRITEPIXELS_BP	7
define	GIM_WRITEPIXELS_DATA	8

define	GIM_READPIXELS		16	# read from a raster
define	GIM_READPIXELS_LEN	7
define	GIM_READPIXELS_RN	1
define	GIM_READPIXELS_EC	2
define	GIM_READPIXELS_X1	3
define	GIM_READPIXELS_Y1	4
define	GIM_READPIXELS_NX	5
define	GIM_READPIXELS_NY	6
define	GIM_READPIXELS_BP	7

define	GIM_RET_RPIX_LEN	1	# return value for above
define	GIM_RET_RPIX_NP		1

define	GIM_REFRESHPIXELS	17	# refresh a screen region
define	GIM_REFRESHPIXELS_LEN	6
define	GIM_REFRESHPIXELS_RN	1
define	GIM_REFRESHPIXELS_CT	2
define	GIM_REFRESHPIXELS_X1	3
define	GIM_REFRESHPIXELS_Y1	4
define	GIM_REFRESHPIXELS_NX	5
define	GIM_REFRESHPIXELS_NY	6

define	GIM_SETPIXELS		18	# set a region to a solid color
define	GIM_SETPIXELS_LEN	8
define	GIM_SETPIXELS_RN	1
define	GIM_SETPIXELS_CT	2
define	GIM_SETPIXELS_X1	3
define	GIM_SETPIXELS_Y1	4
define	GIM_SETPIXELS_NX	5
define	GIM_SETPIXELS_NY	6
define	GIM_SETPIXELS_CO	7
define	GIM_SETPIXELS_OP	8

define	GIM_WRITECMAP		19	# write to a colormap
define	GIM_WRITECMAP_LEN	3
define	GIM_WRITECMAP_MP	1
define	GIM_WRITECMAP_FC	2
define	GIM_WRITECMAP_NC	3
define	GIM_WRITECMAP_DATA	4

define	GIM_READCMAP		20	# read from a colormap
define	GIM_READCMAP_LEN	3
define	GIM_READCMAP_MP		1
define	GIM_READCMAP_FC		2
define	GIM_READCMAP_NC		3

define	GIM_RET_RCMAP_LEN	1	# return value for above
define	GIM_RET_RCMAP_NC	1

define	GIM_LOADCMAP		21	# load (and scale) colormap
define	GIM_LOADCMAP_LEN	4
define	GIM_LOADCMAP_MP		1
define	GIM_LOADCMAP_OF		2
define	GIM_LOADCMAP_DX		3
define	GIM_LOADCMAP_DY		4

define	GIM_LOADCMAP_SCALE	4

define	GIM_FREECMAP		22	# free a colormap
define	GIM_FREECMAP_LEN	1
define	GIM_FREECMAP_MP		1

define	GIM_WRITEIOMAP		23	# write to the iomap
define	GIM_WRITEIOMAP_LEN	2
define	GIM_WRITEIOMAP_FC	1
define	GIM_WRITEIOMAP_NC	2
define	GIM_WRITEIOMAP_DATA	3

define	GIM_READIOMAP		24	# read from the iomap
define	GIM_READIOMAP_LEN	2
define	GIM_READIOMAP_FC	1
define	GIM_READIOMAP_NC	2

define	GIM_RET_RIOMAP_LEN	1	# return value for above
define	GIM_RET_RIOMAP_NC	1

define	GIM_INITMAPPINGS	25	# destroy all mappings
define	GIM_INITMAPPINGS_LEN	0

define	GIM_FREEMAPPING		26	# free a mapping
define	GIM_FREEMAPPING_LEN	1
define	GIM_FREEMAPPING_MP	1

define	GIM_COPYRASTER		27	# copy part of a raster
define	GIM_COPYRASTER_LEN	13
define	GIM_COPYRASTER_OP	1
define	GIM_COPYRASTER_SR	2
define	GIM_COPYRASTER_ST	3
define	GIM_COPYRASTER_SX	4
define	GIM_COPYRASTER_SY	5
define	GIM_COPYRASTER_SW	6
define	GIM_COPYRASTER_SH	7
define	GIM_COPYRASTER_DR	8
define	GIM_COPYRASTER_DT	9
define	GIM_COPYRASTER_DX	10
define	GIM_COPYRASTER_DY	11
define	GIM_COPYRASTER_DW	12
define	GIM_COPYRASTER_DH	13

define	GIM_SETMAPPING		28	# set or modify a mapping
define	GIM_SETMAPPING_LEN	14
define	GIM_SETMAPPING_MP	1
define	GIM_SETMAPPING_OP	2
define	GIM_SETMAPPING_SR	3
define	GIM_SETMAPPING_ST	4
define	GIM_SETMAPPING_SX	5
define	GIM_SETMAPPING_SY	6
define	GIM_SETMAPPING_SW	7
define	GIM_SETMAPPING_SH	8
define	GIM_SETMAPPING_DR	9
define	GIM_SETMAPPING_DT	10
define	GIM_SETMAPPING_DX	11
define	GIM_SETMAPPING_DY	12
define	GIM_SETMAPPING_DW	13
define	GIM_SETMAPPING_DH	14

define	GIM_GETMAPPING		29	# get a mapping
define	GIM_GETMAPPING_LEN	1
define	GIM_GETMAPPING_MP	1

define	GIM_RET_GMAP_LEN	14	# return value for above
define	GIM_RET_GMAP_EN		1
define	GIM_RET_GMAP_OP		2
define	GIM_RET_GMAP_SR		3
define	GIM_RET_GMAP_ST		4
define	GIM_RET_GMAP_SX		5
define	GIM_RET_GMAP_SY		6
define	GIM_RET_GMAP_SW		7
define	GIM_RET_GMAP_SH		8
define	GIM_RET_GMAP_DR		9
define	GIM_RET_GMAP_DT		10
define	GIM_RET_GMAP_DX		11
define	GIM_RET_GMAP_DY		12
define	GIM_RET_GMAP_DW		13
define	GIM_RET_GMAP_DH		14

define	GIM_ENABLEMAPPING	30	# enable a mapping
define	GIM_ENABLEMAPPING_LEN	2
define	GIM_ENABLEMAPPING_MP	1
define	GIM_ENABLEMAPPING_FL	2

define	GIM_DISABLEMAPPING	31	# disable a mapping
define	GIM_DISABLEMAPPING_LEN	2
define	GIM_DISABLEMAPPING_MP	1
define	GIM_DISABLEMAPPING_FL	2

define	GIM_REFRESHMAPPING	32	# refresh a mapped region
define	GIM_REFRESHMAPPING_LEN	1
define	GIM_REFRESHMAPPING_MP	1
