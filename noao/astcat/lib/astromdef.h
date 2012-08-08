# The private astrometry package definitions.


# Define the astrometry package structure. 

define	LEN_ASTROM	10

define	AT_PIO		Memi[$1]	     # the i/o structure
define	AT_PRCENTER	Memi[$1+1]	     # the region definition structure
define	AT_PFILTER 	Memi[$1+2]	     # the catalog filtering structure
define	AT_PWCS 	Memi[$1+3]	     # the image wcs structure
define	AT_PIMPARS 	Memi[$1+4]	     # the image data structure


# Define the i/o substructure

define	IO_SZ_FNAME	(SZ_FNAME + 1) / 2	
define	LEN_PIO		(10 + 12 * IO_SZ_FNAME) 

define	AT_CATALOGS	Memc[P2C($1+10)]
define	AT_SURVEYS	Memc[P2C($1+10+IO_SZ_FNAME)]
define	AT_IMAGES	Memc[P2C($1+10+2*IO_SZ_FNAME)]	
define	AT_INPUT	Memc[P2C($1+10+3*IO_SZ_FNAME)]
define	AT_OUTPUT	Memc[P2C($1+10+4*IO_SZ_FNAME)]
define	AT_CATNAME	Memc[P2C($1+10+5*IO_SZ_FNAME)]
define	AT_SVNAME	Memc[P2C($1+10+6*IO_SZ_FNAME)]
define	AT_IMNAME	Memc[P2C($1+10+7*IO_SZ_FNAME)]
define	AT_INFNAME	Memc[P2C($1+10+8*IO_SZ_FNAME)]
define	AT_OUTFNAME	Memc[P2C($1+10+9*IO_SZ_FNAME)]
define	AT_CATDB	Memc[P2C($1+10+10*IO_SZ_FNAME)]
define	AT_IMDB		Memc[P2C($1+10+11*IO_SZ_FNAME)]


# Define the field center substructure

define	RC_SZ_FNAME	(SZ_FNAME + 1) / 2	
define  LEN_PRCENTER    (12 + 2 * RC_SZ_FNAME)

define  AT_RCRA       Memd[P2D($1)]           # the field center ra / lon
define  AT_RCDEC      Memd[P2D($1+2)]         # the field center dec / lat
define  AT_RCRAWIDTH  Memd[P2D($1+4)]         # the field ra / lon width
define  AT_RCDECWIDTH Memd[P2D($1+6)]         # the field dec / lat width
define  AT_RCRAUNITS  Memi[$1+8]              # the ra / lon units
define  AT_RCDECUNITS Memi[$1+9]              # the dec / lat units
#define	AT_RCCC	      Memi[$1+10]             # the field center cc structure
define	AT_RCST	      Memi[$1+11]             # the field center symbol table
define  AT_RCSYSTEM   Memc[P2C($1+12)]        # the field center cc system
define  AT_RCSOURCE   Memc[P2C($1+12+RC_SZ_FNAME)] # the field center cc system

# Define the field filtering subtructure.

define	FS_SZ_FNAME	(SZ_FNAME + 1) / 2
define	FS_SZ_LINE	(SZ_LINE + 1) / 2

define	LEN_PFILTER	(10+12*FS_SZ_FNAME+6*FS_SZ_LINE)
define	F1OFFSET	P2C($1+10+$2*FS_SZ_FNAME)
define	F2OFFSET	P2C($1+10+12*FS_SZ_FNAME+$2*FS_SZ_LINE)

define	AT_FREVERSE	Memi[$1]
define	AT_FREPLACE	Memi[$1+1]
define	AT_FORAUNITS	Memi[$1+2]
define	AT_FODECUNITS	Memi[$1+3]

define	AT_FSORT	Memc[F1OFFSET($1,0)]
define	AT_FOSYSTEM	Memc[F1OFFSET($1,1)]
define	AT_FIRA		Memc[F1OFFSET($1,2)]
define	AT_FIDEC	Memc[F1OFFSET($1,3)]
define	AT_FORAFORMAT	Memc[F1OFFSET($1,4)]
define	AT_FODECFORMAT	Memc[F1OFFSET($1,5)]
define	AT_FIXP		Memc[F1OFFSET($1,6)]
define	AT_FIYP		Memc[F1OFFSET($1,7)]
define	AT_FIXC		Memc[F1OFFSET($1,8)]
define	AT_FIYC		Memc[F1OFFSET($1,9)]
define	AT_FOXFORMAT	Memc[F1OFFSET($1,10)]
define	AT_FOYFORMAT	Memc[F1OFFSET($1,11)]

define	AT_FIELDS	Memc[F2OFFSET($1,0)]
define	AT_FNAMES	Memc[F2OFFSET($1,1)]
define	AT_FNTYPES	Memc[F2OFFSET($1,2)]
define	AT_FNUNITS	Memc[F2OFFSET($1,3)]
define	AT_FNFORMATS	Memc[F2OFFSET($1,4)]
define	AT_FEXPR	Memc[F2OFFSET($1,5)]
