# FITSSPP.COM -- Common block definitions used in fitsspp.x.

define	NB		20		# number of file buffers
define	NE		200		# maximun allowed number of extensions
					#  in the FITS files
define	MAXFILES	199		# more than needed

# The following common is used throughout the fitsio code.
int	bufnum, chdu, hdutyp, maxhdu, hdstrt, hdend, nxthdr, dtstrt
int     nxtfld
bool	wrmode

common	/ft0001/ bufnum[MAXFILES],chdu[NB],hdutyp[NB],maxhdu[NB],
	wrmode[NB],hdstrt[NB,NE],hdend[NB],nxthdr[NB],dtstrt[NB],nxtfld

int	compid
common	/ftcpid/compid

int	buflun, reclen, bytnum, filesize, recnum, bufid

common	/ftsbuf/buflun[NB],reclen[NB],
	bytnum[NB],filesize[NB],recnum[NB],bufid[MAXFILES]

