include <math.h>
include <pkg/skywcs.h>

# RG_LLTRANSFORM -- Transform the reference image world coordinates to the
# input image world coordinate system.

procedure rg_lltransform (cooref, cooin, rxlng, rylat, ixlng, iylat, npts)

pointer	cooref		#I pointer to the reference image coordinate structure
pointer	cooin		#I pointer to the input image coordinate structure
double	rxlng[ARB]	#I the x refererence image world coordinates (degrees)
double	rylat[ARB]	#I the y refererence image world coordinates (degrees)
double	ixlng[ARB]	#O the x refererence image world coordinates (degrees)
double	iylat[ARB]	#O the y refererence image world coordinates (degrees)
int	npts		#I the number of coordinates

int	i
double	ilng, ilat, olng, olat
int	sk_stati()

begin
	if (sk_stati (cooref, S_PLNGAX) < sk_stati (cooref, S_PLATAX)) {
	    do i = 1, npts {
	        ilng = DEGTORAD (rxlng[i])
	        ilat = DEGTORAD (rylat[i])
	        call sk_lltran (cooref, cooin, ilng, ilat, INDEFD,
		    INDEFD, 0.0d0, 0.0d0, olng, olat)
	        ixlng[i] = RADTODEG (olng)
	        iylat[i] = RADTODEG (olat)
	    }
	} else {
	    do i = 1, npts {
	        ilng = DEGTORAD (rylat[i])
	        ilat = DEGTORAD (rxlng[i])
	        call sk_lltran (cooref, cooin, ilng, ilat, INDEFD,
		    INDEFD, 0.0d0, 0.0d0, olng, olat)
	        ixlng[i] = RADTODEG (olat)
	        iylat[i] = RADTODEG (olng)
	    }
	}
end

