# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>


# IC_RMASKS --  Set pixels for rejection mask.

procedure ic_rmasks (pm, v, id, nimages, n, npts)

pointer	pm			#I Pixel mask
long	v[ARB]			#I Output vector (input)
pointer	id[nimages]		#I Image id pointers
int	nimages			#I Number of images
int	n[npts]			#I Number of good pixels
int	npts			#I Number of output points per line

int	i, j, k, ndim, impnls()
long	v1[IM_MAXDIM]
pointer	buf

begin
	ndim = IM_NDIM(pm)
	do k = 1, nimages {
	    call amovl (v, v1, ndim-1)
	    v1[ndim] = k
	    i = impnls (pm, buf, v1)
	    do j = 1, npts {
		if (n[j] == nimages)
		    Mems[buf+j-1] = 0
		else {
		    Mems[buf+j-1] = 1
		    do i = 1, n[j] {
			if (Memi[id[i]+j-1] == k) {
			    Mems[buf+j-1] = 0
			    break
			}
		    }
		}
	    }
	}
end
