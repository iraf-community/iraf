include "fitsio.h"

procedure fspcls(ounit,colnum,frow,felem,nelem,sray,dim1,status)

# write an array of character string values to the  specified column of
# the table.
# The binary or ASCII table column being written to must have datatype 'A'

int     ounit           # i output file pointer
int     colnum          # i column number
long	frow            # i first row
long	felem           # i first element in row
long	nelem           # i number of elements
char    sray[dim1,ARB]       # i array of strings
int     dim1            # i size of 1st dimension of 2D character string array
%       character*256 fsray
int     status          # o error status
int	i
int	elem

int	i_frow
int	i_nelem

begin
	i_frow = frow
	i_nelem = nelem

	elem=felem
	do i=1,i_nelem            {
	    call f77pak(sray(1,i),fsray,dim1)
	    call ftpcls(ounit,colnum,i_frow,elem,1,fsray,status)
	    elem=elem+1
	}
end
