include "fitsio.h"

procedure fspclu(ounit,colnum,frow,felem,nelem,status)

# set elements of a table to be undefined

int     ounit           # i output file pointer
int     colnum          # i column number
long	frow            # i first row
long	felem           # i first element in row
long	nelem           # i number of elements
int     status          # o error status

int	i_frow
int	i_felem
int	i_nelem

begin
	i_frow = frow
	i_felem = felem
	i_nelem = nelem
	call ftpclu(ounit,colnum,i_frow,i_felem,i_nelem,status)
end
