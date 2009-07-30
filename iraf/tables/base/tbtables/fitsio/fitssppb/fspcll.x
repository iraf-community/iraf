include "fitsio.h"

procedure fspcll(ounit,colnum,frow,felem,nelem,lray,status)

# write an array of logical values to the  specified column of the table.
# The binary table column being written to must have datatype 'L'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
long	frow            # i first row
long	felem           # i first element in row
long	nelem           # i number of elements
bool    lray[ARB]       # i logical array
int     status          # o error status

int	i_frow
int	i_felem
int	i_nelem

begin
	i_frow = frow
	i_felem = felem
	i_nelem = nelem
	call ftpcll(ounit,colnum,i_frow,i_felem,i_nelem,lray,status)
end
