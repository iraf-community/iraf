# This is the non-VMS version.  A character string variable is passed
# to an integer array argument.
#
#     FTPCBF -- Write a sequence of characters to a file. 

# FTPCBF -- Write a sequence of characters to a file.  The sequence may begin 
# on any byte boundary and may be any number of bytes long.

procedure ftpcbf (iunit, convrt, nbytes, array, status)

int	iunit		#I fortran unit number
int	convrt		#I convert to ASCII? (not used in SPP version)
int	nbytes		#I number of bytes to be transferred
%	character*(*)   array
int	status		#U output error status

begin
	# Write the data.  Won't work on a VAX.
        call ftpbyt (iunit, nbytes, array, status)
end
