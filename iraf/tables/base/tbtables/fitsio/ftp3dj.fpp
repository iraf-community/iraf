C--------------------------------------------------------------------------
        subroutine ftp3dj(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of i*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   i  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        integer array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftpprj(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
