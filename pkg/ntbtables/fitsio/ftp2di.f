C--------------------------------------------------------------------------
        subroutine ftp2di(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of i*2 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   i*2  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        integer*2 array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftppri(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
