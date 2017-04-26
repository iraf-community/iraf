C----------------------------------------------------------------------
        subroutine ftptdm(iunit,colnum,naxis,naxes,status)

C       write the TDIMnnn keyword describing the dimensionality of a column

C       iunit   i  fortran unit number to use for reading
C       colnum  i  column number to read
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, October 1993

        integer iunit,colnum,naxis,naxes(*),status

        integer i,j,nextsp
        character tdim*120, cval*20

        if (status .gt. 0)return

        if (naxis .lt. 1 .or. naxis .gt. 100)then
C               illegal number of axes
                status=320
                return
        else if (colnum .lt. 1 .or. colnum .gt. 999)then
C               illegal column number
                status=302
                return
        end if

C       construct the keyword value
        tdim='('

        nextsp=2
        do 100 i=1,naxis
                if (naxes(i) .lt. 1)then
                        status=323
                        return
                end if

C               convert integer to right justified C*20 string
                call fti2c(naxes(i),cval,status)
                if (status .gt. 0)return

                do 20 j=20,1,-1
                        if (cval(j:j) .eq. ' ')then
                                tdim(nextsp:)=cval(j+1:20)
                                nextsp=nextsp+21-j
                                tdim(nextsp-1:)=','
                                go to 100
                        end if
20              continue
100     continue

        tdim(nextsp-1:)=')'             

        call ftpkns(iunit,'TDIM',colnum,1,tdim,
     &          'size of the multidimensional array',status)
        end
