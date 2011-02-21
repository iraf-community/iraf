C--------------------------------------------------------------------------
        subroutine ftpsse(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of real values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   r  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        real array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7
        character caxis*20

        if (status .gt. 0)return

        if (naxis .lt. 1 .or. naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=320
                write(caxis,1001)naxis
1001            format(i20)
                call ftpmsg('NAXIS ='//caxis//' in the call to FTPSSE '
     &          //'is illegal.')
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftppre(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
