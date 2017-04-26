C----------------------------------------------------------------------------
        subroutine ftgsfd(iunit,colnum,naxis,naxes,blc,trc,inc,
     &  array,flgval,anynul,status)

C       read a subsection of double precision data values from an image or
C       a table column.  Returns an associated array of null value flags.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       naxis   i  number of dimensions in the FITS array
C       naxes   i  size of each dimension. 
C       blc     i  'bottom left corner' of the subsection to be read
C       trc     i  'top right corner' of the subsection to be read
C       inc     i  increment to be applied in each dimension
C       array   i  array of data values that are read from the FITS file
C       flgval  l  set to .true. if corresponding array element is undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1993

        integer iunit,colnum,naxis,naxes(*),blc(*),trc(*),inc(*),status
        double precision array(*),nulval
        logical anynul,anyf,flgval(*)

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        integer i,i1,i2,i3,i4,i5,i6,i7,i8,i9,row,rstr,rstp,rinc
        integer str(9),stp(9),incr(9),dsize(10)
        integer felem,nelem,nultyp,ninc,ibuff,numcol
        character caxis*20

C       this routine is set up to handle a maximum of nine dimensions

        if (status .gt. 0)return

        if (naxis .lt. 1 .or. naxis .gt. 9)then
                status=320
                write(caxis,1001)naxis
1001            format(i20)
                call ftpmsg('NAXIS ='//caxis//' in the call to FTGSFD '
     &          //'is illegal.')
                return
        end if

C       if this is a primary array, then the input COLNUM parameter should
C       be interpreted as the row number, and we will alway read the image
C       data from column 2 (any group parameters are in column 1).

        ibuff=bufnum(iunit)
        if (hdutyp(ibuff) .eq. 0)then
C               this is a primary array, or image extension
                if (colnum .eq. 0)then
                    rstr=1
                    rstp=1
                else
                    rstr=colnum
                    rstp=colnum
                end if
                rinc=1
                numcol=2
        else
C               this is a table, so the row info is in the (naxis+1) elements
                rstr=blc(naxis+1)
                rstp=trc(naxis+1)
                rinc=inc(naxis+1)
                numcol=colnum
        end if

        nultyp=2
        anynul=.false.
        i1=1
        do 5 i=1,9
                str(i)=1
                stp(i)=1
                incr(i)=1
                dsize(i)=1
5       continue
        do 10 i=1,naxis
                if (trc(i) .lt. blc(i))then
                        status=321
                        write(caxis,1001)i
        call ftpmsg('In FTGSFD, the range specified for axis '//
     &  caxis(19:20)//' has the start greater than the end.')
                        return
                end if
                str(i)=blc(i)
                stp(i)=trc(i)
                incr(i)=inc(i)
                dsize(i+1)=dsize(i)*naxes(i)
10      continue

        if (naxis .eq. 1 .and. naxes(1) .eq. 1)then
C               This is not a vector column, so read all the rows at once
                nelem=(rstp-rstr)/rinc+1
                ninc=rinc
                rstp=rstr
        else
C               have to read each row individually, in all dimensions
                nelem=(stp(1)-str(1))/inc(1)+1
                ninc=incr(1)
        end if

        do 100 row=rstr,rstp,rinc
         do 90 i9=str(9),stp(9),incr(9)
          do 80 i8=str(8),stp(8),incr(8)
           do 70 i7=str(7),stp(7),incr(7)
            do 60 i6=str(6),stp(6),incr(6)
             do 50 i5=str(5),stp(5),incr(5)
              do 40 i4=str(4),stp(4),incr(4)
               do 30 i3=str(3),stp(3),incr(3)
                do 20 i2=str(2),stp(2),incr(2)

        felem=str(1)+(i2-1)*dsize(2)+(i3-1)*dsize(3)+(i4-1)*dsize(4)
     &  +(i5-1)*dsize(5)+(i6-1)*dsize(6)+(i7-1)*dsize(7)
     &  +(i8-1)*dsize(8)+(i9-1)*dsize(9)

        call ftgcld(iunit,numcol,row,felem,nelem,ninc,
     &  nultyp,nulval,array(i1),flgval(i1),anyf,status)
        if (status .gt. 0)return
        if (anyf)anynul=.true.
        i1=i1+nelem

20              continue
30             continue
40            continue
50           continue
60          continue
70         continue
80        continue
90       continue
100     continue
        end
