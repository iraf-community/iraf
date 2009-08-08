C----------------------------------------------------------------------
        subroutine fti4r4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output,status)

C       copy input i*4 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   i  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i  value in the input array that is used to indicated nulls
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  r  returned array of values

        integer input(*),chkval
        real output(*),setval
        integer n,i,chktyp,status
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (status .gt. 0)return

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
