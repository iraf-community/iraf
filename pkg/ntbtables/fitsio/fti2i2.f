C----------------------------------------------------------------------
        subroutine fti2i2(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output,status)

C       copy input i*2 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values
C       status  i  output error status (0 = ok)

        integer*2 input(*),output(*),chkval,setval,j,mini2,maxi2
        integer n,i,chktyp,status
        double precision scale,zero,dval,i2max,i2min
        logical tofits,flgray(*),anynul,noscal

        parameter (maxi2=32767)
        parameter (mini2=-32768)
        parameter (i2max=3.276749D+04)
        parameter (i2min=-3.276849D+04)

        if (status .gt. 0)return

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if

        if (tofits)then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
C                       Have to use internal variable j to work around
C                       a bug in the Microsoft v5.0 compiler on IBM PCs
                                j=input(i)
                                output(i)=j
10                      continue
                else
                        do 20 i=1,n
                            dval=(input(i)-zero)/scale
C                           trap any values that overflow the I*2 range
                            if (dval.lt.i2max .and. dval.gt.i2min)then      
                                output(i)=nint(dval)
                            else if (dval .ge. i2max)then
                                status=-11
                                output(i)=maxi2
                            else
                                status=-11
                                output(i)=mini2
                            end if
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
C                               Have to use internal variable j to work around
C                               a bug in the Microsoft v5.0 compiler on IBM PCs
                                        j=input(i)
                                        output(i)=j
30                              continue
                        else
                            do 40 i=1,n
                              dval=input(i)*scale+zero
C                             trap any values that overflow the I*2 range
                              if (dval.lt.i2max .and. dval.gt.i2min)then
                                  output(i)=dval
                              else if (dval .ge. i2max)then
                                  status=-11
                                  output(i)=maxi2
                              else
                                  status=-11
                                  output(i)=mini2
                              end if
40                          continue
                        end if
                else 
C                   must test for null values
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
C                               Have to use internal variable j to work around
C                               a bug in the Microsoft v5.0 compiler on IBM PCs
                                                j=input(i)
                                                output(i)=j
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
                              dval=input(i)*scale+zero
C                             trap any values that overflow the I*2 range
                              if (dval.lt.i2max .and. dval.gt.i2min)then
                                  output(i)=dval
                              else if (dval .ge. i2max)then
                                  status=-11
                                  output(i)=maxi2
                              else
                                  status=-11
                                  output(i)=mini2
                              end if
                            end if
60                      continue
                    end if
                end if
        end if
        end
