C--------------------------------------------------------------------------
        subroutine ftnulc(input,np,chktyp,setval,flgray,anynul,
     &                    scaled,scale,zero)

C       check input complex array for nulls and apply scaling
C       if chktyp=1 then set the undefined pixel = SETVAL
C       if chktyp=2 then set the corresponding FLGRAY = .true.

C       When scaling complex data values,  both the real and imaginary
C       components of the value are scaled by SCALE, but the offset
C       given by ZERO is only applied to the real part of the complex number

C       input   r  input array of values
C       np      i  number of pairs of values 
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       scaled  l  does data need to be scaled?
C       scale   d  scale factor
C       zero    d  offset

        real input(*),setval(2)
        integer np,i,chktyp,j
        double precision scale,zero
        logical flgray(*),anynul,scaled
        logical fttrnn
        external fttrnn

        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,np
                        flgray(i)=.false.
5               continue
        end if

        j=1
        do 10 i=1,np
C               do the real part of the complex number
                if (chktyp .ne. 0 .and. fttrnn(input(j)))then
                    anynul=.true.
                    if (chktyp .eq. 1)then
C                               set both parts of the complex number to the
C                               specified special value
                                input(j)=setval(1)
                                input(j+1)=setval(2)
                    else
C                               set the corresponding flag value to true
                                flgray(i)=.true.
                    end if
                    j=j+2
                else if (scaled)then
                    input(j)=input(j)*scale+zero
                    j=j+1

C                   do the imaginary part of the complex number
                    if (chktyp .ne. 0 .and. fttrnn(input(j)))then
                            anynul=.true.
                            if (chktyp .eq. 1)then
C                               set both parts of the complex number to the
C                               specified special value
                                input(j-1)=setval(1)
                                input(j)=setval(2)
                            else
C                               set the corresponding flag value to true
                                flgray(i)=.true.
                            end if
                    else if (scaled)then
                        input(j)=input(j)*scale
                    end if
                    j=j+1
                else
                    j=j+2
                end if
10      continue
        end
