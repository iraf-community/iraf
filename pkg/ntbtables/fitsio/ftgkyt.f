C--------------------------------------------------------------------------
        subroutine ftgkyt(iunit,keywrd,jval,dval,comm,status)

C       read an integer value and fractional parts of a keyword value
C       and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       jval    i  output integer part of keyword value 
C       dval    d  output fractional part of keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, Sept 1992

        character*(*) keywrd,comm
        integer iunit,jval,status,i,dot
        double precision dval
        character value*35
        logical ed

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       read keyword in straight forward way first:
C       just convert character string to double precision
C       datatype conversion will be performed if necessary and if possible
        call ftc2d(value,dval,status)
        jval=dval
        if (jval .ge. 0)then
                dval=dval-jval
        else
                dval=dval+jval
        end if

C       now see if we have to read the fractional part again, this time
C       with more precision

C       find the decimal point, if any, and look for a D or E
        dot=0
        ed=.false.
        do 10 i=1,35
            if (value(i:i) .eq. '.')dot=i
            if (value(i:i) .eq. 'E' .or. value(i:i) .eq. 'D')ed=.true.
10      continue

        if (.not. ed .and. dot .gt. 0)then
C           convert fractional part to double precision
            call ftc2d(value(dot:),dval,status)
        end if

        end
