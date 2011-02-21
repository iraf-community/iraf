C----------------------------------------------------------------------
        subroutine ftc2s(in,cval,status)
C       convert an input quoted string to an unquoted string
C
C       The first character of the input string must be a quote character (')
C       and at least one additional quote character must also be present in the 
C       input string. This routine then simply outputs all the characters 
C       between the first and last quote characters in the input string.
C
C       in      c  input quoted string
C       cval    c  output unquoted string
C       status  i  output error status (0=ok, 1=first quote missing,
C                  2=second quote character missing.

        character*(*) in,cval
        integer length,i,j,i2,status
        character*1 dtype

C       test for datatype
        call ftdtyp(in,dtype,status)
        if (status .gt. 0)return
        if (dtype .ne. 'C')then
C               do no conversion and just return the raw character string
                cval=in
        else
C               convert character string to unquoted string

C               find closing quote character
                length=len(in)
                i2=length-1
                do 10 i=length,2,-1
                        if (in(i:i) .eq. '''')go to 20
                        i2=i2-1
10              continue
20              continue

                if (i2 .eq. 0)then
C                       there was no closing quote character
                        status=205
            call ftpmsg('The following keyword value string has no '
     &      //'closing quote:')
            call ftpmsg(in)
                else if (i2 .eq. 1)then
C                       null string
                        cval=' '
                else
                        cval=in(2:i2)

C                       test for double single quote characters; if found,
C                       then  delete one of the quotes (FITS uses 2 single
C                       quote characters to represent a single quote)
                        i2=i2-2
                        do 30  i=1,i2
                            if (cval(i:i) .eq. '''')then
                                if (cval(i+1:i+1) .eq. '''')then
                                   do 40 j=i+1,i2
                                         cval(j:j)=cval(j+1:j+1)
40                                 continue
                                   cval(i2:i2)=' '
                                end if
                            end if
30                      continue
                end if
        end if
        end
