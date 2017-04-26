C--------------------------------------------------------------------------
        subroutine ftprsv(keyrec,lenval,status)

C       find the total length of the keyword+value string in a keyword record

C       keyrec  c  80 column header record
C       OUTPUT PARAMETERS:
C       lenval  i  output length of keyword+value string
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*80 keyrec
        integer lenval,status,j,c1

        if (status .gt. 0)return

        if (keyrec(1:8) .eq.'COMMENT ' .or. keyrec(1:8).eq.'HISTORY '
     &  .or. keyrec(1:8).eq.'END     ' .or. keyrec(1:8).eq.'        ')
     &  then
C           this is a COMMENT or HISTORY record, with no value
             lenval=8
        else if (keyrec(9:10) .eq. '= ')then
C           this keyword has a value field; now find the first character:
            do 10 j=10,80
                if (keyrec(j:j) .ne. ' ')then
                        c1=j
                        go to 15
                end if
10          continue
C           error: value is blank
            status=204
            call ftpmsg('The keyword '//keyrec(1:8)//
     &      ' has no value string after the equal sign:')
            call ftpmsg(keyrec)
            return

15          if (keyrec(c1:c1) .eq. '''')then
C               This is a string value.
C               Work forward to find a single quote.  Two single quotes
C               in succession is to be interpreted as a literal single
C               quote character as part of the character string, not as
C               the end of the character string.  Everything to the right 
C               of the closing quote is assumed to be the comment.
                do 20 j=c1+1,80
                    if (keyrec(j:j) .eq. '''')then
                        if (j.lt.80 .and. keyrec(j+1:j+1).eq.'''')then
C                               found 2 successive quote characters; this is 
C                               interpreted as a literal quote character
                        else
                                lenval=max(30,j)
                                go to 30
                        end if
                    end if
20              continue
C               error: no closing quote character
                status=205
            call ftpmsg('The following Keyword value string has '//
     &            'no closing quote:')
            call ftpmsg(keyrec)
                return
            else
C               This is either an integer, floating point, or logical value.
C               Extract the first token as the value; remainder = comment
                do 25 j=c1,80
                    if (keyrec(j:j) .eq. ' ')then
                        lenval=j-1
                        go to 30
                    end if
25              continue
C               the first token went all the way to column 80:
                lenval=80
            end if
        else
C               illegal keyword record format; must have '= ' in columns 9-10
C                status=210
C            Modified July 1993:  this is actually not an error.  The
C            keyword should simply be interpreted as a comment.
             lenval=8
        end if
30      continue
        end
