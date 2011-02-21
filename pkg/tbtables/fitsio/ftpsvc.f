C--------------------------------------------------------------------------
        subroutine ftpsvc(keyrec,value,comm,status)

C       parse the header record to find value and comment strings

C       keyrec  c  80 column header record
C       OUTPUT PARAMETERS:
C       value   c  output keyword value string
C       comm    c  output keyword comment string
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*80 keyrec,keytmp
        character*(*) value,comm
        character*80 ctemp
        integer status,j,c1

        if (status .gt. 0)return

        if (keyrec(1:8) .eq.'COMMENT ' .or. keyrec(1:8).eq.'HISTORY '
     &  .or. keyrec(1:8).eq.'END     ' .or. keyrec(1:8).eq.'        ')
     &  then
C           this is a COMMENT or HISTORY record, with no value
            value=' '
            comm=keyrec(9:80)
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
C               First, copy input to temporary string variable
                keytmp=keyrec
                do 20 j=c1+1,80
                    if (keytmp(j:j) .eq. '''')then
                        if (j.lt.80 .and. keytmp(j+1:j+1).eq.'''')then
C                               found 2 successive quote characters; this is 
C                               interpreted as a literal quote character; remove
C                               one of the quotes from the string, and continue
C                               searching for the closing quote character:
                                keytmp(j+1:80)=keytmp(j+2:80)
                        else
                                value=keytmp(c1:j)
                                if (j .lt. 80)then
                                        ctemp=keytmp(j+1:80)
                                else
                                        ctemp=' '
                                end if
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
                        value=keyrec(c1:j-1)
                        ctemp=keyrec(j+1:80)
                        go to 30
                    end if
25              continue
C               the first token went all the way to column 80:
                value=keyrec(c1:80)
                ctemp=' '
            end if

30          comm=' '
C           look for first character in the comment string
            do 40 j=1,78
                if (ctemp(j:j).ne.' ')then
                        if (ctemp(j:j).eq.'/')then
C                            ignore first space, if it exists
                             if (ctemp(j+1:j+1) .eq. ' ')then
                                comm=ctemp(j+2:80)
                             else
                                comm=ctemp(j+1:80)
                             end if
                        else
                                comm=ctemp(j:80)
                        end if
                        go to 50
                end if
40          continue
        else
C           illegal keyword record format; must have '= ' in columns 9-10
C           status=210
C           Modified July 1993:  this is actually not an error.  The
C           keyword should simply be interpreted as a comment.
            value=' '
            comm=keyrec(9:80)
        end if
50      continue
        end
