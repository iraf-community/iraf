C--------------------------------------------------------------------------
        subroutine ftgthd(tmplat,card,hdtype,status)

C       'Get Template HeaDer'
C       parse a template header line and create a formated
C       80-character string which is suitable for appending to a FITS header 

C       tmplat  c  input header template string
C       card    c  returned 80-character string = FITS header record
C       hdtype  i  type of operation that should be applied to this keyword:
C                      -2  =  modify the name of a keyword; the new name
C                             is returned in characters 41:48 of CARD.
C                      -1  =  delete this keyword 
C                       0  =  append (if it doesn't already exist) or 
C                             overwrite this keyword (if it does exist)
C                       1  =  append this comment keyword ('HISTORY', 
C                             'COMMENT', or blank keyword name) 
C                       2  =  this is an END record; do not append it
C                             to a FITS header! 
C       status  i  returned error status 
C               if a positive error status is returned then the first 
C               80 characters of the offending input line are returned
C               by the CARD parameter

        integer hdtype,status,tstat
        character*(*) tmplat
        character*80 card
        integer i1,i2,com1,strend,length
        character inline*100,keynam*8,ctemp*80,qc*1
        logical number
        double precision dvalue

        if (status .gt. 0)return
        card=' '
        hdtype=0

        inline=tmplat

C       test if columns 1-8 are blank; if so, this is a FITS comment record;
C       just copy it verbatim to the FITS header
        if (inline(1:8) .eq. ' ')then
                card=inline(1:80)
                go to 999
        end if

C       parse the keyword name = the first token separated by a space or a '='
C       1st locate the first nonblank character (we know it is not all blank):
        i1=0
20      i1=i1+1
C       test for a leading minus sign which flags name of keywords to be deleted
        if (inline(i1:i1) .eq. '-')then
                hdtype=-1
C               test for a blank keyword name
                if (inline(i1+1:i1+8) .eq. '        ')then
                       card=' '
                       i2=i1+9
                       go to 35
                end if
                go to 20
        else if (inline(i1:i1) .eq. ' ')then
                go to 20
        end if

C       now find the last character of the keyword name
        i2=i1
30      i2=i2+1
        if (inline(i2:i2) .ne. ' ' .and. inline(i2:i2) .ne. '=')go to 30

C       test for legal keyword name length (max 8 characters)
        if (i2-i1 .gt. 8)then
                status=207
                card=inline(1:80)
                go to 999
        end if

        keynam=inline(i1:i2-1)

C       convert to upper case and test for illegal characters in keyword name
        call ftupch(keynam)
        call fttkey(keynam,status)
        if (status .gt. 0)then
                card=inline(1:80)
                go to 999
        end if

C       if this is the 'END' then this is the end of the input file
        if (keynam .eq. 'END     ')goto 998

C       copy the keyword name to the output record string
        card(1:8)=keynam

C       jump if this is just the name of keyword to be deleted
        if (hdtype .lt. 0)go to 35

C       test if this is a COMMENT or HISTORY record
        if (keynam .eq. 'COMMENT' .or. keynam .eq. 'HISTORY')then
C               append next 72 characters from input line to output record
                card(9:80)=inline(i2:)
                hdtype=1
                go to 999
        else
C               this keyword must have a value, so append the '= ' to output
                card(9:10)='= '
        end if

C       now locate the value token in the input line.  If it includes
C       embedded spaces it must be enclosed in single quotes. The value must
C       be separated by at least one blank space from the comment string

C       find the first character of the value string
35      i1=i2-1
40      i1=i1+1
        if (i1 .gt. 100)then
C               no value is present in the input line
                if (hdtype .lt. 0)then
C                       this is normal; just quit
                        go to 999
                else
                        status=204
                        card=inline(1:80)
                        go to 999
                end if
        end if
        if (hdtype .lt. 0 .and. inline(i1:i1) .eq. '=')then
C               The leading minus sign, plus the presence of an equal sign
C               between the first 2 tokens is taken to mean that the
C               keyword with the first token name is to be deleted.
                go to 999
        else if (inline(i1:i1).eq. ' ' .or.inline(i1:i1).eq. '=')then 
                go to 40
        end if

C       is the value a quoted string?
        if (inline(i1:i1) .eq. '''')then
C               find the closing quote 
                i2=i1
50              i2=i2+1
                if (i2 .gt. 100)then
C                       error: no closing quote on value string
                        status=205
                        card=inline(1:80)
            call ftpmsg('Keyword value string has no closing quote:')
            call ftpmsg(card)
                        go to 999
                end if
                if (inline(i2:i2) .eq. '''')then
                        if (inline(i2+1:i2+1) .eq. '''')then
C                               ignore 2 adjacent single quotes
                                i2=i2+1
                                go to 50
                        end if
                else
                        go to 50
                end if
C               value string can't be more than 70 characters long (cols 11-80)
                length=i2-i1
                if (length .gt. 69)then
                        status=205
                        card=inline(1:80)
            call ftpmsg('Keyword value string is too long:')
            call ftpmsg(card)
                        go to 999
                end if

C               append value string to output, left justified in column 11
                card(11:11+length)=inline(i1:i2)
C               com1 is the starting position for the comment string
                com1=max(32,13+length)

C               FITS string must be at least 8 characters long
                if (length .lt. 9)then
                        card(11+length:11+length)=' '
                        card(20:20)=''''
                end if
        else
C               find the end of the value field
                i2=i1
60              i2=i2+1
                if (i2 .gt. 100)then
C                       error: value string is too long
                        status=205
                        card=inline(1:80)
            call ftpmsg('Keyword value string is too long:')
            call ftpmsg(card)
                        go to 999
                end if
                if (inline(i2:i2) .ne. ' ')go to 60

C               test if this is a logical value
                length=i2-i1
                if (length .eq. 1 .and. (inline(i1:i1) .eq. 'T'
     &              .or. inline(i1:i1) .eq. 'F'))then
                        card(30:30)=inline(i1:i1)
                        com1=32
                else
C                   test if this is a numeric value; try reading it as 
C                   double precision value; if it fails, it must be a string
                    number=.true.
                    tstat=status
                    call ftc2dd(inline(i1:i2-1),dvalue,status)
                    if (status .gt. 0)then
                        status=tstat
                        number=.false.
                    else
C                       check the first character to make sure this is a number
C                       since certain non-numeric character strings pass the
C                       above test on SUN machines.
                        qc=inline(i1:i1)
                        if (qc .ne. '+' .and. qc .ne. '-' .and. qc .ne.
     &                  '.' .and. (qc .lt. '0' .or. qc .gt. '9'))then
C                              This really was not a number!
                               number=.false.
                        end if
                    end if

                    if (number)then
                        if (length .le. 20)then
C                               write the value right justified in col 30
                                card(31-length:30)=inline(i1:i2-1)
                                com1=32
                        else
C                               write the long value left justified in col 11
                                card(11:10+length)=inline(i1:i2-1)
                                com1=max(32,12+length)
                        end if
                    else
C                       value is a character string datatype
                        card(11:11)=''''
                        strend=11+length
                        card(12:strend)=inline(i1:i2-1)
C                       need to expand any embedded single quotes into 2 quotes
                        i1=11
70                      i1=i1+1
                        if (i1 .gt. strend) go to 80
                        if (card(i1:i1) .eq. '''')then
                                i1=i1+1
                                if (card(i1:i1) .ne. '''')then
C                                       have to insert a 2nd quote into string
                                        ctemp=card(i1:strend)
                                        card(i1:i1)=''''
                                        strend=strend+1
                                        i1=i1+1
                                        card(i1:strend)=ctemp
                                end if
                        end if
                        go to 70

80                      strend=max(20,strend+1)
                        card(strend:strend)=''''
                        com1=max(32,strend+2)
                    end if
                end if
        end if
        
C       check if this was a request to modify a keyword name
        if (hdtype .eq. -1)then
                hdtype = -2
C               the keyword value is really the new keyword name
C               return the new name in characters 41:48 of the output card
                keynam=card(12:19)
C               convert to upper case and test for illegal characters in name
                call ftupch(keynam)
                call fttkey(keynam,status)
                if (status .gt. 0)then
                        card=inline(1:80)
                        go to 999
                else
                        card(9:80)=' '
                        card(41:48)=keynam
                        go to 999
                end if
        end if

C       is there room for a comment string?
        if (com1 .lt. 79)then
C               now look for the beginning of the comment string
                i1=i2
90              i1=i1+1
C               if no comment field then just quit
                if (i1 .gt. 100)go to 999
                if (inline(i1:i1) .eq. ' ')go to 90

C               append the comment field
                if (inline(i1:i1) .eq. '/')then
                        card(com1:80)=inline(i1:)
                else
                        card(com1:80)='/ '//inline(i1:)
                end if
        end if

        go to 999

C       end of input file was detected
998     hdtype=2

999     continue
        end
