C--------------------------------------------------------------------------
        subroutine ftpkls(ounit,keywrd,strval,comm,status)

C       write a character string value to a header record, supporting
C       the OGIP long string convention.  If the keyword string value
C       is longer than 68 characters (which is the maximum that will fit
C       on a single 80 character keyword record) then the value string will
C       be continued over multiple keywords.  This OGIP convention uses the
C       '&' character at the end of a string to indicate that it is continued
C       on the next keyword.  The name of all the continued keywords is
C       'CONTINUE'.
C
C       The FTPLSW subroutine should be called prior to using this
C       subroutine, to write a warning message in the header
C       describing how the convention works. 

C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Sept 1994

        character*(*) keywrd,comm,strval
        integer ounit,status,lenval,ncomm,nvalue
        integer clen,i,strlen,nseg,c1,c2
        character value*70,keynam*10,cmnt*48
        
        if (status .gt. 0)return

        keynam=keywrd
        keynam(9:10)='= '
        cmnt=comm

C       find the number of characters in the input string
        clen=len(strval)
        do 10 i=clen,1,-1
                if (strval(i:i) .ne. ' ')then
                        strlen=i
                        go to 20
                end if
10      continue
        strlen=1

C       calculate the number of keywords needed to write the whole string
20      nseg=max(1,(strlen-2)/67+1)
        
        c1=1
        do 30 i=1,nseg
                c2=min(c1+67,strlen)
C               convert string to quoted character string 

C        fts2c was modified on 29 Nov 1994, so this code is no longer needed
C                (remember to declare character*70 ctemp if this code is used)
C                if (i .gt. 1 .and. strval(c1:c1) .eq. ' ')then
CC                   have to preserve leading blanks on continuation cards
C                    ctemp='A'//strval(c1+1:c2)
C                    call fts2c(ctemp,value,lenval,status)
CC                   now reset the first character of the string back to a blank
C                    value(2:2)=' '
C                else

                    call fts2c(strval(c1:c2),value,lenval,status)

C                end if

                if (i .ne. nseg .and. lenval .ne. 70)then
C                       if the string is continued, preserve trailing blanks
                        value(lenval:69)=' '
                        value(70:70)=''''
                        lenval=70
                end if

C               overwrite last character with a '&' if string is continued.
                if (i .lt. nseg)then
                        value(69:69)='&'
                end if

C               find amount of space left for comment string (assume
C               10 char. for 'keyword = ', and 3 between value and comment) 
C               which leaves 67 spaces for the value + comment strings

                nvalue=max(20,lenval)
                ncomm=67-nvalue

C               write the keyword record
                if (ncomm .gt. 0)then
C                       there is space for a comment
                        call ftprec(ounit,keynam//
     &                  value(1:nvalue)//' / '//cmnt(1:ncomm),status)
                else
C                       no room for a comment
                        call ftprec(ounit,keynam//
     &                  value(1:nvalue)//'   ',status)
                end if

C               initialize for the next segment of the string, if any
                c1=c1+67
                keynam='CONTINUE  '
30      continue
        end
