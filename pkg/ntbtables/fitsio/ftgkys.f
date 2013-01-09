C--------------------------------------------------------------------------
        subroutine ftgkys(iunit,keywrd,strval,comm,status)

C       read a character string value and comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       strval  c  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C       modified 6/93 to support long strings which are continued
C       over several keywords.  A string may be continued by putting
C       a backslash as the last non-blank character in the keyword string,
C       then continuing the string in the next keyword which must have
C       a blank keyword name. 
C       Modified 9/94 to support the new OGIP continuation convention

        character*(*) keywrd,comm,strval
        integer status,iunit
        character value*70, comm2*70, bslash*1
        integer clen,i,bspos,lenval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to unquoted string
        call ftc2s(value,strval,status)

        if (status .gt. 0)return

        clen=len(strval)

C       is last character a backslash or & ?
C       have to use 2 \\'s because the SUN compiler treats 1 \ as an escape
        bslash='\\'
        do 10 i=70,1,-1
                if (value(i:i) .ne. ' ' .and. value(i:i).ne.'''')then
                        if (value(i:i) .eq. bslash .or. 
     &                      value(i:i) .eq. '&')then
C                               have to subtract 1 due to the leading quote char
                                bspos=i-1
                                go to 20
                        end if
C                       no continuation character, so just return
                        return
                end if
10      continue
C       value field was blank, so just return
        return

C       try to get the string continuation, and new comment string
20      call ftgnst(iunit,value,lenval,comm2,status)
        if (lenval .eq. 0)return

        if (bspos .le. clen)then
                strval(bspos:)=value(1:lenval)
                bspos=bspos+lenval-1
        end if

        if (comm2 .ne. ' ')comm=comm2

C       see if there is another continuation line
        if (value(lenval:lenval) .eq. bslash .or. 
     &      value(lenval:lenval) .eq. '&')go to 20
        end
