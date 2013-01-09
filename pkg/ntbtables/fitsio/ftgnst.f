C--------------------------------------------------------------------------
        subroutine ftgnst(iunit,value,lenval,comm,status)

C       get the next string keyword.
C       see if the next keyword in the header is the continuation
C       of a long string keyword, and if so, return the value string,
C       the number of characters in the string, and the associated comment
C       string.

C       value  c  returned value of the string continuation
C       lenval i  number of non-blank characters in the continuation string
C       comm   C  value of the comment string, if any, in this keyword.

        character*(*) value,comm
        integer iunit,lenval,status

        integer i,length,tstat,nkeys,nextky
        character record*80, strval*70

        if (status .gt. 0)return

        tstat=status
        value=' '
        comm=' '
        lenval=0

C       get current header position
        call ftghps(iunit,nkeys,nextky,status)

C       get the next keyword record
        if (nextky .le. nkeys)then
            call ftgrec(iunit,nextky,record,status)
        else
C           positioned at end of header, so there is no next keyword to read
            return
        end if

C       does this appear to be a continuation keyword (=blank keyword name
C       or CONTINUE)?
        if (record(1:10) .ne. ' ' .and. record(1:10) .ne. 
     &     'CONTINUE  ')return

C       return if record is blank
        if (record .eq. ' ')return

C       set a dummy keyword name
        record(1:10)='DUMMYKEY= '

C       parse the record to get the value string and comment
        call ftpsvc(record,strval,comm,status)

C       convert character string to unquoted string
        call ftc2s(strval,value,status)
        if (status .gt. 0)then
C               this must not be a continuation card; reset status and messages
                status=tstat
                call ftcmsg
                value=' '
                comm=' '
                return
        end if

        length=len(value)
        do 10 i=length,1,-1
                if (value(i:i) .ne. ' ')then
                        lenval=i
                        return
                end if
10      continue
        end
