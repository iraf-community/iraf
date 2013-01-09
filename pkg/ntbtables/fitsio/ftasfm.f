C----------------------------------------------------------------------
        subroutine ftasfm(form,dtype,width,decims,status)

C       'ASCII Format'
C       parse the ASCII table TFORM column format to determine the data
C       type, the field width, and number of decimal places (if relevant)
C
C       form    c  TFORM format string
C       OUTPUT PARAMETERS:
C       dattyp  i  datatype code
C       width   i  width of the field
C       decims  i  number of decimal places
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, November 1994

        character*(*) form
        integer dtype,width,decims,status
        character dattyp*1,cform*16
        integer nc,c1,i,nw

        if (status .gt. 0)return

        cform=form

C       find first non-blank character
        nc=len(form)
        do 5 i=1,nc
                if (form(i:i) .ne. ' ')then
                        c1=i
                        go to 10
                end if
5       continue

C       error: TFORM is a blank string
        status=261
        call ftpmsg('The TFORM keyword has a blank value.')
        return

10      continue

C       now the chararcter at position c1 should be the data type code
        dattyp=form(c1:c1)

C       set the numeric datatype code
        if (dattyp .eq. 'I')then
                dtype=41
        else if (dattyp .eq. 'E')then
                dtype=42
        else if (dattyp .eq. 'F')then
                dtype=42
        else if (dattyp .eq. 'D')then
                dtype=82
        else if (dattyp .eq. 'A')then
                dtype=16
        else
C               unknown tform datatype code
                status=262
                call ftpmsg('Unknown ASCII table TFORMn keyword '//
     &                      'datatype: '//cform)
                return
        end if
                
C       determine the field width
        c1=c1+1
        nw=0
        do 40 i=c1,nc
            if (form(i:i) .ge. '0' .and. form(i:i).le.'9')then
                    nw=nw+1
            else
                    go to 50
            end if
40      continue
50      continue
        if (nw .eq. 0)then
C               error, no width specified
                go to 990
        else
                call ftc2ii(form(c1:c1+nw-1),width,status)
                if (status .gt. 0 .or. width .eq. 0)then
C                      unrecognized characters following the type code
                       go to 990
                end if
        end if

C       determine the number of decimal places (if any)
        decims=-1
        c1=c1+nw
        if (form(c1:c1) .eq. '.')then
            c1=c1+1
            nw=0
            do 60 i=c1,nc
                if (form(i:i) .ge. '0' .and. form(i:i).le.'9')then
                    nw=nw+1
                else
                    go to 70
            end if
60          continue
70          continue

            if (nw .eq. 0)then
C               error, no decimals specified
                go to 990
            else
                call ftc2ii(form(c1:c1+nw-1),decims,status)
                if (status .gt. 0)then
C                   unrecognized characters 
                    go to 990
                end if
            end if
        else if (form(c1:c1) .ne. ' ')then
            go to 990
        end if

C       consistency checks
        if (dattyp .eq. 'A' .or. dattyp .eq. 'I')then
            if (decims .eq. -1)then
                decims=0
            else
                go to 990
            end if
        else if (decims .eq. -1)then
C           number of decmal places must be specified for D, E, or F fields
            go to 990
        else if (decims .ge. width)then
C           number of decimals must be less than the width
            go to 990
        end if

        if (dattyp .eq. 'I')then
C           set datatype to SHORT integer if 4 digits or less
            if (width .le. 4)dtype=21
        else if (dattyp .eq. 'F')then
C           set datatype to DOUBLE if 8 digits or more
            if (width .ge. 8)dtype=82
        end if
          
        return

990     continue
        status=261
        call ftpmsg('Illegal ASCII table TFORMn keyword: '//cform)
        end
