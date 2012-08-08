C----------------------------------------------------------------------
        subroutine ftbnfm(form,dtype,rcount,width,status)

C       'Binary Format'
C       parse the binary table column format to determine the data
C       type and the repeat count (and string width, if it is an ASCII field)
C
C       form    c  format string
C       OUTPUT PARAMETERS:
C       dattyp  i  datatype code
C       rcount  i  repeat count
C       width   i  if ASCII field, this is the width of the unit string
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) form
        integer dtype,rcount,width,status,tstat
        character dattyp*1,cform*16
        integer point,nc,c1,i,nw

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

C       find the size of the field repeat count, if present
        nw=0
        do 20 i=c1,nc
                if (form(i:i) .ge. '0' .and. form(i:i) .le. '9')then
                        nw=nw+1
                else
                        go to 30
                end if
20      continue
30      continue
        if (nw .eq. 0)then
C               no explicit repeat count, so assume a value of 1
                rcount=1
        else
                call ftc2ii(form(c1:c1+nw-1),rcount,status)
                if (status .gt. 0)then
                   call ftpmsg('Error in FTBNFM evaluating TFORM'
     &             //' repeat value: '//cform)
                   return
                 end if
        end if

        c1=c1+nw

C       see if this is a variable length pointer column (e.g., 'rPt'); if so,
C       then add 1 to the starting search position in the TFORM string
        if (form(c1:c1) .eq. 'P')then
                point=-1
                c1=c1+1
                rcount=1
        else
                point=1
        end if

C       now the chararcter at position c1 should be the data type code
        dattyp=form(c1:c1)

C       set the numeric datatype code
        if (dattyp .eq. 'I')then
                dtype=21
        else if (dattyp .eq. 'J')then
                dtype=41
        else if (dattyp .eq. 'E')then
                dtype=42
        else if (dattyp .eq. 'D')then
                dtype=82
        else if (dattyp .eq. 'A')then
                dtype=16
        else if (dattyp .eq. 'L')then
                dtype=14
        else if (dattyp .eq. 'X')then
                dtype=1
        else if (dattyp .eq. 'B')then
                dtype=11
        else if (dattyp .eq. 'C')then
                dtype=83
        else if (dattyp .eq. 'M')then
                dtype=163
        else
C               unknown tform datatype code
                status=262
                call ftpmsg('Unknown Binary table TFORMn keyword '//
     &                      'datatype: '//cform)
                return
        end if
                
C       set dtype negative if this is a variable length field ('P')
        dtype=dtype*point

C       if this is an ASCII field, determine its width
        if (dtype .eq. 16)then
                c1=c1+1
                nw=0
                do 40 i=c1,nc
                    if (form(i:i) .ge. '0' .and. form(i:i).le.'9')then
                        nw=nw+1
                else
                        go to 50
                end if
40              continue
50              continue
                if (nw .eq. 0)then
C                       no explicit width field, so assume that the
C                       width is the same as the repeat count
                        width=rcount
                else
                        tstat=status
                        call ftc2ii(form(c1:c1+nw-1),width,status)
                        if (status .gt. 0)then
C                       unrecognized characters following the 'A', so ignore it
                               width=rcount
                               status=tstat
                        end if
                end if
        end if
        end
