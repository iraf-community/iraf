C----------------------------------------------------------------------
        subroutine ftgphx(iunit,maxdim,simple,bitpix,naxis,naxes,pcount
     &               ,gcount,extend,bscale,bzero,blank,nblank,status)

C       get the main primary header keywords which define the array structure
C
C       iunit   i  fortran unit number to use for reading
C       maxdim  i  maximum no. of dimensions to read; dimension of naxes
C       OUTPUT PARAMETERS:
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters (usually 0)
C       gcount  i  number of random groups (usually 1 or 0)
C       extend  l  may extensions be present in the FITS file?
C       bscale  d  scaling factor
C       bzero   d  scaling zero point
C       blank   i  value used to represent undefined pixels
C       nblank  i  number of trailing blank keywords immediately before the END
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxdim,bitpix,naxis
        integer naxes(*),pcount,gcount,blank,status,tstat
        logical simple,extend,unknow
        character keynam*8,value*20,lngval*40,comm*72,extn*4,keybuf*80
        double precision bscale,bzero
        integer nkey,nblank,i,ibuff,taxes,maxd

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check that the first keyword is valid
        call ftgrec(iunit,1,keybuf,status)

        keynam=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)

        if (status .gt. 0)go to 900

        simple=.true.
        unknow=.false.
        if (chdu(ibuff) .eq. 1)then
            if (keynam .eq. 'SIMPLE')then
                if (value .eq. 'F')then
C                       this is not a simple FITS file; try to process it anyway
                        simple=.false.
                else if (value .ne. 'T')then
C                       illegal value for the SIMPLE keyword
                        status=220

         if (keybuf(9:10) .ne. '= ')then 
           call ftpmsg('The SIMPLE keyword is missing "= " in '//
     &     'columns 9-10.')
         else
           call ftpmsg('The SIMPLE keyword value is illegal:'//value
     &     // '.  It must equal T or F:')
         end if

                        call ftpmsg(keybuf)
                end if
            else
                status=221
        call ftpmsg('First keyword of the file is not SIMPLE: '//keynam)
                call ftpmsg(keybuf)
                go to 900
            end if
        else
             if (keynam .eq. 'XTENSION')then
                if (value(2:9) .ne. 'IMAGE   ' .and. 
     &              value(2:9) .ne. 'IUEIMAGE')then
C                    I don't know what type of extension this is, but press on
                     unknow=.true.

         if (keybuf(9:10) .ne. '= ')then 
           call ftpmsg('The XTENSION keyword is missing "= " in '//
     &     'columns 9-10.')
         else
           call ftpmsg('This is not an IMAGE extension: '//value)
         end if

                     call ftpmsg(keybuf)
                 end if
             else
                 status=225
                 write(extn,1000)chdu(ibuff)
1000             format(i4)
                 call ftpmsg('First keyword in extension '//extn//
     &           ' was not XTENSION: '//keynam)
                 call ftpmsg(keybuf)
             end if
        end if
        if (status .gt. 0)go to 900

C       check that BITPIX is the second keyword
        call ftgrec(iunit,2,keybuf,status)

        keynam=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)

        if (status .gt. 0)go to 900
        if (keynam .ne. 'BITPIX')then
                status=222
        call ftpmsg('Second keyword was not BITPIX: '//keynam)
                call ftpmsg(keybuf)
                go to 900
        end if
C       convert character string to integer
        call ftc2ii(value,bitpix,status)
        if (status .gt. 0)then
C         bitpix value must be an integer
          if (keybuf(9:10) .ne. '= ')then
             call ftpmsg('BITPIX keyword is missing "= "'//
     &      ' in columns 9-10.')
          else
              call ftpmsg('Value of BITPIX is not an integer: '//value)
          end if
          call ftpmsg(keybuf)
          status=211
          go to 900
        end if

C       test that bitpix has a legal value
        call fttbit(bitpix,status)
        if (status .gt. 0)then
                call ftpmsg(keybuf)
                go to 900
        end if

C       check that the third keyword is NAXIS
        call ftgtkn(iunit,3,'NAXIS',naxis,status)
        if (status .eq. 208)then
C               third keyword was not NAXIS
                status=223
        else if (status .eq. 209)then
C               NAXIS value was not an integer
                status=212
        end if
        if (status .gt. 0)go to 900

        if (maxdim .le. 0)then
                maxd=naxis
        else
                maxd=min(maxdim,naxis)
        end if

        do 10 i=1,naxis
C               construct keyword name
                call ftkeyn('NAXIS',i,keynam,status)
C               attempt to read the keyword
                call ftgtkn(iunit,3+i,keynam,taxes,status)
                if (status .gt. 0)then
                        status=224
                        go to 900
                else if (taxes .lt. 0)then
C                       NAXISn keywords must not be negative
                        status=213
                        go to 900
                else if (i .le. maxd)then
                        naxes(i)=taxes
                end if
10      continue

C       now look for other keywords of interest: bscale, bzero, blank, and END
C       and pcount, gcount, and extend
15      bscale=1.
        bzero=0.
        pcount=0
        gcount=1
        extend=.false.
C       choose a special value to represent the absence of a blank value
        blank=123454321

        nkey=3+naxis
18      nblank=0
20      nkey=nkey+1
        tstat=status
        call ftgrec(iunit,nkey,keybuf,status)
        if (status .gt. 0)then
C               first, check for normal end-of-header status, and reset to 0
                if (status .eq. 203)status=tstat
C               if we hit the end of file, then set status = no END card found
                if (status .eq. 107)then
                       status=210
                       call ftpmsg('FITS header has no END keyword!')
                end if
                go to 900
        end if
        keynam=keybuf(1:8)
        comm=keybuf(9:80)

        if (keynam .eq. 'BSCALE')then
C               convert character string to floating pt.
                call ftpsvc(keybuf,lngval,comm,status)
                call ftc2dd(lngval,bscale,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading BSCALE keyword value'//
     &               ' as a Double:'//lngval)
                end if
        else if (keynam .eq. 'BZERO')then
C               convert character string to floating pt.
                call ftpsvc(keybuf,lngval,comm,status)
                call ftc2dd(lngval,bzero,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading BZERO keyword value'//
     &               ' as a Double:'//lngval)
                end if
        else if (keynam .eq. 'BLANK')then
C               convert character string to integer
                call ftpsvc(keybuf,value,comm,status)
                call ftc2ii(value,blank,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading BLANK keyword value'//
     &               ' as an integer:'//value)
                end if
        else if (keynam .eq. 'PCOUNT')then
C               convert character string to integer
                call ftpsvc(keybuf,value,comm,status)
                call ftc2ii(value,pcount,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading PCOUNT keyword value'//
     &               ' as an integer:'//value)
                end if
        else if (keynam .eq. 'GCOUNT')then
C               convert character string to integer
                call ftpsvc(keybuf,value,comm,status)
                call ftc2ii(value,gcount,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading GCOUNT keyword value'//
     &               ' as an integer:'//value)
                end if
        else if (keynam .eq. 'EXTEND')then
C               convert character string to logical
                call ftpsvc(keybuf,value,comm,status)
                call ftc2ll(value,extend,status)
                if (status .gt. 0)then
                     call ftpmsg('Error reading EXTEND keyword value'//
     &               ' as a Logical:'//value)
                 end if
        else if (keynam .eq. ' ' .and. comm .eq. ' ')then
C               need to ignore trailing blank records before the END card
                nblank=nblank+1
                go to 20
        else if (keynam .eq. 'END')then
                go to 900
        end if
        if (status .gt. 0)go to 900
        go to 18

900     continue

        if (status .gt. 0)then
          if (chdu(ibuff) .eq. 1)then
            call ftpmsg('Failed to parse the required keywords in '//
     &       'the Primary Array header ')
          else
            call ftpmsg('Failed to parse the required keywords in '//
     &       'the Image Extension header (FTGPHX).')
          end if

        else if (unknow)then
C           set status if this was an unknown type of extension
            status=233
        end if
        end
