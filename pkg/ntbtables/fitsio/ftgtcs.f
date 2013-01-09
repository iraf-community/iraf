C------------------------------------------------------------------------------
        subroutine ftgtcs(iunit,xcol,ycol,xrval,yrval,xrpix,yrpix,
     &                   xinc,yinc,rot,type,status)

C       read the values of the celestial coordinate system keywords
C       from a FITS table where the X and Y or RA and DEC coordinates
C       are stored in separate column.  
C       
C       These values may be used as input to the subroutines that
C       calculate celestial coordinates. (FTXYPX, FTWLDP)

C       xcol (integer) number of the column containing the RA type coordinate
C       ycol (integer) number of the column containing the DEC type coordinate

        double precision xrval,yrval,xrpix,yrpix,xinc,yinc,rot
        integer iunit,xcol,ycol,status
        character*(*) type
        character comm*20,ctype*8,keynam*8,xnum*3,ynum*3

        if (status .gt. 0)return

        call ftkeyn('TCRVL',xcol,keynam,status)
        xnum=keynam(6:8)
        call ftgkyd(iunit,keynam,xrval,comm,status)

        call ftkeyn('TCRVL',ycol,keynam,status)
        ynum=keynam(6:8)
        call ftgkyd(iunit,keynam,yrval,comm,status)

        keynam='TCRPX'//xnum
        call ftgkyd(iunit,keynam,xrpix,comm,status)
        keynam='TCRPX'//ynum
        call ftgkyd(iunit,keynam,yrpix,comm,status)

        keynam='TCDLT'//xnum
        call ftgkyd(iunit,keynam,xinc,comm,status)
        keynam='TCDLT'//ynum
        call ftgkyd(iunit,keynam,yinc,comm,status)

        keynam='TCTYP'//xnum
        call ftgkys(iunit,keynam,ctype,comm,status)

        if (status .gt. 0)then
            call ftpmsg('FTGTCS could not find all the required'//
     &                  ' celestial coordinate Keywords.')
            status=505
            return
        end if

        type=ctype(5:8)

        rot=0.
        end
