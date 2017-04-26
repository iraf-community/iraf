C------------------------------------------------------------------------------
        subroutine ftgics(iunit,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,
     &                   type,status)

C       read the values of the celestial coordinate system keywords.
C       These values may be used as input to the subroutines that
C       calculate celestial coordinates. (FTXYPX, FTWLDP)

C       This routine assumes that the CHDU contains an image
C       with the RA type coordinate running along the first axis
C       and the DEC type coordinate running along the 2nd axis.

        double precision xrval,yrval,xrpix,yrpix,xinc,yinc,rot
        integer iunit,status,tstat
        character*(*) type
        character comm*20,ctype*8

        if (status .gt. 0)return

        call ftgkyd(iunit,'CRVAL1',xrval,comm,status)
        call ftgkyd(iunit,'CRVAL2',yrval,comm,status)

        call ftgkyd(iunit,'CRPIX1',xrpix,comm,status)
        call ftgkyd(iunit,'CRPIX2',yrpix,comm,status)

        call ftgkyd(iunit,'CDELT1',xinc,comm,status)
        call ftgkyd(iunit,'CDELT2',yinc,comm,status)

        call ftgkys(iunit,'CTYPE1',ctype,comm,status)

        if (status .gt. 0)then
            call ftpmsg('FTGICS could not find all the required'//
     &                  'celestial coordinate Keywords.')
            status=505
            return
        end if

        type=ctype(5:8)

        tstat=status
        call ftgkyd(iunit,'CROTA2',rot,comm,status)
        if (status .gt. 0)then
C           CROTA2 is assumed to = 0 if keyword is not present
            status=tstat
            rot=0.
        end if
        end
