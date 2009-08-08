C--------------------------------------------------------------------------
        subroutine ftplsw(ounit,status)

C       Put Long String Warning:
C       write the LONGSTRN keyword and a few COMMENT keywords to the header 
C       (if they don't already exist) to warn users that this FITS file
C       may use the OGIP long string convention.

C       This subroutine should be called whenever FTPKLS is called.

        integer ounit,status,tstat
        character value*8,comm*8

        if (status .gt. 0)return

        tstat=status
        call ftgkys(ounit,'LONGSTRN',value,comm,status)
        if (status .eq. 0)then
C             The keyword already exists so just exit
              return
         end if

         status=tstat
         call ftpkys(ounit,'LONGSTRN','OGIP 1.0',
     &   'The OGIP Long String Convention may be used.',status)

         call ftpcom(ounit,
     & 'This FITS file may contain long string keyword values that are'
     &  ,status)
           call ftpcom(ounit,
     & 'continued over multiple keywords.  The OGIP convention uses the'
     &  //' &',status)
            call ftpcom(ounit,
     & 'character at the end of each substring which is then continued'       
     &  ,status)
            call ftpcom(ounit,
     & 'on the next keyword which has the name CONTINUE.'
     &  ,status)
        end                
