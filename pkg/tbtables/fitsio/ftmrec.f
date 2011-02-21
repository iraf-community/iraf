C--------------------------------------------------------------------------
        subroutine ftmrec(ounit,nkey,record,status)

C       modify the nth keyword in the CHU, by replacing it with the
C       input 80 character string.
C
C       ounit   i  fortran output unit number
C       nkey    i  sequence number (starting with 1) of the keyword to read
C       record  c  80-character string to replace the record with
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nkey,status
        character*(*) record
        character rec*80

C       find the old keyword; just use REC as a temporary variable
        call ftgrec(ounit,nkey,rec,status)
        
        rec=record
C       overwrite the keyword with the new record
        call ftmodr(ounit,rec,status)
        end
