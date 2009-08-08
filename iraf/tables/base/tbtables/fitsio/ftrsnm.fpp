C--------------------------------------------------------------------------
        subroutine ftrsnm

C       simply reset the column names as undefined
C       this will force ftgcnn to read the column names from the
C       file the next time it is called

C       written by Wm Pence, HEASARC/GSFC, Feb 1995

        integer colpnt,untpnt
        common/ftname/colpnt,untpnt

        colpnt= -999
        untpnt=0
        end
