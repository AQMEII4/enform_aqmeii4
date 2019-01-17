C
C-----------------------------------------GENERATE_OUTPUT_DATES_MD---
C
      SUBROUTINE GENERATE_OUTPUT_DATES_MD(OUTPUT_DATES,
     &           OUTPUT_DATES_WORK, NTMAX)
C
C     (C) Copyright ENSEMBLE Consortium
C
C --- PURPOSE: This routine finds the array of output dates for MD
C              averaging, form starting date and number of outputs
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   OUTPUT_DATES
C              NTMAX
C
C --- OUTPUT:  OUTPUT_DATES_WORK
C
C --- CALLS:   
C
C --- VERSION: 2019-01-17
C
C-----------------------------------------GENERATE_OUTPUT_DATES_MD---
C
      IMPLICIT NONE
C
      INTEGER IYBEG, IYEND, IMBEG
      INTEGER IY, IM, ID, IH, NT, NTMAX
      INTEGER OUTPUT_DATES(5,NTMAX)
      INTEGER OUTPUT_DATES_24(5)
      INTEGER OUTPUT_DATES_WORK(5,NTMAX)

      LOGICAL DBG
      DBG = .TRUE.

      NT = 1

C --- Find year iand month of first date
      CALL FIX24(1,OUTPUT_DATES_24)
      IYBEG = OUTPUT_DATES_24(1)
      IMBEG = OUTPUT_DATES(2,1)

C --- Find year iand month of last date
      CALL FIX24(NTMAX,OUTPUT_DATES_24)
      IYEND = OUTPUT_DATES_24(1)

      DO IY = IYBEG, IYEND
 
         DO IM = 1, 12

            IF (IY.EQ.IYBEG .AND. IM.LT.IMBEG) THEN
            ELSE
               DO IH = 1, 24
                  ! generate date iy,im,01,ih
                  OUTPUT_DATES_WORK(1,NT) = IY
                  OUTPUT_DATES_WORK(2,NT) = IM
                  OUTPUT_DATES_WORK(3,NT) = 1
                  OUTPUT_DATES_WORK(4,NT) = IH

                  IF (DBG) THEN
             WRITE(6,*) 'generate_output_dates_md - NT    --> ',NT
             WRITE(6,*) 'generate_output_dates_md - DATE  --> ',
     &                  OUTPUT_DATES_WORK(1,NT),OUTPUT_DATES_WORK(2,NT),
     &                  OUTPUT_DATES_WORK(3,NT),OUTPUT_DATES_WORK(4,NT)
                  ENDIF

                  NT = NT + 1
                  IF (NT .GT. NTMAX) CYCLE

               ENDDO
            ENDIF

         ENDDO

      ENDDO
         
      RETURN

      END
