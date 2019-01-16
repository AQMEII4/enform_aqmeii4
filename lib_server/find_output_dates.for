C
C----------------------------------------------FIND_OUTPUT_DATES---
C
      SUBROUTINE FIND_OUTPUT_DATES(OUTPUT_DATES,
     &           AVGTYPE, OUTPUT_DATES_WORK, NTMAX)
C
C     (C) Copyright ENSEMBLE Consortium
C
C --- PURPOSE: This routine finds the array of output dates for a given
C              averaging, starting date and number of outputs
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   OUTPUT_DATES
C              NTMAX
C              AVGTYPE
C
C --- OUTPUT:  OUTPUT_DATES_WORK
C
C --- CALLS:   
C
C --- VERSION: 2010-04-12
C
C----------------------------------------------FIND_OUTPUT_DATES---
C
      IMPLICIT NONE
C
      INTEGER NTO, ICURR, IVAL, NT, I, NTMAX
      INTEGER OUTPUT_DATES(5,NTMAX)
      INTEGER OUTPUT_DATES_TMP(5)
      INTEGER OUTPUT_DATES_WORK(5,NTMAX)
      CHARACTER*(*) AVGTYPE
      LOGICAL SEEN
      INTEGER IQUARTER(12)
      DATA IQUARTER /1,1,1,2,2,2,3,3,3,4,4,4/

      LOGICAL DBG
      DBG = .FALSE.

C --- Find first date, based on AVGTYPE
      IF (AVGTYPE .EQ. 'D') THEN
          ICURR = OUTPUT_DATES(3,1)
      ELSEIF (AVGTYPE .EQ. 'M') THEN
          ICURR = OUTPUT_DATES(2,1)
      ELSEIF (AVGTYPE .EQ. 'Q') THEN
          ICURR = IQUARTER(OUTPUT_DATES(2,1))
      ELSE
          STOP 'Unimplemented AVGTYPE'
      ENDIF

      NT = 0
      NTO = 0
      SEEN = .FALSE.
    
      IF (DBG) THEN
          WRITE(6,*) 'find_output_dates - AVGTYPE ',AVGTYPE
          WRITE(6,*) 'find_output_dates - NTMAX ',NTMAX
      ENDIF




      DO WHILE (NT .LT. NTMAX)
 
         NT = NT + 1
      
         IF (AVGTYPE .EQ. 'D') THEN
             IVAL = OUTPUT_DATES(3,NT)
         ELSEIF (AVGTYPE .EQ. 'M') THEN
             IVAL = OUTPUT_DATES(2,NT)
         ELSEIF (AVGTYPE .EQ. 'Q') THEN
             IVAL = IQUARTER(OUTPUT_DATES(2,NT))
         ENDIF
         IF (IVAL .NE. ICURR) THEN
             NTO = NTO + 1
             DO I = 1, 5
               OUTPUT_DATES_WORK(I,NTO) = OUTPUT_DATES_TMP(I)
             ENDDO
             ICURR = IVAL
             SEEN = .FALSE.
         ELSE
             IF (.NOT.SEEN) THEN
                 DO I = 1, 5
                    OUTPUT_DATES_TMP(I) = OUTPUT_DATES(I,NT)
                 ENDDO
                 SEEN = .TRUE.
             ENDIF
         ENDIF
         IF (DBG) THEN
             WRITE(6,*) 'find_output_dates - NT    --> ',NT
             WRITE(6,*) 'find_output_dates - NTO   --> ',NTO
             WRITE(6,*) 'find_output_dates - IVAL  --> ',IVAL
             WRITE(6,*) 'find_output_dates - ICURR --> ',ICURR
         ENDIF

      ENDDO

      RETURN

      END
