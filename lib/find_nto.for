C
C-------------------------------------------------------FIND_NTO---
C
      SUBROUTINE FIND_NTO(NTM,AVGTYPE,NTO)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT      : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine finds NTO, the number of AVGTYPE averages that 
C                    can be made on NTMAX OUTPUT_DATES
C
C --- INPUT        : NTMAX
C                    AVGTYPE
C
C --- OUTPUT       : NTO
C
C --- CALLS        : none   
C
C --- VERSION: 2010-02-11
C              2019-01-01 CHO - Added MD statistics
C
C-------------------------------------------------------FIND_NTO---
C
      IMPLICIT NONE
C
      INTEGER NTM, NTO, ICURR, IVAL, NT
      INTEGER ICURRMD(NTM),N
      INTEGER OUTPUT_DATES_24(5)
      LOGICAL LMATCHMD
      CHARACTER*(*) AVGTYPE

      INTEGER IQUARTER(12)
      DATA IQUARTER /1,1,1,2,2,2,3,3,3,4,4,4/

C --- Find first date, based on AVGTYPE
      CALL FIX24(1,OUTPUT_DATES_24)
      IF (AVGTYPE .EQ. 'D') THEN
          ICURR = OUTPUT_DATES_24(3)
      ELSEIF (AVGTYPE .EQ. 'M') THEN
          ICURR = OUTPUT_DATES_24(2)
      ELSEIF (AVGTYPE .EQ. 'Q') THEN
          ICURR = IQUARTER(OUTPUT_DATES_24(2))
      ELSEIF (AVGTYPE .EQ. 'MD') THEN
          ICURRMD(1) = (OUTPUT_DATES_24(2) * 100) +
     &                 OUTPUT_DATES_24(4)      
      ELSE
          STOP 'Unimplemented AVGTYPE'
      ENDIF
      
      NT = 0
      NTO = 1
 
      DO WHILE (NT .LT. NTM)
 
         NT = NT + 1
      
         CALL FIX24(NT,OUTPUT_DATES_24)
         IF (AVGTYPE .EQ. 'D') THEN
             IVAL = OUTPUT_DATES_24(3)
         ELSEIF (AVGTYPE .EQ. 'M') THEN
             IVAL = OUTPUT_DATES_24(2)
         ELSEIF (AVGTYPE .EQ. 'Q') THEN
             IVAL = IQUARTER(OUTPUT_DATES_24(2))
         ELSEIF (AVGTYPE .EQ. 'MD') THEN
             IVAL = (OUTPUT_DATES_24(2)* 100) +
     &               OUTPUT_DATES_24(4)
         ENDIF
       
         IF (AVGTYPE .NE. 'MD') THEN
     
             IF (IVAL .NE. ICURR) THEN
                 NTO = NTO + 1
                 ICURR = IVAL
             ENDIF
     
         ELSE
      
             LMATCHMD = .FALSE.
             DO N = 1, NTO
     
                IF (IVAL .EQ. ICURRMD(N)) LMATCHMD = .TRUE.
      
             ENDDO
      
             IF (.NOT. LMATCHMD) THEN
                 NTO = NTO + 1
                 ICURRMD(NTO) = IVAL
             ENDIF
            
         ENDIF  

      ENDDO

      RETURN

      END
