C
C-----------------------------------------------------PARSE_AVERAGING---
C
      SUBROUTINE PARSE_AVERAGING(STRING,AVERAGING,STATISTICS,PERCENTILE)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- INPUT        : STRING
C
C --- PURPOSE      : This routine parses an averaging record of the source 
C                    term file for AQ applications
C
C --- AUTHOR       : R.Bianconi - http://www.enviroware.com
C
C --- INPUT        : STRING
C
C --- OUTPUT       : AVERAGING,STATISTICS,PERCENTILE
C
C --- CALLS        : 
C
C --- VERSION      : 2010-01-20
C                  : 20141217 RBI removed soem unused variables
C
C-----------------------------------------------------PARSE_AVERAGING---
C
      IMPLICIT NONE
      CHARACTER*1 SEP
      PARAMETER (SEP = ',')

      CHARACTER*(*) STRING, AVERAGING, STATISTICS
      REAL PERCENTILE
C
      CHARACTER*10 AVG,STS
      INTEGER I, K, IUNIT, COMMA(3), LSTRIML, LSTRIM
      PARAMETER (IUNIT = 44)
C
      PERCENTILE = 0.
      AVERAGING  = ' '
      STATISTICS  = ' '
      PERCENTILE = 0.

      DO K = 1, 3
         COMMA(K) = 0
      ENDDO

      I = 0
      DO K = 1, LEN(STRING)
         IF( STRING(K:K).EQ.SEP ) THEN
             I = I + 1
             COMMA(I) = K
         ENDIF
      ENDDO

      !AVG = STRING(COMMA(1)+1:COMMA(2)-1)
      !AVG = AVG(LSTRIML(AVG):LSTRIM(AVG))

      STRING = STRING(LSTRIML(STRING):LSTRIM(STRING))

      IF (STRING(1:1) .EQ. 'N') THEN
          AVERAGING = 'N' 
          RETURN
      ENDIF

      IF (STRING(1:1) .EQ. 'P') THEN
          AVERAGING = 'P'
      ELSE
          AVG = STRING(1:COMMA(1)-1)
          AVG = AVG(LSTRIML(AVG):LSTRIM(AVG))
          
          OPEN(IUNIT,STATUS='SCRATCH')
          WRITE(IUNIT,'(A)') AVG
          REWIND(IUNIT)
          READ(IUNIT,*) AVERAGING
          CLOSE(IUNIT)
      ENDIF

      IF (I .EQ. 0) GOTO 900

      IF (I .EQ. 1) THEN
          STS = STRING(COMMA(1)+1:)
      ELSE
          STS = STRING(COMMA(1)+1:COMMA(2)-1)
      ENDIF
      STS = STS(LSTRIML(STS):LSTRIM(STS))

      IF (STS(1:3) .EQ. 'AVG') THEN
          STATISTICS = 'AVG'
      ELSE IF (STS(1:3) .EQ. 'MAX') THEN
          STATISTICS = 'MAX'
      ELSE IF (STS(1:3) .EQ. 'INT') THEN
          STATISTICS = 'INT'
      ELSE IF (STS(1:3) .EQ. 'PCT') THEN
          STATISTICS = 'PCT'
          
          IF (I .NE. 2) GOTO 900
          OPEN(IUNIT,STATUS='SCRATCH')
          WRITE(IUNIT,'(A)') STRING(COMMA(2)+1:)
          REWIND(IUNIT)
          READ(IUNIT,*) PERCENTILE
          CLOSE(IUNIT)
      ENDIF
 
      RETURN

900   WRITE (6,*) 'ERROR (PARSE_AVERAGING)'
      WRITE (6,*) 'Averaging record is inconsistent'
      WRITE (6,*) STRING
      STOP
    
      END
