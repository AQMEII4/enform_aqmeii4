C
C----------------------------------------------------------PARSE_ITEM---
C
      SUBROUTINE PARSE_ITEM(STRING,IV,DESCRIPTION,
     &                      UNITS,PREC,VAL_MISSING)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine parses an item record of the source 
C                    term file for AQ applications
C
C --- INPUT        : STRING
C
C --- OUTPUT       : IV,DESCRIPTION,UNITS,PREC,VAL_MISSING
C
C --- CALLS        : none
C
C --- VERSION      : Feb 05, 2009
C                  : 20141217 RBI removed some unused variables
C
C----------------------------------------------------------PARSE_ITEM---
C
      IMPLICIT NONE
      CHARACTER*1 SEP
      PARAMETER (SEP = ',')
      CHARACTER*(*) STRING, DESCRIPTION, UNITS
      REAL VAL_MISSING, PREC
      INTEGER IV, I, K, IUNIT, COMMA(4)
      PARAMETER (IUNIT = 44)
C
      I = 0
      DO K = 1, LEN(STRING)
         IF( STRING(K:K).EQ.SEP ) THEN
             I = I + 1
             COMMA(I) = K
         ENDIF
      ENDDO
      IF ( I .GT. 4) THEN
           WRITE (6,*) 'ERROR (PARSE_ITEM)'
           WRITE (6,*) 'Source term file is wrong...'
           WRITE (6,*) 'Too many elements in variable record.'
           STOP
      ENDIF

      OPEN(IUNIT,STATUS='SCRATCH')
      WRITE(IUNIT,'(A)') STRING(1:COMMA(1)-1)
      REWIND(IUNIT)
      READ(IUNIT,*) IV
      CLOSE(IUNIT)

      DESCRIPTION = STRING(COMMA(1)+1:COMMA(2)-1)

      UNITS = STRING((COMMA(2)+1):(COMMA(3)-1))

      OPEN(IUNIT,STATUS='SCRATCH')
      WRITE(IUNIT,'(A)') STRING(COMMA(3)+1:COMMA(4)-1)
      REWIND(IUNIT)
      READ(IUNIT,*) PREC
      CLOSE(IUNIT)

      OPEN(IUNIT,STATUS='SCRATCH')
      WRITE(IUNIT,'(A)') STRING(COMMA(4)+1:)
      REWIND(IUNIT)
      READ(IUNIT,*) VAL_MISSING
      CLOSE(IUNIT)

      RETURN
      END
