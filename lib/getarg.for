C
C-------------------------------------------------------------GETARG---
C
      SUBROUTINE GETARG( NN, INPSTR )
C
C     (C) Copyright ENVIROWARE srl
C                   via Dante, 142
C                   20049 Concorezzo (MB) - Italy
C                   Tel. +39.039.62036236
C                   Fax. +39.039.6042946
C                   http://www.enviroware.com
C                   mail: info@enviroware.com
C
C --- PURPOSE: This routine simulates the GETARG internal
C              command of some compilers (e.g. DIGITAL)
C              using the Lahey internal command GETCL
C              NOTE that the STATUS argument is not allowed
C
C --- INPUT:   NN     - Position of the string to be retrieved
C
C --- OUTPUT:  INPSTR - Portion of command line corresponding
C                       to the NN position to be retrieved
C
C --- CALLS:   GETCL (Lahey internal)
C
C --- AUTHOR:  R. Bellasio, 2000-08-23
C
C --- VERSION: 2000-08-23
C
C-------------------------------------------------------------GETARG---
C
      CHARACTER*200 TMPSTR
      CHARACTER*(*) INPSTR
      LOGICAL FLAG
C
C --- Get the whole command line
C
      CALL GETCL( TMPSTR )
C
C --- Error if no command line
C
      IF( LEN_TRIM( TMPSTR ) .EQ. 0 ) THEN
          WRITE(6,*) 'Error in GETARG'
          WRITE(6,*) 'Command line not specified'
          STOP
      ENDIF
C
C --- Get the desired string in the commend line
C
      FLAG = .TRUE.
      IPOS = 0
      LL     = LEN( TMPSTR )
      DO I = 1, LL
         IF( TMPSTR(I:I) .NE. ' ' .AND. FLAG ) THEN
             IPOS = IPOS + 1
             IF( IPOS .NE. NN ) THEN
                 FLAG = .FALSE.
                 CYCLE
             ENDIF
             INI  = I
             KK   = INDEX( TMPSTR(INI:), ' ' ) - 1
             INPSTR = TMPSTR(INI:INI+KK)
             EXIT
         ELSEIF( TMPSTR(I:I) .EQ. ' ' ) THEN
             FLAG = .TRUE.
         ENDIF
      END DO
C
      RETURN
      END
