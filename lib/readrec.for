C
C---------------------------------------------------------------READREC---
C
      SUBROUTINE READREC( RECINP, NL, IVAR, NDIM, ARRAY1, ARRAY2,
     &           ARRAY3, ARRAY4, ARRAY5 )
C
C     (C) Copyright ENVIROWARE srl
C                   Via Dante, 142
C                   20049 Concorezzo (MB) - Italy
C                   Tel. +39.039.6203636
C                   Fax. +39.039.6042946
C                   http://www.enviroware.com
C                   mail: info@enviroware.com
C
C --- PURPOSE: This routine reads variables from a string.
C              Values must be divided from description by a '$'
C              (e.g. number of grids = 33 ).
C              Values can be REAL, INTEGER, CHARACTER or LOGICAL
C              as specified by the input variable IVAR.
C
C              Requires LSTRIM
C
C --- INPUT:   RECINP - Input record (array from which the first NL
C                       values will be considered)
C              NL     - Number of lines contained in the input (array)
C                       string record
C              IVAR   - Numerical flag indicating the variable type:
C                       1 => REAL
C                       2 => INTEGER
C                       3 => CHARACTER (also arrays)
C                       4 => LOGICAL
C                       5 => CHARACTER (NO ARRAYS)
C                       Use IVAR = 5 when, e.g., input line is a path
C                       containing spaces.
C              NDIM   - Number of variables to be found inside the input
C                       record. Put NDIM < 0 if check must not be done.
C                       Maximum number of variables is defined by the
C                       calling routine.
C              
C --- OUTPUT:  ARRAY1 - Real array
C              ARRAY2 - Integer array
C              ARRAY3 - Character array
C              ARRAY4 - Logical array
C              ARRAY5 - Character (NOT array)
C
C --- AUTHOR:  R. Bellasio - Enviroware srl, 1997-09-05
C
C --- VERSION: 2000-04-17 - RBe: Added check on number of variables
C                                read equal to NDIM
C              2003-12-03 - RBe: Modifica per permettere la presenza
C                                di spazi nelle stringhe
C              2004-01-16 - RBe: Sistemato bug nella definizione di
C                                ARRAY5 che cancellava ultimo carattere
C              2006-11-30 - RBi: Added IMPLICIT NONE
C              2007-12-07 - RBi: Removed use of CYCLE (non standard, pgf77
C                                does not compile)
C              2014-02-20 - RBi: formatted write on scratch file (from * to '(A)')
C
C---------------------------------------------------------------READREC---
C
      IMPLICIT NONE
C
      INTEGER MAXLNG, NL, NDIM
      INTEGER LSCR, IP, ICUT, IVAR, IPQ, I, I1, I2, IQ, INI
      INTEGER LD, IREC, LSTRIM
C
      PARAMETER( MAXLNG = 200 )
      REAL          ARRAY1(*)
      INTEGER       ARRAY2(*)
      CHARACTER*(*) ARRAY3(*)
      CHARACTER*(*) ARRAY5
      LOGICAL       ARRAY4(*)
C
      CHARACTER*(*) RECINP(*)
      CHARACTER*200 TMPSTR
      CHARACTER*1 EQU, BLANK, MSGER
      LOGICAL DOCHECK, FOUND
      PARAMETER( EQU = '$', BLANK = ' ' )
      PARAMETER( LSCR = 89 )
C
C
      MSGER = 'E'
      ICUT = 0

      IF( IVAR .NE. 1 .AND. IVAR .NE. 2 .AND. IVAR .NE. 3 .AND.
     &    IVAR .NE. 4 .AND. IVAR .NE. 5 ) GOTO 920
C
C --- Questo controllo e' stato inserito per permettere la presenza di
C     spazi vuoti nei percorsi delle directory (nelle var stringa).
      IF( IVAR .EQ. 5 .AND. NDIM .GT. 1 ) GOTO 950

      IF( NDIM .GT. 0 ) THEN
          DOCHECK = .TRUE.
      ELSE
          DOCHECK = .FALSE.
      ENDIF      
C
C --- Find the "=" sign
C
C      LOGFLG = .TRUE.
      FOUND  = .TRUE.
      DO I = 1, NL
         I1 = INDEX( RECINP(I) , EQU )
C        I2 = INDEX( RECINP(I) , EQU, LOGFLG )
         I2 = INDEX( RECINP(I) , EQU )
         IF( I1 .NE. I2 ) GOTO 910
         IF( I1 .NE. 0 ) THEN
             ICUT = I1 + 1
             IREC = I
             FOUND = .FALSE.
         ENDIF
      ENDDO
      IF( FOUND ) GOTO 940
C
C --- Write variables on a scratch file
C
      LD = 0
      OPEN(LSCR,STATUS='SCRATCH')
      DO I = IREC, NL
         IF( I .EQ. IREC ) THEN
             INI = ICUT
         ELSE
             INI = 1
         ENDIF
         TMPSTR = RECINP(I)(INI:)
         IP   = 1
         DO WHILE( IP .LE. MAXLNG )
C           IF( IP .GT. MAXLNG ) CYCLE              ! 2007-12-07
            IF( IP .GT. MAXLNG ) GOTO 10
C ---       Elimino gli spazi dopo l'uguale
            DO WHILE( TMPSTR(IP:IP) .EQ. BLANK )
               IP = IP + 1
C              IF( IP .GT. MAXLNG ) CYCLE           ! 2007-12-07
               IF( IP .GT. MAXLNG ) GOTO 5
            ENDDO
5           CONTINUE
C ---       Se IVAR = 5 ho terminato ed esco dal loop
            IF( IVAR .EQ. 5 ) GOTO 100
C ---       altrimenti: Parsing dei campi contenuti separati da spazio
            IF( IP .LE. MAXLNG ) THEN
                IQ = INDEX( TMPSTR(IP:), BLANK )
                IPQ = IP + IQ
                !WRITE(LSCR,*) TMPSTR(IP:IPQ-1)    ! 2014-02-20
                WRITE(LSCR,'(A)') TMPSTR(IP:IPQ-1)
                IP = IPQ
                LD = LD + 1
C RBe 20000417
                IF( LD .EQ. NDIM ) GOTO 100
C RBe 20000417
                IF( DOCHECK ) THEN
                    IF( LD .GT. NDIM ) GOTO 930
                ENDIF
            ENDIF
         ENDDO
10       CONTINUE
      ENDDO
C
C --- Read variables as specified by IVAR
C
 100  REWIND(LSCR)
C     REAL variables
      IF( IVAR .EQ. 1 ) THEN
          DO I = 1, LD
             READ(LSCR,*) ARRAY1(I)
          ENDDO
C     INTEGER variables
      ELSEIF( IVAR .EQ. 2 ) THEN
          DO I = 1, LD
             READ(LSCR,*) ARRAY2(I)
          ENDDO
C     CHARACTER variables
      ELSEIF( IVAR .EQ. 3 ) THEN
          DO I = 1, LD
ccc             READ(LSCR,'(A)') ARRAY3(I)
             READ(LSCR,*) ARRAY3(I)
          ENDDO
C     LOGICAL variables
      ELSEIF( IVAR .EQ. 4 ) THEN
          DO I = 1, LD
             READ(LSCR,*) ARRAY4(I)
          ENDDO
      ELSEIF( IVAR .EQ. 5 ) THEN
C     CHARACTER variable WITH SPACES, NO ARRAY
          ARRAY5 = TMPSTR(IP:)
          ARRAY5 = ARRAY5(1:LSTRIM(ARRAY5))
      ENDIF
      CLOSE(LSCR)
C
      RETURN

C --- Error messages
 910  CONTINUE
      WRITE(6,*) 'Error in READREC.'
      WRITE(6,*) 'More than 1 "=" found in 1 input record.'
      WRITE(6,*) MSGER
      STOP
C
 920  CONTINUE
      WRITE(6,*) 'Error in READREC.'
      WRITE(6,*) 'Unknown value of IVAR.'
      WRITE(6,*) 'Current value is    : ', IVAR
      WRITE(6,*) 'Possible values are : '
      WRITE(6,*) '1 => REAL'
      WRITE(6,*) '2 => INTEGER'
      WRITE(6,*) '3 => CHARACTER'
      WRITE(6,*) '4 => LOGICAL'
      WRITE(6,*) '5 => CHARACTER with spaces (not arrays)'
      WRITE(6,*) MSGER
      STOP
C
 930  CONTINUE
      WRITE(6,*) 'Error in READREC.'
      WRITE(6,*) 'Found more variables than declared.'
      WRITE(6,*) 'Number of declared variables : ', NDIM
      WRITE(6,*) 'Number of found variables    : ', LD
      WRITE(6,*) MSGER
      STOP
C
 940  CONTINUE
      WRITE(6,*) 'Error in READREC.'
      WRITE(6,*) 'No '//EQU//' found in input record.'
      WRITE(6,*) MSGER
      STOP
C
 950  CONTINUE
      WRITE(6,*) 'Error in READREC.'
      WRITE(6,*) 'Cannot manage array of strings if IVAR=5.'
      WRITE(6,*) 'Use IVAR=3.'
      WRITE(6,*) MSGER
      STOP

      END
