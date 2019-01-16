C
C---------------------------------------------------------------DEFORM_AQ---
C
      PROGRAM DEFORM_AQ
C
C --- PURPOSE: This routine decodes the encoded model output
C              VERSION TO BE EXECUTED ON THE WEBSITE
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   file encoded with ENCODEG
C
C --- OUTPUT:  decoded files (one for each variable) and XML files
C
C --- CALLS:   GENERATE_DATES
C              DECODEG
C              PARSE_METAFILE
C
C --- INPUT SINTAX : ./deform_aq input_path input_file source_file model_name 
C                             upl_date delta_m delta_u upl_user upl_ip output_base_path 
C
C --- NOTES        : In case this program is compiled under Lahey Fortran for
C                    windows, uncomment the include of getarg.for at the bottom
C                    of this file.                  
C
C                    Set SLASH in DATA according to the operating system in use
C
C --- VERSION: 2010-04-12
C              2013-06-14 - Fix to error message about difference in missing value
C              2014-02-27 - Added tracking of current record entering in DECODEG to
C                           issue error line
C              2015-02-02 - include of ../lib/decodeg.for
C              2015-11-11 - Fix to VALUES dimension
C              2019-01-14 - Fix to error message
C
C---------------------------------------------------------------DEFORM_AQ---
C
      IMPLICIT NONE
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/version.prm'
      INCLUDE '../inc/enform_aq.cmn'
C
      INTEGER LOUT, LXML, LUS, LUN
      INTEGER USER_MODEL_NUMBER, USER_SEQUENCE_NUMBER, USER_CASE_NUMBER
      INTEGER TMP_MODEL_NUMBER, TMP_SEQUENCE_NUMBER, TMP_CASE_NUMBER
      INTEGER TMP_MODEL_NUMBER_ORIGINAL
      INTEGER TMP_ITEM, TMP_VAR
      INTEGER USER_METEO_DATE(5)
      INTEGER TMP_METEO_DATE(5)
      INTEGER USER_ITEM, IV, IPOST
      INTEGER TMP, NOUT, IOS, NTOUT
      INTEGER LSTRIM, I,J,K,IT,ISWRONG,IX
      INTEGER ISIG(NTMAX)
      REAL TMP_MOD, XX, YY
      REAL VAL_MISSING, FMAX, PREC
      REAL VALUES(NXM)
      CHARACTER STRING*8128
      CHARACTER NXFMT*4
      CHARACTER FORMSTRING*37
      CHARACTER USER_RANDOM_KEY*7
      CHARACTER*128 ST1, ST2, TMP_FILE
      CHARACTER*2 ST3

      CHARACTER*128 INPUT_FILE
      CHARACTER*128 SOURCE_FILE
      CHARACTER*128 INPUT_PATH
      CHARACTER*128 INPUT_PATH_AND_FILE
      CHARACTER*128 OUTPUT_BASE_PATH
      CHARACTER*128 OUTPUT_BASE
      CHARACTER*128 OUTPUT_B
      CHARACTER*128 OUTPUT_FILE
      CHARACTER*128 XML_FILE
      CHARACTER*128 STATUS_R_FILE, STATUS_C_FILE
      CHARACTER*20 USER_VAR
      CHARACTER*1 SLASH

      LOGICAL LERROR 
      LOGICAL LTEST

      INTEGER*4 IREC

      DATA LOUT, LXML, LUS, LUN / 25, 26, 27, 28 /
      DATA LERROR / .FALSE. /
C     DATA SLASH / '\' /   ! windows
      DATA SLASH / '/' /   ! linux
C
      DATA LTEST / .FALSE. /

      IREC = 11 ! top lines 
      IF (LTEST) THEN
          input_path = '..'//SLASH//'client'//SLASH
          input_file = '09900-0999-001-02-02-200604270000.ens' !'09900-0026-001-01-01-200909281200.ens'
          source_file = '..'//SLASH//'test01'//SLASH//'0999-001.src'
          model_name = 'TST'
          upl_date = '200901010101'
          delta_m = '+6h8m'
          delta_u = '+3h5m'
          upl_user = 'rbianconi'
          upl_ip = '127.0.0.1'
          output_base_path = '..'//SLASH//'test01'//SLASH
      ELSE
          CALL GETARG(1,INPUT_PATH)
          CALL GETARG(2,INPUT_FILE)
          CALL GETARG(3,SOURCE_FILE)
          CALL GETARG(4,MODEL_NAME)
          CALL GETARG(5,UPL_DATE)
          CALL GETARG(6,DELTA_M)
          CALL GETARG(7,DELTA_U)
          CALL GETARG(8,UPL_USER)
          CALL GETARG(9,UPL_IP)
          CALL GETARG(10,OUTPUT_BASE_PATH)
      ENDIF

C --- Parse metafile

      OPEN (LUS, FILE=SOURCE_FILE(1:LSTRIM(SOURCE_FILE)), 
     &           STATUS='OLD',ERR=905, IOSTAT= IOS)
      CALL PARSE_METAFILE(LUS)
      CLOSE(LUS)

      WRITE(NXFMT,'(I4.4)') NX
      FORMSTRING = '(SS,1P,'//NXFMT//'E14.6)'

C --- Check  version consistency
      IF (VERSION .LT. SOURCE_VERSION) GOTO 500

C --- Get parameters from the input filename

      INPUT_PATH_AND_FILE = INPUT_PATH(1:LSTRIM(INPUT_PATH))//
     &             INPUT_FILE(1:LSTRIM(INPUT_FILE))
      IF (LDEBUG) THEN
          WRITE(6,*) 'Input file: ',INPUT_PATH_AND_FILE
      ENDIF
C
      READ(INPUT_FILE(1:5),'(I5.5)') TMP_MODEL_NUMBER_ORIGINAL
      IF (LDEBUG) THEN
          WRITE(6,*) 'Model: ',TMP_MODEL_NUMBER_ORIGINAL
      ENDIF
      TMP_MODEL_NUMBER = TMP_MODEL_NUMBER_ORIGINAL
      IF (TMP_MODEL_NUMBER .GT. 99999) GOTO 530
      
      READ(INPUT_FILE(7:10),'(I4.4)') TMP_SEQUENCE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Sequence: ',TMP_SEQUENCE_NUMBER
      ENDIF
      READ(INPUT_FILE(12:14),'(I3.3)') TMP_CASE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Case: ',TMP_CASE_NUMBER
      ENDIF
      READ(INPUT_FILE(16:17),'(I2.2)') TMP_VAR
      IF (LDEBUG) THEN
          WRITE(6,*) 'Variable number: ',TMP_VAR
      ENDIF
      READ(INPUT_FILE(19:20),'(I2.2)') TMP_ITEM
      IF (LDEBUG) THEN
          WRITE(6,*) 'Item number: ',TMP_ITEM
      ENDIF

      READ(INPUT_FILE(22:25),'(I4.4)') TMP
          TMP_METEO_DATE(1) = TMP
          METEO_DATE(1) = TMP
      READ(INPUT_FILE(26:27),'(I2.2)') TMP
          TMP_METEO_DATE(2) = TMP
          METEO_DATE(2) = TMP
      READ(INPUT_FILE(28:29),'(I2.2)') TMP
          TMP_METEO_DATE(3) = TMP
          METEO_DATE(3) = TMP
      READ(INPUT_FILE(30:31),'(I2.2)') TMP
          TMP_METEO_DATE(4) = TMP
          METEO_DATE(4) = TMP
      READ(INPUT_FILE(32:33),'(I2.2)') TMP
          TMP_METEO_DATE(5) = TMP
          METEO_DATE(5) = TMP

      IF (LDEBUG) THEN
          WRITE(6,*) 'Meteo date: ', TMP_METEO_DATE
      ENDIF

C XML status_r

      ST1 = 's0000'//SLASH//'c000'//SLASH//'r00'//SLASH
      WRITE(ST1(2:5),'(I4.4)') TMP_SEQUENCE_NUMBER
      WRITE(ST1(8:10),'(I3.3)') TMP_CASE_NUMBER
      WRITE(ST1(13:14),'(I2.2)') TMP_VAR

      OUTPUT_BASE = OUTPUT_BASE_PATH(1:LSTRIM(OUTPUT_BASE_PATH))
     &              //ST1(1:LSTRIM(ST1))//'status_r'//SLASH

      ST1 = '00000-0000-000-00-'
      WRITE(ST1(1:5),'(I5.5)') TMP_MODEL_NUMBER
      WRITE(ST1(7:10),'(I4.4)') TMP_SEQUENCE_NUMBER
      WRITE(ST1(12:14),'(I3.3)') TMP_CASE_NUMBER
      WRITE(ST1(16:17),'(I2.2)') TMP_VAR
      OUTPUT_BASE = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//
     &              ST1(1:LSTRIM(ST1))

      ST1 = ''
      WRITE(ST1(1:12),1000) (TMP_METEO_DATE(K),K=1,5)
      STATUS_R_FILE = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//
     &                ST1(1:LSTRIM(ST1))//'.xml'

C XML status_c

      ST1 = 's0000'//SLASH//'c000'//SLASH
      WRITE(ST1(2:5),'(I4.4)') TMP_SEQUENCE_NUMBER
      WRITE(ST1(8:10),'(I3.3)') TMP_CASE_NUMBER

      OUTPUT_BASE = OUTPUT_BASE_PATH(1:LSTRIM(OUTPUT_BASE_PATH))
     &              //ST1(1:LSTRIM(ST1))//'status_c'//SLASH

      ST1 = '00000-0000-000-'
      WRITE(ST1(1:5),'(I5.5)') TMP_MODEL_NUMBER
      WRITE(ST1(7:10),'(I4.4)') TMP_SEQUENCE_NUMBER
      WRITE(ST1(12:14),'(I3.3)') TMP_CASE_NUMBER
      OUTPUT_BASE = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//
     &              ST1(1:LSTRIM(ST1))
      ST1 = ''
      WRITE(ST1(1:12),1000) (TMP_METEO_DATE(K),K=1,5)
      STATUS_C_FILE = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//
     &           ST1(1:LSTRIM(ST1))//'.xml'

      UPL_STATUS = 'Unknown'
C
C --- Write XML for status_r directory
C
      TMP_FILE = STATUS_R_FILE
      OPEN (LXML,FILE=TMP_FILE,ERR=940)
      CALL WRITE_STATUS_R_XML(LXML)
      CLOSE (LXML)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Wrote status_r file: ',TMP_FILE
      ENDIF
C
C --- Write XML for status_c directory
C
      TMP_FILE = STATUS_C_FILE
      OPEN (LXML,FILE=TMP_FILE,ERR=950)
      CALL WRITE_STATUS_C_XML(LXML)
      CLOSE (LXML)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Wrote status_c file: ',TMP_FILE
      ENDIF

C     Now read the input file and make some checks...

      TMP_FILE = INPUT_PATH_AND_FILE
      OPEN (LUN, FILE=TMP_FILE, STATUS='OLD', ERR=960)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Opened file: ', TMP_FILE
      ENDIF
      READ(LUN,'(I5.5)') USER_MODEL_NUMBER

      IF (LDEBUG) THEN
          WRITE(6,*) 'Model number in input file: ', USER_MODEL_NUMBER
      ENDIF

      IF (TMP_MODEL_NUMBER_ORIGINAL .NE. USER_MODEL_NUMBER) THEN
          ST1 = ''
          WRITE(ST1(1:5),'(I5.5)') TMP_MODEL_NUMBER_ORIGINAL
          ST2 = ''
          WRITE(ST2(1:5),'(I5.5)') USER_MODEL_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Model number in file name '//ST1(1:LSTRIM(ST1))//
     &    ' while model number in file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      MODEL_NUMBER = TMP_MODEL_NUMBER

      READ(LUN,'(A)') USER_RANDOM_KEY
      IF (LDEBUG) THEN
          WRITE(6,*) 'Random key in input file: ', USER_RANDOM_KEY
      ENDIF
      IF (USER_RANDOM_KEY .NE. SOURCE_RANDOM_KEY) THEN
          ST1 = ''
          WRITE(ST1(1:7),'(A7)') SOURCE_RANDOM_KEY
          ST2 = ''
          WRITE(ST2(1:7),'(A7)') USER_RANDOM_KEY
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Random key in source file '//ST1(1:LSTRIM(ST1))//
     &    ' while random key in file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      READ(LUN,'(I4)') USER_SEQUENCE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Sequence number in input file: ', 
     &                USER_SEQUENCE_NUMBER
      ENDIF

      IF (TMP_SEQUENCE_NUMBER .NE. USER_SEQUENCE_NUMBER) THEN
          ST1 = ''
          WRITE(ST1(1:4),'(I4.4)') TMP_SEQUENCE_NUMBER
          ST2 = ''
          WRITE(ST2(1:4),'(I4.4)') USER_SEQUENCE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Sequence number in file name is '//ST1(1:LSTRIM(ST1))//
     &    ' while sequence number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      IF (SOURCE_SEQUENCE_NUMBER .NE. USER_SEQUENCE_NUMBER) THEN
          ST1 = ''
          WRITE(ST1(1:4),'(I4.4)') SOURCE_SEQUENCE_NUMBER
          ST2 = ''
          WRITE(ST2(1:4),'(I4.4)') USER_SEQUENCE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Sequence number in source file is '//ST1(1:LSTRIM(ST1))//
     &    ' while sequence number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      READ(LUN,'(I3)') USER_CASE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Case number in input file: ', USER_CASE_NUMBER
      ENDIF

      IF (TMP_CASE_NUMBER .NE. USER_CASE_NUMBER) THEN
          ST1 = ''
          WRITE(ST1(1:3),'(I3.3)') TMP_CASE_NUMBER
          ST2 = ''
          WRITE(ST2(1:3),'(I3.3)') USER_CASE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Case number in file name is '//ST1(1:LSTRIM(ST1))//
     &    ' while case number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      IF (SOURCE_CASE_NUMBER .NE. USER_CASE_NUMBER) THEN
          ST1 = ''
          WRITE(ST1(1:3),'(I3.3)') SOURCE_CASE_NUMBER
          ST2 = ''
          WRITE(ST2(1:3),'(I3.3)') USER_CASE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Case number in source file is '//ST1(1:LSTRIM(ST1))//
     &    ' while case number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      READ(LUN,'(A)') USER_VAR
      IF (LDEBUG) THEN
          WRITE(6,*) 'Variable in input file: ', USER_VAR
      ENDIF

      CALL FIXNAME(USER_VAR)

C --- Check if Variable name and code in user file match with
C     name and code in source file
      ICODE = 0

      DO I = 1, NUC_MAX
         IF (USER_VAR .EQ. SOURCE_VAR_NAME(I)) THEN
             ICODE = I
         ENDIF
      ENDDO
      IF (ICODE .EQ. 0) THEN
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Variable name in user file does not appear in source file.'
          GOTO 999
      ENDIF

      IF (TMP_VAR .NE. ICODE) THEN
          ST1 = ''
          WRITE(ST1(1:2),'(I2.2)') ICODE
          ST2 = ''
          WRITE(ST2(1:2),'(I2.2)') TMP_VAR
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Variable number in source file: '//ST1(1:LSTRIM(ST1))//
     &    ' Variable number in user file name: '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,*) USER_ITEM
      IF (LDEBUG) THEN
          WRITE(6,*) 'Item index in input file: ', USER_ITEM
      ENDIF

      READ(LUN,*) VAL_MISSING
      IF (LDEBUG) THEN
          WRITE(6,*) 'Missing value in input file: ', VAL_MISSING
      ENDIF
      IF (VAL_MISSING .NE. VAR_ITEMS_VAL_MISSING(ICODE,USER_ITEM)) THEN
          ST1 = ''
          WRITE(ST1(1:11),'(E11.4)') VAL_MISSING
          ST2 = ''
          WRITE(ST2(1:11),'(E11.4)') 
     &           VAR_ITEMS_VAL_MISSING(ICODE,USER_ITEM)
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Missing value in user file: '//ST1(1:LSTRIM(ST1))//
     &    ' Missing value in src file : '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,*) PREC
      IF (LDEBUG) THEN
          WRITE(6,*) 'Precision in input file: ', PREC
      ENDIF
      IF (PREC .NE. VAR_ITEMS_PRECISION(ICODE,USER_ITEM)) THEN
          ST1 = ''
          WRITE(ST1(1:11),'(E11.4)') PREC
          ST2 = ''
          WRITE(ST2(1:11),'(E11.4)') 
     &           VAR_ITEMS_PRECISION(ICODE,USER_ITEM)
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Precision in source file: '//ST1(1:LSTRIM(ST1))//
     &    ' Precision in user file   : '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,'(I4.4)') NT

      IF (LDEBUG) THEN
          WRITE(6,*) 'Number of output times in model output: ', NT
      ENDIF
      IF (NT .GT. NTMAX) THEN
          ST1 = ''
          WRITE(ST1(1:4),'(I4)') NTMAX
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Time horizon exceeds NTMAX ('//ST1(1:LSTRIM(ST1))//')'
          GOTO 999
      END IF
      IF (NT .GT. NOUTPUTS) THEN
          ST1 = ''
          WRITE(ST1(1:4),'(I4)') NOUTPUTS
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &    ' Time horizon exceeds NT in source file '//ST1(1:LSTRIM(ST1))
          GOTO 999
      END IF

      READ(LUN,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Meteo date in file :', (USER_METEO_DATE(K),K=1,5)
      ENDIF
      DO K = 1, 5
         IF (TMP_METEO_DATE(K) .NE. USER_METEO_DATE(K)) THEN
             ST1 = ''
            WRITE(ST1(1:12),'(I4.4,4I2.2)') (TMP_METEO_DATE(K),J=1,5)
             ST2 = ''
          WRITE(ST2(1:12),'(I4.4,4I2.2)') (USER_METEO_DATE(K),J=1,5)
             LERROR = .TRUE.
             ERROR_MESSAGE = 'ERROR (DEFORM_AQ) - '//
     &       ' Meteo date in file name is '//ST1(1:LSTRIM(ST1))//
     &       ' while meteo date in user file is '//ST2(1:LSTRIM(ST2))
             GOTO 999
         END IF
      ENDDO

C --- Now loop on the statistics that the file might contain.

      IV = USER_ITEM - 1

      DO IPOST = 1, SOURCE_ITEM_NPOSTS(ICODE,USER_ITEM)

         IV = IV + 1

C --- Open files. XML files must be opened here because 
C     they will store the error message in case an error occours.

         ST1 = 's0000'//SLASH//'c000'//SLASH//'r00'//SLASH
         WRITE(ST1(2:5),'(I4.4)') TMP_SEQUENCE_NUMBER
         WRITE(ST1(8:10),'(I3.3)') TMP_CASE_NUMBER
         WRITE(ST1(13:14),'(I2.2)') TMP_VAR
      
         OUTPUT_BASE = OUTPUT_BASE_PATH(1:LSTRIM(OUTPUT_BASE_PATH))
     &              //ST1(1:LSTRIM(ST1))

         ST1 = '-00000-0000-000-00-'
         WRITE(ST1(2:6),'(I5.5)') TMP_MODEL_NUMBER
         WRITE(ST1(8:11),'(I4.4)') TMP_SEQUENCE_NUMBER
         WRITE(ST1(13:15),'(I3.3)') TMP_CASE_NUMBER
         WRITE(ST1(17:18),'(I2.2)') TMP_VAR

         ST3 = '00'
         WRITE(ST3(1:2),'(I2.2)') IV
         ST2 = ''
         WRITE(ST2(1:12),'(I4.4,4I2.2)') (TMP_METEO_DATE(K),K=1,5)
         OUTPUT_B = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//'v'//
     &              ST3//SLASH//'v'//
     &              ST3//ST1(1:LSTRIM(ST1))//
     &              ST2
         OUTPUT_FILE = OUTPUT_B(1:LSTRIM(OUTPUT_B))//'.dat'
         XML_FILE = OUTPUT_B(1:LSTRIM(OUTPUT_B))//'.xml'
         TMP_FILE = OUTPUT_FILE
         IF (LDEBUG) THEN
             WRITE(6,*) 'Opening output file: ',
     &                   OUTPUT_FILE(1:LSTRIM(OUTPUT_FILE))
             WRITE(6,*) 'and XML file:    ',
     &                   XML_FILE(1:LSTRIM(XML_FILE))
         ENDIF
         OPEN (LOUT,FILE=TMP_FILE,ERR=920)
         TMP_FILE = XML_FILE
         OPEN (LXML,FILE=TMP_FILE,ERR=930)

         READ(LUN,'(I4.4)') NTOUT
         NTWORK = NTOUT
         DTWORK = DELTA_MINUTES * INT (0.5 + FLOAT(NT)/FLOAT(NTOUT))

         IF (AVERAGING(ICODE,USER_ITEM,IPOST) .EQ. 'D' .OR.
     &       AVERAGING(ICODE,USER_ITEM,IPOST) .EQ. 'M' .OR.
     &       AVERAGING(ICODE,USER_ITEM,IPOST) .EQ. 'Q' ) THEN

             IF (STATISTICS(ICODE,USER_ITEM,IPOST) .EQ. 'MAX' .OR.
     &           STATISTICS(ICODE,USER_ITEM,IPOST) .EQ. 'PCT') THEN
                 DO K = 1, 5
                    OUTPUT_DATES_WORK(K,1) = SOURCE_FIRST_OUTPUT(K)
                 ENDDO
             ELSE
                 CALL GENERATE_DATES(SOURCE_FIRST_OUTPUT,
     &                    OUTPUT_DATES,DELTA_MINUTES,NT)
                 CALL FIND_OUTPUT_DATES(OUTPUT_DATES,
     &                     AVERAGING(ICODE,USER_ITEM,IPOST),
     &                     OUTPUT_DATES_WORK,NT)
             ENDIF
         ELSE
             CALL GENERATE_DATES(SOURCE_FIRST_OUTPUT,
     &                    OUTPUT_DATES_WORK,DTWORK,NTWORK)
         ENDIF

         DO IT = 1, NTOUT
            WRITE(LOUT,'(I4,4I2.2)')(OUTPUT_DATES_WORK(K,IT),K=1,5)
            DO J = NY, 1, -1
               READ(LUN,'(A)') STRING
               IREC = IREC + 1
               CALL DECODEG(IREC,STRING,VAL_MISSING,PREC,NX,VALUES)
               WRITE(LOUT,FMT=FORMSTRING)(VALUES(I),I=1,NX)
               IF (J .EQ. NY) THEN
                   VAL_MAX(IT) = VALUES(1)
               ENDIF
               DO IX = 1, NX
                  IF (VALUES(IX).GT.VAL_MAX(IT) ) THEN
                      VAL_MAX(IT) = VALUES(IX)
                  ENDIF 
               ENDDO 
            ENDDO

            IF (LDEBUG) THEN
                WRITE(6,*) 'IT, VAL_MAX:',IT, VAL_MAX(IT)
            ENDIF 
         ENDDO

         CLOSE (LOUT)

         ISWRONG = 0

         IF (.NOT.LERROR) THEN
              UPL_STATUS = 'Ok'
              ERROR_MESSAGE = ''
         ELSE
              ISWRONG = 1
              UPL_STATUS = 'Ko'
         ENDIF

C --- Write XML

         AVGOUT = AVERAGING(ICODE,USER_ITEM,IPOST)
         STTOUT = STATISTICS(ICODE,USER_ITEM,IPOST)
         PCTOUT = PERCENTILE(ICODE,USER_ITEM,IPOST)
         CALL WRITE_XML(LXML)
         CLOSE (LXML)

         IF (ISWRONG .GT. 0) THEN
             UPL_STATUS = 'Ko'
             GOTO 2000
         ENDIF

      ENDDO

2000  CONTINUE

999   CLOSE (LUN)

      IF (LDEBUG) THEN
          WRITE(6,*) "Error:",ERROR_MESSAGE
      ENDIF

C
C --- rewrite XML for status_r directory
C

      CALL GENERATE_DATES(SOURCE_FIRST_OUTPUT,
     &                    OUTPUT_DATES,DELTA_MINUTES,NT)

      TMP_FILE = STATUS_R_FILE
      OPEN (LXML,FILE=TMP_FILE,ERR=940)
      CALL WRITE_STATUS_R_XML(LXML)
      CLOSE (LXML)
C
C --- rewrite XML for status_c directory
C
      TMP_FILE = STATUS_C_FILE
      OPEN (LXML,FILE=TMP_FILE,ERR=950)
      CALL WRITE_STATUS_C_XML(LXML)
      CLOSE (LXML)
C
      stop
500   WRITE (6,*) 'ERROR (DEFORM_AQ)'
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      WRITE (6,*) 'Minimum required version: ', SOURCE_VERSION
      STOP
520   WRITE (6,*) 'ERROR (DEFORM_AQ)'
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      WRITE (6,*) 'CASE NUMBER NOT VALID'
      STOP
530   WRITE (6,*) 'ERROR (DEFORM_AQ)'
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      WRITE (6,*) 'MODEL NUMBER NOT VALID'
      STOP
905   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'The source file (',
     &        SOURCE_FILE(1:LSTRIM(SOURCE_FILE)),') cannot be opened.'
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      STOP
920   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'Cannot open dat output file ', TMP_FILE
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      stop
930   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'Cannot open xml output file ', TMP_FILE
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      stop
940   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'Cannot open xml output file in status_r directory', 
     &           TMP_FILE
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      stop
950   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'Cannot open xml output file in status_c directory', 
     &           TMP_FILE
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      stop
960   WRITE(6,*) ' ERROR (DEFORM_AQ) - '//
     &           'Cannot open input_file', 
     &           TMP_FILE
      WRITE (6,*) 'DEFORM_AQ version: ', VERSION
      stop
C
1000  FORMAT (I4.4,4I2.2)
      END
C
      INCLUDE '../lib/day_number.for'
      INCLUDE '../lib/decodeg.for'
      INCLUDE '../lib/fix24.for'
      INCLUDE '../lib/fixname.for'
      INCLUDE '../lib/generate_dates.for'
      INCLUDE '../lib/get_date.for'
!     INCLUDE 'getarg.for'
      INCLUDE '../lib/leap_year.for'
      INCLUDE '../lib/lstrim.for'
      INCLUDE '../lib/lstriml.for'
      INCLUDE '../lib/parse_averaging.for'
      INCLUDE '../lib/parse_metafile.for'
      INCLUDE '../lib/parse_item.for'
      INCLUDE '../lib/readrec.for'

      INCLUDE '../lib_server/find_output_dates.for'
      INCLUDE '../lib_server/write_xml.for'
      INCLUDE '../lib_server/write_status_c_xml.for'
      INCLUDE '../lib_server/write_status_r_xml.for'
