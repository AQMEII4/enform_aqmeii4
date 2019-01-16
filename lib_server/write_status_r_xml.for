C
C-------------------------------------------------WRITE_STATUS_R_XML---
C
      SUBROUTINE WRITE_STATUS_R_XML(LXML)
C
C --- PURPOSE: This routine writes the XML file in the status_c directory
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   LXML XML file unit
C
C --- OUTPUT:  XML file
C
C --- CALLS:   none
C
C --- VERSION: 2010-01-22 - AQ
C              
C-------------------------------------------------WRITE_STATUS_R_XML---
C
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/enform_aq.cmn'
C
      CHARACTER*12 M_DATE
      CHARACTER*128 ST
      WRITE (M_DATE( 1: 4),'(I4.4)') METEO_DATE(1)
      WRITE (M_DATE( 5: 6),'(I2.2)') METEO_DATE(2)
      WRITE (M_DATE( 7: 8),'(I2.2)') METEO_DATE(3)
      WRITE (M_DATE( 9:10),'(I2.2)') METEO_DATE(4)
      WRITE (M_DATE(11:12),'(I2.2)') METEO_DATE(5)
C    
      WRITE(LXML,'(A)') '<opt>'
      ST = UPL_STATUS
      WRITE(LXML,'(A)') '   <Status>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Status>'
      ST = ERROR_MESSAGE
      WRITE(LXML,'(A)') '   <ErrorMessage>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</ErrorMessage>'
      ST = '' 
      WRITE (ST(1:5),'(I5.5)') MODEL_NUMBER
      WRITE(LXML,'(A)') '   <ModelNumber>'//
     &                   ST(1:5)
     &                  // '</ModelNumber>'
      WRITE(LXML,'(A)') '   <ModelName>'//
     &               MODEL_NAME(1:LSTRIM(MODEL_NAME))
     &                  // '</ModelName>'
      ST = ''
      WRITE(ST,'(I4.4)') SOURCE_SEQUENCE_NUMBER
      WRITE(LXML,'(A)') '   <SequenceId>'//
     &                   ST(1:4)
     &                  // '</SequenceId>'
      ST = ''
      WRITE(ST,'(I3.3)') SOURCE_CASE_NUMBER
      WRITE(LXML,'(A)') '   <CaseId>'//
     &                   ST(1:3)
     &                  // '</CaseId>'
      ST = ''
      WRITE(ST,'(I2.2)') RELEASE_NUMBER
      WRITE(LXML,'(A)') '   <ReleaseId>'//
     &                   ST(1:2)
     &                  // '</ReleaseId>'
      WRITE(LXML,'(A)') '   <MeteoDate>'//
     &                   M_DATE
     &                  // '</MeteoDate>'
      WRITE(LXML,'(A)') '   <MeteoDelta>'//
     &                   DELTA_M(1:LSTRIM(DELTA_M))
     &                  // '</MeteoDelta>'
      WRITE(LXML,'(A)') '   <UploadDate>'//
     &                   UPL_DATE
     &                  // '</UploadDate>'
      WRITE(LXML,'(A)') '   <UploadDelta>'//
     &                   DELTA_U(1:LSTRIM(DELTA_U))
     &                  // '</UploadDelta>'
      WRITE(LXML,'(A)') '   <UploadUser>'//
     &                   UPL_USER(1:LSTRIM(UPL_USER))
     &                  // '</UploadUser>'
      WRITE(LXML,'(A)') '   <UploadIp>'//
     &                   UPL_IP(1:LSTRIM(UPL_IP))
     &                  // '</UploadIp>'
      WRITE(LXML,'(A)') '   <Coordinates>'//
     &                   COORDINATES
     &                  // '</Coordinates>'
      ST = ''
      WRITE (ST(1:10),'(F10.2)') X_MIN
      WRITE(LXML,'(A)') '   <Xmin>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Xmin>'
      ST = ''
      WRITE (ST(1:10),'(F10.2)') Y_MIN
      WRITE(LXML,'(A)') '   <Ymin>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ymin>'

      X_MAX= X_MIN + DX * (NX - 1)
      ST = ''
      WRITE (ST(1:10),'(F10.2)') X_MAX
      WRITE(LXML,'(A)') '   <Xmax>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Xmax>'

      Y_MAX= Y_MIN + DY * (NY - 1)      
      ST = ''
      WRITE (ST(1:10),'(F10.2)') Y_MAX
      WRITE(LXML,'(A)') '   <Ymax>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ymax>'
      ST = ''
      WRITE (ST(1:9),'(F9.2)') DX
      WRITE(LXML,'(A)') '   <Dx>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Dx>'
      ST = ''
      WRITE (ST(1:9),'(F9.2)') DY
      WRITE(LXML,'(A)') '   <Dy>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Dy>'
      ST = ''
      WRITE (ST(1:3),'(I3)') NX
      WRITE(LXML,'(A)') '   <Nx>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Nx>'
      ST = ''
      WRITE (ST(1:3),'(I3)') NY
      WRITE(LXML,'(A)') '   <Ny>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ny>'
      ST = ''
      WRITE (ST(1:4),'(I4.4)') NT
      WRITE(LXML,'(A)') '   <Nt>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Nt>'
      DO I = 1, NT
         ST = ''
         WRITE (ST(1:16),1000) (OUTPUT_DATES(J,I),J=1,5)
         WRITE(LXML,'(A)') '   <OutputDates>'//
     &                      ST(1:LSTRIM(ST))
     &                     // '</OutputDates>'
      ENDDO
      WRITE(LXML,'(A)') '</opt>'
C
      RETURN
C
1000  FORMAT (I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2)
C
      END
