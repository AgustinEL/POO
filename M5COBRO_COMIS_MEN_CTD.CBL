*>$RDB = TRUE
IDENTIFICATION DIVISION.
*------------------------*
PROGRAM-ID.  COBRO-COMIS-MEN.
AUTHOR.      VICTOR LAGOS  (ACCESSO LTDA).
SECURITY.
*>      Este proceso utiliza la rutina NUM_PRODUC la cual se encuentra en la
*>      AHV$LIB.OLB.
*>
*>      Modificación
*>      ============
*>      FECHA       : 19-06-2003
*>      DESCRIPCION : Se incorpora calculo de comisión:
*>                    1111 (Cargo administración anual)
*>
*>      Modificación
*>      ============
*>      FECHA       : 03-02-2004
*>      DESCRIPCION : Se incorpora generación de archivo con detalle de
*>                    comisiones por transacción adicional (1002) facturadas
*>
*>      Modificación
*>      ============
*>      FECHA       : 01-DIC-2008
*>      AUTOR       : Leonardo Gómez A. (ALPHATEC)
*>      DESCRIPCION : Según fecha de apertura de convenio de la chequera res-
*>                    pectiva, se aplica nuevo modelo de cobro de comisiones
*>                    por tramificación.
*>
*>      Modificación
*>      ============
*>      FECHA       : 19-MAY-2009
*>      AUTOR       : Leonardo Gómez A. (ALPHATEC)
*>      DESCRIPCION : Según tipo de cliente que es el tarjetahabiente de che-
*>                    quera electrónica, que se ve por tipo de convenio y fecha
*>                    de apertura del mismo, se aplica modelo de cobro de comi-
*>                    siones por tramificación o segmentación.
*>                    Las comisiones 1000 de "Cargo Fijo Mensual (2)" se dejan
*>                    de cobrar según Nro. de Prds v/s Sum. de Depósitos. Se
*>                    cobrarán si el cliente es 'S'egmentado.  Se incorpora
*>                    nueva tabla donde se graban estas comisiones.
*>
 ENVIRONMENT DIVISION.
*---------------------*
 CONFIGURATION SECTION.
*----------------------*
SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.

 INPUT-OUTPUT SECTION.
*----------------------*
FILE-CONTROL.

        SELECT AHVINFO ASSIGN TO "AHV/TMP/AHVINFO-CTD.TXT"
                ORGANIZATION IS SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL.

        SELECT AHVINFO2 ASSIGN TO "AHV/TMP/AHVINFO2-CTD.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS COM-LLAVE.

        SELECT GIRCAJAUT ASSIGN TO "AHV/TMP/GIRCAJAUT.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS REG-LLAVE.

        SELECT SALIDA ASSIGN TO "AHV/TMP/COMISION-CTD.TXT"
                ORGANIZATION IS SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL.

        SELECT CONSMIC ASSIGN TO "AHV/TMP/CONSMIC.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS MIC-CONS-CUENTA.

        SELECT RUTLCR   ASSIGN TO "AHV/TMP/RUTLCR.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS RUTLCR-RUT.

* INI VLS ACCESSO 03-02-2004
        SELECT DETCOMATM ASSIGN TO "AHV/TMP/DETCOMATM-CTD.TXT"
                ORGANIZATION IS SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL.
* FIN VLS ACCESSO 03-02-2004

* INI VLS 16-03-2007
        SELECT COMCLIEM  ASSIGN TO "AHV/TMP/COMCLIEM-A-CTD.TXT".

        SELECT GIROS-CC ASSIGN  TO "AHV/TMP/GIROS-CC.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS GIRO-CC-K.

        SELECT CONSU-CC ASSIGN  TO "AHV/TMP/CONSU-CC.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS CONS-CC-K.

        SELECT DEPOS-CC ASSIGN  TO "AHV/TMP/DEPOS-CC.DAT"
                ORGANIZATION     IS INDEXED
                ACCESS MODE      IS DYNAMIC
                RECORD KEY       IS DEPO-CC-K.
* FIN VLS 16-03-2007

 DATA DIVISION.
*--------------*

 FILE SECTION.
*--------------*
FD AHVINFO.
01 REG-AHVINFO.
   03 ENT-NRO-CUENTA            PIC  9(12).
   03 ENT-NRT-CLIENT            PIC  9(09).
   03 ENT-DGV-CLIENT            PIC  X(01).
   03 ENT-FEC-ACTIVA            PIC  9(08).
   03 ENT-PLAN                  PIC  9(04).
   03 ENT-FLG-PAGA              PIC  9(01).
   03 ENT-DIAS-PAGO             PIC  9(02).
   03 ENT-MODELO                PIC 9(03).
   03 ENT-RELACION              PIC 9(02).
   03 ENT-FLGINS                PIC X(01).
   03 ENT-FECULTAB              PIC 9(08).
* INI VLS ACCESSO 03-02-2004
   03 ENT-CONVENIO              PIC 9(05).
* FIN VLS ACCESSO 03-02-2004

FD AHVINFO2.
01 REG-AHVINFO2.
   03 COM-LLAVE.
        05 COM-TIPO-COM         PIC 9(05).
        05 COM-CODIGO-TRAN      PIC 9(04).
   03 COM-GLOSA-COM             PIC X(30).
   03 COM-MONTO-COM             PIC 9(10)V9(04).
   03 COM-PORCEN-DCTO           PIC 9(10)V9(04).
   03 COM-PERIODO-COM           PIC 9(04).
   03 COM-NUM-TXGRT             PIC 9(10)V9(04).
   03 COM-FLG-TXGRT             PIC X(01).
   03 COM-FLG-IVA               PIC X(01).
   03 COM-MONTONB-COM           PIC 9(10)V9(04).
   03 COM-TIPAG                 PIC X(01).
* INI VLS 16-03-2007
   03 COM-MONTO-COM-E           PIC 9(10)V9(04).
   03 COM-NUM-TXGRT-E           PIC 9(10)V9(04).
* FIN VLS 16-03-2007

FD GIRCAJAUT.
01 REG-GIRCAJAUT.
        03 REG-LLAVE.
           10 GIRO-CUENTA        PIC 9(12).
           10 GIRO-FECHA         PIC 9(09).
        03 GIRO-HORA             PIC 9(09).
        03 GIRO-FLG-B-NB         PIC X(01).
        03 GIRO-FLG-GC           PIC X(01).

FD SALIDA.
01 REG-SALIDA.
        03 COM-CODEMP           PIC 9(04).
        03 COM-TRN              PIC 9(04).
        03 COM-KEY              PIC 9(12).
        03 COM-TIPOPER          PIC X(01).
        03 COM-MONTO            PIC 9(16)V9(02).
        03 COM-FECHA-COB        PIC 9(08).
        03 COM-FLAG-IVA         PIC X(01).
        03 COM-FLG-PAGA         PIC 9(02).
        03 COM-DIAS-PAGO        PIC 9(02).

FD CONSMIC.
01 REG-CONSMIC.
        03 MIC-CONS-CUENTA       PIC 9(12).
        03 MIC-CONS-RUTCLI       PIC 9(09).
        03 MIC-CONS-DIGCLI       PIC X(01).
        03 MIC-CONS-NUMCON       PIC 9(03).

FD RUTLCR.
01 REG-RUTLCR.
        03 RUTLCR-RUT            PIC 9(09).

* INI VLS ACCESSO 03-02-2004
FD DETCOMATM.
01 REG-DETCOMATM.
        03 DET-CONVENIO         PIC 9(05).
        03 DET-MARCA-PAGO       PIC X(01).
        03 DET-BANCA            PIC X(12).
        03 DET-KEY              PIC 9(12).
        03 DET-PLAN             PIC 9(04).
        03 DET-TX-GRATIS        PIC 9(02).
        03 DET-TX-B-GRATIS      PIC 9(02).
        03 DET-TX-NB-GRATIS     PIC 9(02).
        03 DET-GIROS-GRATIS     PIC 9(02).
        03 DET-CONSU-GRATIS     PIC 9(02).
        03 DET-CNT-TOT-GB       PIC 9(02).
        03 DET-CNT-TOT-GNB      PIC 9(02).
        03 DET-CNT-TOT-CB       PIC 9(02).
        03 DET-CNT-TOT-CNB      PIC 9(02).
        03 DET-CNT-FAC-GB       PIC 9(02)V9(02).
        03 DET-CNT-FAC-GNB      PIC 9(02)V9(02).
        03 DET-CNT-FAC-CB       PIC 9(02).
        03 DET-CNT-FAC-CNB      PIC 9(02).
        03 DET-MTO-FAC-GB       PIC 9(03)V9(04).
        03 DET-MTO-FAC-GNB      PIC 9(03)V9(04).
        03 DET-MTO-FAC-CB       PIC 9(03)V9(04).
        03 DET-MTO-FAC-CNB      PIC 9(03)V9(04).
        03 DET-UF-FACTURACION   PIC 9(09)V9(02).
        03 DET-MTO-TOT-FAC      PIC 9(08).
* FIN VLS ACCESSO 03-02-2004
* INI VLS 16-03-2007
        03 DET-TX-GRATIS-E      PIC 9(02).
        03 DET-TX-B-GRATIS-E    PIC 9(02).
        03 DET-TX-NB-GRATIS-E   PIC 9(02).
        03 DET-GIROS-GRATIS-E   PIC 9(02).
        03 DET-CONSU-GRATIS-E   PIC 9(02).

        03 DET-CNT-FAC-GB-E     PIC 9(02)V9(02).
        03 DET-CNT-FAC-GNB-E    PIC 9(02)V9(02).
        03 DET-CNT-FAC-CB-E     PIC 9(02).
        03 DET-CNT-FAC-CNB-E    PIC 9(02).

        03 DET-MTO-FAC-GB-E     PIC 9(03)V9(04).
        03 DET-MTO-FAC-GNB-E    PIC 9(03)V9(04).
        03 DET-MTO-FAC-CB-E     PIC 9(03)V9(04).
        03 DET-MTO-FAC-CNB-E    PIC 9(03)V9(04).

        03 DET-MTO-TOT-FAC-E    PIC 9(08).

FD COMCLIEM.
01 REG-COMCLIEM.
        03 EMP-AHV-CONVENIO                     PIC 9(05).
        03 EMP-AHV-KEY                          PIC 9(12).
        03 EMP-AHV-MONTO                        PIC 9(16)V9(02).
        03 EMP-AHV-CARGO                        PIC X(01).
        03 EMP-AHV-FECHA                        PIC 9(08).
        03 EMP-TRCODE                           PIC 9(04).

FD GIROS-CC.
01 REG-GIROS-CC.
        03 GIRO-CC-K.
           10 GIRO-CC-CUENTA     PIC 9(12).
           10 GIRO-CC-FECHA      PIC 9(09).
        03 GIRO-CC-HORA          PIC 9(09).
        03 GIRO-CC-FLG-B-NB      PIC X(01).
        03 GIRO-CC-FLG-GC        PIC X(01).

FD CONSU-CC.
01 REG-CONSU-CC.
        03 CONS-CC-K.
           10 CONS-CC-CUENTA     PIC 9(12).
           10 CONS-CC-FECHA      PIC 9(09).
        03 CONS-CC-HORA          PIC 9(09).
        03 CONS-CC-FLG-B-NB      PIC X(01).
        03 CONS-CC-FLG-GC        PIC X(01).

FD DEPOS-CC.
01 REG-DEPOS-CC.
        03 DEPO-CC-K.
           10 DEPO-CC-CUENTA     PIC 9(12).
           10 DEPO-CC-FECHA      PIC 9(09).
        03 DEPO-CC-HORA          PIC 9(09).
        03 DEPO-CC-FLG-B-NB      PIC X(01).
        03 DEPO-CC-FLG-GC        PIC X(01).
* FIN VLS 16-03-2007

WORKING-STORAGE SECTION.
*------------------------
       77 mfsqlmessagetext pic x(200).
*>     % INCLUDE WS_RUT OF "LBR:COMMON_RUT.LBR"
        COPY SQLCA. *> COMMON_RUT.CPY
        COPY "DCL".
        COPY "WRK".

*>     EXEC SQL DECLARE BCT SCHEMA FOR FILENAME BCT END-EXEC.
*>     EXEC SQL DECLARE AHP SCHEMA FOR FILENAME AHP END-EXEC.
*>     EXEC SQL DECLARE AHVHIS SCHEMA FOR FILENAME AHVHIS END-EXEC.
*>     EXEC SQL DECLARE CRD SCHEMA FOR FILENAME CRD END-EXEC.
*>     EXEC SQL DECLARE TCR SCHEMA FOR FILENAME TCR END-EXEC.


* Ini Servinfor Ltda.
*>R-AHV-COMPROD             = TRUE
*>R-AHV-PND                 = TRUE
*>R-AHV-APERTURA            = TRUE
*>R-AHV-PND-HIS             = TRUE

*>INCLUDE ID            OF "GEN:RECAHV.GLB"
*>INCLUDE IO-DB         OF "GEN:RECAHV.GLB"
*>INCLUDE IO-WS         OF "GEN:RECAHV.GLB"


* Fin Servinfor Ltda

01 VARIABLES.
        03 MTO-COMIS-TUNC       PIC 9(16).
        03 FLAG-COMISION        PIC 9(01).
        03 SW-FIN               PIC 9(01).
        03 MONTO-COMISION       PIC 9(16)V9(02).
        03 MONTO-COMISION-CONS  PIC 9(16)V9(02).
        03 UF-DEL-DIA           PIC 9(09)V9(02).
        03 WS-TRCODE            PIC 9(04).
        03 WS-CTA-KEY           PIC 9(12).
        03 WS-FLAG-IVA          PIC 9(03)V9(04).
        03 WS-TOT-GRABADOS      PIC 9(07) VALUE 0.
        03 WS-TOT-GRABADOS-CC   PIC 9(07) VALUE 0.
        03 WS-CON-TXADICI       PIC 9(07) VALUE 0.
        03 WS-CON-MANTENCION    PIC 9(07) VALUE 0.
        03 WS-CON-APERTURA      PIC 9(07) VALUE 0.
        03 WS-ELIM-APERTURA     PIC 9(07) VALUE 0.
        03 WS-UPD-APERTURA      PIC 9(07) VALUE 0.
        03 NUMERO-TRN           PIC 9(06).
        03 WS-FEC-APERT         PIC 9(06).
        03 WS-CON-CONSMIC       PIC 9(07) VALUE 0.
        03 WS-CON-ADM-ANUAL     PIC 9(07) VALUE 0.

        03 MONTO-COMISION1      PIC 9(16)V9(02).
        03 MONTO-COMISION2      PIC 9(16)V9(02).
        03 MONTO-NO-BAE         PIC 9(14)V9(04).
        03 MONTO-BAE            PIC 9(14)V9(04).
        03 CNT-GIROS-N-BAE      PIC 9(07)V9(02) VALUE 0.
        03 CNT-GIROS-BAE        PIC 9(07)V9(02) VALUE 0.
        03 CNT-CONSULTAS-N-BAE  PIC 9(07)V9(02) VALUE 0.
        03 CNT-CONSULTAS-BAE    PIC 9(07)V9(02) VALUE 0.
* INI VLS 16-03-2007
        03 WS-MONTO-EMPRESA     PIC 9(16).
        03 MONTO-COMISION-E     PIC 9(16)V9(02).
        03 MONTO-COMISION-CONS-E PIC 9(16)V9(02).
        03 MONTO-COMISION1-E    PIC 9(16)V9(02).
        03 MONTO-COMISION2-E    PIC 9(16)V9(02).
        03 CNT-GIROS-N-BAE-E    PIC 9(07)V9(02) VALUE 0.
        03 CNT-GIROS-BAE-E      PIC 9(07)V9(02) VALUE 0.
        03 CNT-CONSULTAS-N-BAE-E  PIC 9(07)V9(02) VALUE 0.
        03 CNT-CONSULTAS-BAE-E  PIC 9(07)V9(02) VALUE 0.
        03 CNT-TX-CC            PIC 9(07)V9(02) VALUE 0.
        03 CNT-TX-CC-E          PIC 9(07)V9(02) VALUE 0.
        03 FIN-ARCHIVO-CC       PIC 9(07) VALUE 0.
        03 WS-NO-GIROS-CC       PIC 9(01) VALUE 0.
        03 WS-NO-CONSU-CC       PIC 9(01) VALUE 0.
        03 WS-NO-DEPOS-CC       PIC 9(01) VALUE 0.
        03 WS-CC-CUENTA         PIC 9(12) VALUE 0.
* FIN VLS 16-03-2007
        03 FIN-CONCAJAUT        PIC 9(07) VALUE 0.
        03 FIN-GIRCAJAUT        PIC 9(07) VALUE 0.
        03 WS-NO-CONCAJAUT      PIC 9(07) VALUE 0.
        03 WS-NO-GIRCAJAUT      PIC 9(07) VALUE 0.
        03 CON-GRATIS           PIC 9(07)V9(02) VALUE 0.
        03 CON-GRATIS1          PIC 9(07)V9(02) VALUE 0.
        03 COM-MONTO-COM1       PIC S9(14)V9(04) .*>COMP.
        03 WS-DIF-TXGRT         PIC 9(07) VALUE 0.
        03 WS-TOT-CONS          PIC S9(07) VALUE 0.
        03 WS-VAL-CONS          PIC 9(01)V9(01) VALUE 0.

01 FECHAS.
        05 FECHA-PROC   PIC 9(08).
        05 FECHA-DMA    REDEFINES FECHA-PROC.
                10 DMA-DIA      PIC 9(02).
                10 DMA-MES      PIC 9(02).
                10 DMA-ANO      PIC 9(04).
        05 FECHA-AMD.
                10 AMD-ANO      PIC 9(04).
                10 AMD-MES      PIC 9(02).
                10 AMD-DIA      PIC 9(02).
        05 FECHA-AMD-R  REDEFINES FECHA-AMD PIC 9(08).

01 FECHA-PROC1  PIC 9(08).
* Ini Servinfor

01 ws-sld-mov                   pic S9(16)V9(02) .*>COMP.

* Fin Servinfor

* Variables para calculo de saldo promedio anual.
* variables para validacion del cobro mantencion segundo ano..
01 MES-PROCE                    PIC 9(02).
01 ANO-PROCE                    PIC 9(04).
* FECHA INICIO PARA SUMATORIA DEL MES DE PROCESO -1
01 FEC-CAL-INI.
   03 FEC-CAL-INI-ANO           PIC 9(04).
   03 FEC-CAL-INI-MES           PIC 9(02).
   03 FEC-CAL-INI-DIA           PIC 9(02) VALUE 01.
01 FEC-CAL-INI-R REDEFINES FEC-CAL-INI    PIC 9(08).

* FECHA TERMINO PARA SUMATORIA DEL MES DE PROCESO -1
01 FEC-CAL-FIN.
   03 FEC-CAL-FIN-ANO           PIC 9(04).
   03 FEC-CAL-FIN-MES           PIC 9(02).
   03 FEC-CAL-FIN-DIA           PIC 9(02) VALUE 31.
01 FEC-CAL-FIN-R REDEFINES FEC-CAL-FIN    PIC 9(08).

01 TOT-MTO-SLDPRM               PIC S9(12) VALUE 0.

* Variables paso de parametros
01 NUM-PRODUCT                  PIC 9(01).
01 WS-NRT-CLIEN                 PIC 9(08).
01 WS-DIG                       PIC X(01).


01 WS-TABLA-DIAS.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 28.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 30.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 30.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 30.
   03                   PIC 9(02) VALUE 31.
   03                   PIC 9(02) VALUE 30.
   03                   PIC 9(02) VALUE 31.
01 WS-TAB-DIAS REDEFINES WS-TABLA-DIAS.
   03 WS-DIAS-MES OCCURS 12 TIMES PIC 9(02).

01 WS-TABLA-ABOCAR.
   03 WS-TAB-ABOCAR             OCCURS 31 TIMES PIC S9(07).

01 FEC-ACTIVACION.
   05 ACTIV-ANO     PIC 9(04).
   05 ACTIV-MES     PIC 9(02).
   05 ACTIV-DIA     PIC 9(02).
01 FEC-ACTIVACION-R REDEFINES FEC-ACTIVACION PIC 9(08).

01 ACTIV-ANO-1      PIC 9(04).

01 WS-AUX-FECHA-X.
   05 WS-AUX-DIA                PIC 9(02).
   05 WS-AUX-MES                PIC 9(02).
   05 WS-AUX-ANO                PIC 9(04).
01 WS-AUX-FECHA REDEFINES WS-AUX-FECHA-X PIC 9(08).

01 WS-FEC-INI-X.
   03 WS-ANO-INI                PIC 9(04).
   03 WS-MES-INI                PIC 9(02).
   03 WS-DIA-INI                PIC 9(02) VALUE 01.
01 WS-FEC-INI REDEFINES WS-FEC-INI-X PIC 9(08).

01 WS-FEC-FIN-X.
   03 WS-ANO-FIN                PIC 9(04).
   03 WS-MES-FIN                PIC 9(02).
   03 WS-DIA-FIN                PIC 9(02) VALUE 31.
01 WS-FEC-FIN REDEFINES WS-FEC-FIN-X PIC 9(08).

01 WS-SLD-ANO-MES-X.
   03 WS-SLD-ANO                PIC 9(04).
   03 WS-SLD-MES                PIC 9(02).
01 WS-SLD-ANO-MES REDEFINES WS-SLD-ANO-MES-X PIC 9(06).


01    WS-SLD-ANO-A              PIC 9(04).
01    WS-SLD-MES-A              PIC 9(02).

01    WS-RUTLCR                 PIC 9(01).

*INI LGA (ALPHATEC) 01/12/2008
01 R-FACXTRM-REC.
   03 R-FACXTRM-KEY             PIC S9(18) .*>COMP.
   03 R-FACXTRM-FEC-TRN         PIC S9(09) .*>COMP.
   03 R-FACXTRM-NRO-TRAMO       PIC S9(04) .*>COMP.
   03 R-FACXTRM-INI-TRAMO       PIC S9(04) .*>COMP.
   03 R-FACXTRM-FIN-TRAMO       PIC S9(04) .*>COMP.
   03 R-FACXTRM-VAL-UNIT        PIC S9(05)V9(04) .*>COMP.
   03 R-FACXTRM-NRO-TXS         PIC S9(04) .*>COMP.
   03 R-FACXTRM-VAL-UF          PIC S9(07)V9(02) .*>COMP.
   03 R-FACXTRM-MTO-COMIS       PIC S9(09) .*>COMP.
   03 R-FACXTRM-FLG-EST         PIC X(01).
   03 R-FACXTRM-PER-PROCESO     PIC S9(09) .*>COMP.

01 EMP-REC.
   03 EMP-FECHA-APER            PIC S9(09) .*>COMP.

01 WS-FLG-COM-TRAMIF            PIC 9(01).
   88 WS-SI-COM-TRAMIF          VALUE 0.
   88 WS-NO-COM-TRAMIF          VALUE 1.

01 WS-FLG-ESTADO                PIC X(01).
01 WS-FLG-TRAMIF                PIC 9(01).
   88 WS-SI-TRAMIF              VALUE 0.
   88 WS-NO-TRAMIF              VALUE 1.

01 WS-TRM-TOT-TX                PIC 9(03).
01 WS-FEC-TOPE                  PIC S9(14)V9(04) .*>COMP.
01 WS-TRM-PARAMS.
   03 WS-TRM-I.
      05 WS-TRM-I-NRO-CUENTA    PIC 9(12).
      05 WS-TRM-I-NUM-PLAN      PIC 9(04).
      05 WS-TRM-I-NUM-TRXS      PIC 9(03).
      05 WS-TRM-I-UF-DEL-DIA    PIC 9(09)V9(02).
      05 WS-TRM-I-FEC-PROC      PIC 9(08).
   03 WS-TRM-O.
      05 WS-TRM-O-NRO-TRAMO     PIC 9(03).
      05 WS-TRM-O-INI-TRAMO     PIC 9(03).
      05 WS-TRM-O-FIN-TRAMO     PIC 9(03).
      05 WS-TRM-O-UF-TRAMIF     PIC 9(01)V9(04).
      05 WS-TRM-O-MTO-TRAMIF    PIC 9(16)V9(02).
      05 WS-TRM-O-FLG-ERROR     PIC X(01).

01 BCT-MAYVAL-REC.
   03 BCT-MAYVAL-TIPVALTAB      PIC S9(04) .*>COMP.
   03 BCT-MAYVAL-CODVALTAB      PIC S9(09) .*>COMP.
   03 BCT-MAYVAL-VALPARTAB      PIC S9(14)V9(04) .*>COMP.
*FIN LGA (ALPHATEC) 01/12/2008

* Ini servinfor Ltda 18-APR-2005 13:48:10.90
01 ws-tabla     PIC 9(02).
01 ws-periodo   PIC 9(06).

        EXEC SQL DECLARE CUR_PER_ACTUAL CURSOR FOR
             SELECT TABLA
             FROM AHVHIS.AHVHIS_PERIODOS
             WHERE PERIODO = :WS-PERIODO
        END-EXEC.
* Fin servinfor Ltda 18-APR-2005 13:48:10.90


*>FOR I=1 TO 12

EXEC SQL DECLARE AHV-TRN-HIS-MES-CURSOR{I} CURSOR FOR
        SELECT KEY,FEC-TRN,HRS-TRN,COD-TRN,MTO-TRN,ABO-CAR
        FROM AHVHIS.AHV-TRN-HIS{I}
        WHERE  KEY       =: WS-CTA-KEY       AND
               ( FEC-TRN  >=: FEC-CAL-INI-R  AND
               FEC-TRN  <=: FEC-CAL-FIN-R )  AND
               (COD-TRN = 1921 OR COD-TRN = 1922)
END-EXEC.

*>NEXT I



01 AHV-TRN-HIS-MES-REC.
   03 AHV-TRN-HIS-MES-KEY       PIC S9(18) .*>COMP.
   03 AHV-TRN-HIS-MES-FEC-TRN   PIC S9(09) .*>COMP.
   03 AHV-TRN-HIS-MES-HRS-TRN   PIC S9(09) .*>COMP.
   03 AHV-TRN-HIS-MES-COD-TRN   PIC S9(04) .*>COMP.
   03 AHV-TRN-HIS-MES-MTO-TRN   PIC S9(16)V9(02) .*>COMP.
   03 AHV-TRN-HIS-MES-ABO-CAR   PIC X(1).

********************************CURSORES******************************
*-----------------------------------------------------------------------
        EXEC SQL DECLARE BCTUF CURSOR FOR
                SELECT VALPARTAB
                FROM BCT.BCTSVAL
                WHERE (TIPVALTAB  =   10          AND
                       CODVALTAB  =   998         AND
                       ANOVALTAB  = : AMD-ANO     AND
                       MESVALTAB  = : AMD-MES     AND
                       DIAVALTAB  = : AMD-DIA)
        END-EXEC.

01 REG-BCTUF.
   03 WS-UF                   PIC S9(14)V9(04) .*>COMP.

*-----------------------------------------------------------------------
        EXEC SQL DECLARE BCTFEC CURSOR FOR
                SELECT VALOR_FECHA
                FROM BCT.FEC
                WHERE (TIPO_FEC =   1)
        END-EXEC.

01 FEC-REC.
   03 FEC-VALOR-FECHA         PIC S9(09) .*>COMP.

*-----------------------------------------------------------------------

        EXEC SQL DECLARE CUR_APERTURA CURSOR FOR
                SELECT *
                FROM AHV.AHV_APERTURA_CTD
                WHERE FEC_INIPAG <= : WS-FEC-APERT
                ORDER BY KEY ASC
        END-EXEC.

**************************** FIN CURSORES ****************************

01 FEC-CTAMAS-REC.
   03 FEC-CTAMAS-TIPO           PIC S9(04) .*>COMP.
   03 FEC-CTAMAS-FECHA          PIC S9(09) .*>COMP.
   03 FEC-CTAMAS-STATUS         PIC X(01).

01 WS-STATUS                    PIC X(01).
01 WS-COBRA-CONSULTA            PIC 9(01).
01 CON-GRATIS-NBAE              PIC 9(03).

01 REG-AHVINFO2-A.
   03 COM-LLAVE-A.
        05 COM-TIPO-COM-A       PIC 9(05).
        05 COM-CODIGO-TRAN-A    PIC 9(04).
   03 COM-GLOSA-COM-A           PIC X(30).
   03 COM-MONTO-COM-A           PIC 9(10)V9(04).
   03 COM-PORCEN-DCTO-A         PIC 9(10)V9(04).
   03 COM-PERIODO-COM-A         PIC 9(04).
   03 COM-NUM-TXGRT-A           PIC 9(10)V9(04).
   03 COM-FLG-TXGRT-A           PIC X(01).
   03 COM-FLG-IVA-A             PIC X(01).
   03 COM-MONTONB-COM-A         PIC 9(10)V9(04).
   03 COM-TIPAG-A               PIC X(01).
* INI VLS 16-03-2007
   03 COM-MONTO-COM-E-A         PIC 9(10)V9(04).
   03 COM-NUM-TXGRT-E-A         PIC 9(10)V9(04).
* FIN VLS 16-03-2007

01 REG-AHVINFO2-1012.
   03 COM-LLAVE-B.
        05 COM-TIPO-COM-B       PIC 9(05).
        05 COM-CODIGO-TRAN-B    PIC 9(04).
   03 COM-GLOSA-COM-B           PIC X(30).
   03 COM-MONTO-COM-B           PIC 9(10)V9(04).
   03 COM-PORCEN-DCTO-B         PIC 9(10)V9(04).
   03 COM-PERIODO-COM-B         PIC 9(04).
   03 COM-NUM-TXGRT-B           PIC 9(10)V9(04).
   03 COM-FLG-TXGRT-B           PIC X(01).
   03 COM-FLG-IVA-B             PIC X(01).
   03 COM-MONTONB-COM-B         PIC 9(10)V9(04).
   03 COM-TIPAG-B               PIC X(01).
* INI VLS 16-03-2007
   03 COM-MONTO-COM-E-B         PIC 9(10)V9(04).
   03 COM-NUM-TXGRT-E-B         PIC 9(10)V9(04).
* FIN VLS 16-03-2007

01 REG-AHVINFO2-1022.
   03 COM-LLAVE-C.
        05 COM-TIPO-COM-C       PIC 9(05).
        05 COM-CODIGO-TRAN-C    PIC 9(04).
   03 COM-GLOSA-COM-C           PIC X(30).
   03 COM-MONTO-COM-C           PIC 9(10)V9(04).
   03 COM-PORCEN-DCTO-C         PIC 9(10)V9(04).
   03 COM-PERIODO-COM-C         PIC 9(04).
   03 COM-NUM-TXGRT-C           PIC 9(10)V9(04).
   03 COM-FLG-TXGRT-C           PIC X(01).
   03 COM-FLG-IVA-C             PIC X(01).
   03 COM-MONTONB-COM-C         PIC 9(10)V9(04).
   03 COM-TIPAG-C               PIC X(01).
* INI VLS 16-03-2007
   03 COM-MONTO-COM-E-C         PIC 9(10)V9(04).
   03 COM-NUM-TXGRT-E-C         PIC 9(10)V9(04).
* FIN VLS 16-03-2007

01 REG-AHVINFO2-1032.
   03 COM-LLAVE-D.
        05 COM-TIPO-COM-D       PIC 9(05).
        05 COM-CODIGO-TRAN-D    PIC 9(04).
   03 COM-GLOSA-COM-D           PIC X(30).
   03 COM-MONTO-COM-D           PIC 9(10)V9(04).
   03 COM-PORCEN-DCTO-D         PIC 9(10)V9(04).
   03 COM-PERIODO-COM-D         PIC 9(04).
   03 COM-NUM-TXGRT-D           PIC 9(10)V9(04).
   03 COM-FLG-TXGRT-D           PIC X(01).
   03 COM-FLG-IVA-D             PIC X(01).
   03 COM-MONTONB-COM-D         PIC 9(10)V9(04).
   03 COM-TIPAG-D               PIC X(01).
* INI VLS 16-03-2007
   03 COM-MONTO-COM-E-D         PIC 9(10)V9(04).
   03 COM-NUM-TXGRT-E-D         PIC 9(10)V9(04).
* FIN VLS 16-03-2007

01 WS-DIF-TX-GRATIS             PIC 9(01).
01 WS-DIF-TX-GRATIS1            PIC 9(01).
* INI VLS 16-03-2007
01 WS-DIF-TX-GRATIS-E           PIC 9(01).
01 WS-DIF-TX-GRATIS1-E          PIC 9(01).
* FIN VLS 16-03-2007
01 WS-TOT-DIF-TX                PIC 9(02)V9(02) VALUE 0.
01 WS-TOT-DIF-TX1               PIC 9(02)V9(02) VALUE 0.
01 WS-NO-COBRA-1000             PIC 9(07) VALUE 0.

*>INCLUDE WS OF "LBR$DOC:RESCATA-VALORES.LBR"

01 WS-SND.
   03  SND-FECHA-1              PIC 9(08).
   03  SND-FECHA-2              PIC 9(08).
   03  SND-DIAS                 PIC S9(09) .*>COMP.
01 WS-CNT-ABOINS                PIC 9(01).
01 WS-CNT-MOV1111               PIC 9(03).
01 WS-CNT-MOD1111               PIC 9(07) VALUE 0.
01 WS-CNT-INS1111               PIC 9(07) VALUE 0.

* INI VLS ACCESSO 03-02-2004
01 WS-VAR-DETCOMATM.
   03 WSS-CNT-TOT-GB            PIC 9(02).
   03 WSS-CNT-TOT-GNB           PIC 9(02).
   03 WSS-CNT-TOT-CB            PIC 9(02).
   03 WSS-CNT-TOT-CNB           PIC 9(02).
   03 WSS-CNT-FAC-GB            PIC 9(02)V9(02).
   03 WSS-CNT-FAC-GNB           PIC 9(02)V9(02).
   03 WSS-CNT-FAC-CB            PIC 9(02).
   03 WSS-CNT-FAC-CNB           PIC 9(02).
* INI VLS 16-03-2007
   03 WSS-CNT-FAC-GB-E          PIC 9(02)V9(02).
   03 WSS-CNT-FAC-GNB-E         PIC 9(02)V9(02).
   03 WSS-CNT-FAC-CB-E          PIC 9(02).
   03 WSS-CNT-FAC-CNB-E         PIC 9(02).
* FIN VLS 16-03-2007
01 WS-GRABADOS-DET              PIC 9(07) VALUE 0.
* FIN VLS ACCESSO 03-02-2004
* INI VLS 16-03-2007
01 WS-GRABADOS-COMCLIEM         PIC 9(07) VALUE 0.
01 WS-CNT-PROCESADOS            PIC 9(07) VALUE 0.
01 WS-CNT-PROCESADOS-T          PIC 9(07) VALUE 0.
01 WS-TOTAL-TXGRT-BCO-EMP       PIC 9(07) VALUE 0.
01 WS-TOTAL-TXGRT-BCO-EMP-NB    PIC 9(07) VALUE 0.
* INI VLS 16-03-2007

*INI LGA (ALPHATEC) 19/05/2009
01 LNK-CTC-PARAMS.
   03 LNK-CTC-I-CON.
      05 LNK-CTC-I-NRO-CONV         PIC 9(06).
   03 LNK-CTC-O-CON.
      05 LNK-CTC-O-FLG-TIPO         PIC X(01).

01 LNK-CCS-PARAMS.
   03 LNK-CCS-I-CON.
      05 LNK-CCS-I-NRO-CUENTA           PIC 9(12).
   03 LNK-CCS-O-CON.
      05 LNK-CCS-O-FLG-BANCO            PIC X(01).
      05 LNK-CCS-O-SERVICIOS OCCURS 20 TIMES.
         10 LNK-CCS-O-FLG-SERV          PIC X(01).
         10 LNK-CCS-O-GLS-SERV          PIC X(20).
      05 LNK-CCS-O-VAL-REEM             PIC 9(02)V9(04).
      05 LNK-CCS-O-VAL-ESTCTA           PIC 9(02)V9(04).
      05 LNK-CCS-O-VAL-TARIF            PIC 9(02)V9(04).
      05 LNK-CCS-O-VAL-COMADIC          PIC 9(02)V9(04).

01 COBSEG-REC.
   03 COBSEG-KEY                PIC S9(18) .*>COMP.
   03 COBSEG-COD-TX             PIC S9(04) .*>COMP.
   03 COBSEG-FEC-CALC           PIC S9(09) .*>COMP.
   03 COBSEG-VAL-COMIS          PIC S9(05)V9(04) .*>COMP.
   03 COBSEG-VAL-UF             PIC S9(07)V9(02) .*>COMP.
   03 COBSEG-VAL-PESOS          PIC S9(09) .*>COMP.
   03 COBSEG-FEC-CART           PIC S9(09) .*>COMP.
   03 COBSEG-FLG-EST            PIC  X(01).
*FIN LGA (ALPHATEC) 19/05/2009

       77 w-null pic s9(04) comp-5.

PROCEDURE DIVISION.

         COPY "CREA-FILES.PRO".

*-------------------*
PRINCIPAL.
*---------
        PERFORM INICIO.

* Ini servinfor Ltda 18-APR-2005 13:48:10.90
* INI VLS
*        MOVE FECHA-AMD(1:6) TO WS-PERIODO(1:6)
        MOVE FEC-CAL-INI-R(1:6) TO WS-PERIODO(1:6)
* FIN VLS
           EXEC SQL OPEN CUR_PER_ACTUAL END-EXEC
           EXEC SQL FETCH CUR_PER_ACTUAL INTO :WS-TABLA END-EXEC
        if sqlcode = 100
           DISPLAY "Error: No existe tabla para Periodo " WS-PERIODO
           DISPLAY "Error: No existe tabla para Periodo " WS-PERIODO
           stop Run
        end-if.
           EXEC SQL CLOSE CUR_PER_ACTUAL END-EXEC
* Fin servinfor Ltda 18-APR-2005 13:48:10.90

        PERFORM PROCESO.
        PERFORM TERMINO.
        STOP RUN.

INICIO.
*-----*
        CALL "LIB$INIT_TIMER"
           on overflow                                                             *> AGUSTIN OVERFLOW
              accept fecha-amd from date yyyymmdd                                  *> AGUSTIN
              move AMD-ANO to DMA-ANO                                              *> AGUSTIN
              move AMD-MES to DMA-MES                                              *> AGUSTIN
              move AMD-DIA to DMA-DIA                                              *> AGUSTIN
              display 'Overflow '   "LIB$INIT-TIMER"                               *> AGUSTIN
                      ' USAR: ' FECHA-PROC                                         *> AGUSTIN
              continue                                                             *> AGUSTIN
        end-call

        PERFORM KIO-START-TRANSACTION.
        PERFORM FECHA-PROCESO.
        PERFORM CALCULA-FECHA-180

* INI VLS ACCESSO 23-09-2003 Se rescata valor iva en forma parametrica
        MOVE 783                TO TIPVALTAB
        MOVE 1                  TO CODVALTAB
        MOVE DMA-DIA            TO DIAVALTAB
        MOVE DMA-MES            TO MESVALTAB
        MOVE DMA-ANO            TO ANOVALTAB
*INI LGA (ALPHATEC) 01/12/2008
*%      INCLUDE PD OF "LBR$DOC:RESCATA_VALORES.LBR"
*       IF IO-NO-EXITOSO THEN
        MOVE  TIPVALTAB  TO  BCT-MAYVAL-TIPVALTAB
        MOVE  CODVALTAB  TO  BCT-MAYVAL-CODVALTAB

           EXEC SQL
                SELECT  VALPARTAB
                       ,ANOVALTAB
                       ,MESVALTAB
                       ,DIAVALTAB

                INTO    :BCT-MAYVAL-VALPARTAB :w-null
                       ,:ANOVALTAB
                       ,:MESVALTAB
                       ,:DIAVALTAB
                FROM    BCT.BCTSVAL
                WHERE   TIPVALTAB =: BCT-MAYVAL-TIPVALTAB AND
                        CODVALTAB =: BCT-MAYVAL-CODVALTAB AND
                        (DIAVALTAB + (MESVALTAB * 100)
                        + (ANOVALTAB * 10000)
                                 <=: FECHA-AMD-R) AND
                        ROWNUM=1
                ORDER BY
                        ANOVALTAB DESC,
                        MESVALTAB DESC,
                        DIAVALTAB DESC

       *>            LIMIT  1
           END-EXEC
        IF SQLCODE = 0 THEN
           MOVE  BCT-MAYVAL-VALPARTAB  TO  VALPARTAB
        END-IF
        IF SQLCODE NOT = 0 THEN
*FIN LGA (ALPHATEC) 01/12/2008
           DISPLAY "NO EXISTE PARAMETRO DE IVA... SE ABORTA EL PROCESO"
           STOP RUN
        ELSE
*--> Deja IVA en 0.19
           COMPUTE VALPARTAB1 = (VALPARTAB/100)
*--> Deja IVA en 1.19
           COMPUTE VALPARTAB  = (VALPARTAB/100) + 1
        END-IF
        MOVE VALPARTAB  TO WS-FLAG-IVA

        PERFORM VERIFICA-FECHA-PROCESO.
        PERFORM LEER-UF-DELDIA.

*INI LGA (ALPHATEC) 01/12/2008
        PERFORM EXTRAE-FECHA-TOPE
*FIN LGA (ALPHATEC) 01/12/2008

        MOVE 0 TO SW-FIN.
        OPEN INPUT  AHVINFO GIRCAJAUT RUTLCR GIROS-CC CONSU-CC DEPOS-CC.
        OPEN INPUT  CONSMIC AHVINFO2.
* INI VLS ACCESSO 03-02-2004
        OPEN OUTPUT DETCOMATM COMCLIEM
* FIN VLS ACCESSO 03-02-2004
        OPEN OUTPUT SALIDA.
        PERFORM LEE-AHVINFO.

FECHA-PROCESO.
*------------*
           EXEC SQL OPEN BCTFEC END-EXEC.
        PERFORM KIO-MAKE-ERROR.
        IF PV-SUCCESS THEN
           EXEC SQL FETCH BCTFEC INTO
                : FEC-REC
           END-EXEC
           PERFORM KIO-MAKE-ERROR
           IF PV-SUCCESS THEN
              MOVE FEC-VALOR-FECHA      TO      FECHA-PROC
              MOVE DMA-DIA              TO      AMD-DIA
              MOVE DMA-MES              TO      AMD-MES WS-SLD-MES-A WS-MES-INI WS-MES-FIN
              MOVE DMA-ANO              TO      AMD-ANO WS-SLD-ANO-A WS-ANO-INI WS-ANO-FIN
              DISPLAY "FECHA DE PROCESO : ", FECHA-PROC
              MOVE FECHA-AMD-R            TO   FECHA-PROC1

* INI VLS
              MOVE AMD-ANO    TO  ANO-PROCE
              COMPUTE MES-PROCE = AMD-MES - 1
              IF MES-PROCE = 0 THEN
                 MOVE     12  TO    MES-PROCE
                 COMPUTE ANO-PROCE = AMD-ANO - 1
              END-IF
              MOVE ANO-PROCE    TO FEC-CAL-FIN-ANO FEC-CAL-INI-ANO
              MOVE MES-PROCE    TO FEC-CAL-FIN-MES FEC-CAL-INI-MES
* FIN VLS
           ELSE
              DISPLAY "NO EXISTE FECHA DE PROCESO"
              STOP RUN
           END-IF
           EXEC SQL CLOSE BCTFEC END-EXEC
        END-IF.

VERIFICA-FECHA-PROCESO.
*----------------------
* Valida que sea fecha de proceso en fec$proce
           EXEC SQL SELECT * INTO :FEC-CTAMAS-REC
                FROM BCT.FECSCTAMAS
                WHERE FECHA  =:FECHA-PROC1 AND
                      STATUS = '4'         AND
                      TIPO   = 1
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
                DISPLAY "HOY NO SE PROCESA PROGRAMA COBRO-COMIS-MEN.EXE"
                DISPLAY "FECHA EN TABLA FEC-CTAMAS DEBE SER IGUAL A FECHA DE PROCESO"
                DISPLAY "STATUS DEBE SER IGUAL A : 4"
                DISPLAY "TIPO DEBE SER IGUAL A   : 1"
                STOP RUN
        END-IF.

*INI LGA (ALPHATEC) 01/12/2008
EXTRAE-FECHA-TOPE.
*-----------------
        MOVE  0  TO  WS-FEC-TOPE
           EXEC SQL
                SELECT  VALPARTAB
                INTO    :WS-FEC-TOPE
                FROM    BCT.BCTSVAL
                WHERE   TIPVALTAB = 350 AND
                        CODVALTAB =   1
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
           DISPLAY "NO existe fecha tope de convenios... Se asume 20081231"
           SET   PV-SUCCESS  TO  TRUE
           MOVE  20081231    TO  WS-FEC-TOPE
        END-IF.
*FIN LGA (ALPHATEC) 01/12/2008

LEER-UF-DELDIA.
*-------------*
           EXEC SQL OPEN BCTUF END-EXEC.
        PERFORM KIO-MAKE-ERROR.
        IF PV-SUCCESS THEN
           EXEC SQL FETCH BCTUF INTO
                :REG-BCTUF
           END-EXEC
           PERFORM KIO-MAKE-ERROR
           IF PV-SUCCESS THEN
              MOVE WS-UF        TO UF-DEL-DIA
           ELSE
              DISPLAY " NO EXISTE UF DEL DIA (ABORT).."
           END-IF
           EXEC SQL CLOSE BCTUF END-EXEC
        END-IF.

LEE-AHVINFO.
*----------*
        READ AHVINFO NEXT AT END
                MOVE 1 TO SW-FIN
                NOT AT END
                ADD 1 TO WS-CNT-PROCESADOS WS-CNT-PROCESADOS-T
                IF WS-CNT-PROCESADOS = 10000 THEN
                   DISPLAY "REGISTROS PROCESADOS : " WS-CNT-PROCESADOS-T
                   MOVE 0 TO WS-CNT-PROCESADOS
                END-IF
        END-READ.

PROCESO.
*------*
* Comisiones por :
*       - Mantencion cuenta vista segundo ano (cargo fijo mensual 2 [1000])
*       - Transaccion adicional por ATM (1002)
*       - Consulta micromatico  (1004)
*       - Cargo Fijo anual      (0036)
        PERFORM UNTIL SW-FIN = 1
           PERFORM COM-ACUMCOB
           PERFORM LEE-AHVINFO
        END-PERFORM.
* Comisio por Apertura cuenta mas (1001)
        PERFORM COM-APERCTA.

* INI COMISION POR APERTURA CUENTA MAS (1001).
COM-APERCTA.
*----------*
        COMPUTE MES-PROCE = AMD-MES - 1.
        MOVE AMD-ANO    TO  ANO-PROCE.
        IF MES-PROCE = 0 THEN
            MOVE     12  TO    MES-PROCE
            COMPUTE ANO-PROCE = AMD-ANO - 1
        END-IF.
        MOVE ANO-PROCE      TO  WS-FEC-APERT(1:4).
        MOVE MES-PROCE      TO  WS-FEC-APERT(5:2).

        PERFORM APER-AHV-APERTURA.
        PERFORM UNTIL PV-NO-SUCCESS
             IF R-AHV-APERTURA-COD-MES = 1
                  MOVE R-AHV-APERTURA-MTO-DIF   TO  MONTO-COMISION
              ELSE
                  MOVE R-AHV-APERTURA-MTO-MEN   TO  MONTO-COMISION
             END-IF
             IF R-AHV-APERTURA-COD-MES = 12 THEN
                  PERFORM BORRA-AHV-APERTURA
             ELSE
                  PERFORM ACTUALIZA-AHV-APERTURA
             END-IF

             ADD  1  TO  WS-CON-APERTURA
             PERFORM GRABA-SALIDA-APER
             PERFORM LEER-CUR-APERTURA
        END-PERFORM.
           EXEC SQL CLOSE CUR_APERTURA END-EXEC.

ACTUALIZA-AHV-APERTURA.
*----------------------
        ADD 1  TO R-AHV-APERTURA-COD-MES
           EXEC SQL UPDATE AHV.AHV_APERTURA_CTD SET
                  COD_MES  =:R-AHV-APERTURA-COD-MES
                 WHERE CURRENT OF CUR_APERTURA
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-SUCCESS THEN
              ADD 1 TO WS-UPD-APERTURA
        ELSE
              DISPLAY "ERROR AL ACTUALIZAR TABLA AHV-APERTURA-CTD. KEY : ", R-AHV-APERTURA-KEY
        END-IF.

BORRA-AHV-APERTURA.
*------------------
           EXEC SQL DELETE FROM AHV.AHV_APERTURA_CTD
                WHERE CURRENT OF CUR_APERTURA
           END-EXEC
        IF PV-SUCCESS THEN
              ADD 1 TO WS-ELIM-APERTURA
        ELSE
              DISPLAY "ERROR AL BORRAR REGISTRO DE TABLA AHV-APERTURA-CTD. KEY : ", R-AHV-APERTURA-KEY
        END-IF.


APER-AHV-APERTURA.
*----------------*
           EXEC SQL OPEN CUR_APERTURA END-EXEC.
        PERFORM KIO-MAKE-ERROR.
        IF PV-SUCCESS THEN
           PERFORM LEER-CUR-APERTURA
        END-IF.

LEER-CUR-APERTURA.
*----------------*
        INITIALIZE R-AHV-APERTURA-REC.
           EXEC SQL FETCH CUR_APERTURA INTO
             :R-AHV-APERTURA-REC
           END-EXEC.
        PERFORM KIO-MAKE-ERROR.

* FIN COMISION POR APERTURA CUENTA MAS (1001).


COM-ACUMCOB.
*----------*
        MOVE ENT-NRO-CUENTA     TO WS-CTA-KEY.
* (el plan viene en el archivo ahvinfo)
        PERFORM EVALUA-COBRO.


EVALUA-COBRO.
*-----------*
********************************************************************************
* (ACCESSO 11/03/1999) comision por mantencion cuenta mas 2do ano
********************************************************************************
*INI LGA (ALPHATEC) 19/05/2009
*       MOVE 1000 TO WS-TRCODE.
*       MOVE ENT-FEC-ACTIVA   TO FEC-ACTIVACION-R.
*
*       COMPUTE ACTIV-ANO-1 = ACTIV-ANO + 1
*
*        IF (ACTIV-MES <= MES-PROCE AND ACTIV-ANO-1 = ANO-PROCE) OR
*            ACTIV-ANO-1 < ANO-PROCE  THEN
*                PERFORM CALCULA-SALDO-MENSUAL
*               PERFORM SACA-COMISION-MANTENCION
*        END-IF.
*FIN LGA (ALPHATEC) 19/05/2009

********************************************************************************
* Comision por Administración anual
********************************************************************************
        IF ENT-FLGINS = 'N' AND ENT-FECULTAB < SND-FECHA-2 THEN
           MOVE 1111 TO WS-TRCODE
           MOVE ENT-FEC-ACTIVA   TO FEC-ACTIVACION-R
           MOVE AMD-ANO    TO  ANO-PROCE
           COMPUTE MES-PROCE = AMD-MES - 1
           IF MES-PROCE = 0 THEN
              MOVE     12  TO    MES-PROCE
              COMPUTE ANO-PROCE = AMD-ANO - 1
           END-IF

           IF (ACTIV-MES = MES-PROCE AND ACTIV-ANO < ANO-PROCE) THEN    *> NARCISO por aca pasa ok
              PERFORM LEE-COMISION
* INI VLS 16-03-2007
              IF COM-TIPAG NOT= 'C' THEN
                 MOVE COM-MONTO-COM-E   TO COM-MONTO-COM
              END-IF
* FIN VLS 16-03-2007

      *>       IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' AND
              MOVE 10,15   TO COM-MONTO-COM                             *> NARCISO
              IF FLAG-COMISION = 1 AND                                  *> NARCISO SE SACA PARA QUE COMPUTE COMISION -> AND COM-FLG-TXGRT = ' ' AND
                 COM-MONTO-COM > 0 THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                 ELSE
                    COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                 END-IF
                 ADD 1  TO  WS-CON-ADM-ANUAL
                 PERFORM GRABA-SALIDA
* INI VLS 16-03-2007
                 IF COM-TIPAG = 'E' THEN
                    MOVE MONTO-COMISION         TO WS-MONTO-EMPRESA
                    PERFORM GRABA-COMCLIEM
                 END-IF
* FIN VLS 16-03-2007
              END-IF
           END-IF
        END-IF.

********************************************************************************
* Comision por TX de Giro Adicional por ATM Cuenta Vista.
********************************************************************************

*INI LGA (ALPHATEC) 01/12/2008
        SET  WS-NO-COM-TRAMIF  TO  TRUE
*FIN LGA (ALPHATEC) 01/12/2008

        IF ENT-MODELO = 1 THEN
* (se cambia acceso a tabla ahv_comision por una lectura a un archivo indexado ahvinfo2)
* se lee con 1002 Para giros BAE
           INITIALIZE WS-VAR-DETCOMATM REG-AHVINFO2-A
           MOVE 1002    TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE 0 TO FIN-GIRCAJAUT

           IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
              MOVE 0  TO  CNT-GIROS-BAE   CNT-GIROS-BAE-E
              MOVE 0  TO  CNT-GIROS-N-BAE CNT-GIROS-N-BAE-E
              PERFORM START-GIRCAJAUT

* Si cliente tiene transacciones por cajero automatico (bae o no bae) calcula monto de
* la comision que pagara la empresa.
              MOVE ZEROES       TO  MONTO-COMISION-E  MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E
              IF CNT-GIROS-BAE-E > 0 THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION-E ROUNDED = MONTO-COMISION-E * CNT-GIROS-BAE-E
                 ELSE
                    COMPUTE MONTO-COMISION-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E
                    COMPUTE MONTO-COMISION-E ROUNDED = MONTO-COMISION-E * CNT-GIROS-BAE-E
                 END-IF
              END-IF
              MOVE 0    TO WS-MONTO-EMPRESA
              IF MONTO-COMISION-E > 0 THEN
                 MOVE 1002              TO WS-TRCODE
                 MOVE MONTO-COMISION-E  TO WS-MONTO-EMPRESA
                 PERFORM GRABA-COMCLIEM
              END-IF

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision.
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS
              IF CNT-GIROS-BAE > 0 THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * CNT-GIROS-BAE
                 ELSE
                    COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                    COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * CNT-GIROS-BAE
                 END-IF
              END-IF

*INI LGA (ALPHATEC) 19/05/2009 --> Verifico Tipo de Cliente: 'T'ramificado o 'S'egmentado...
              MOVE  ENT-CONVENIO  TO  LNK-CTC-I-NRO-CONV
              CALL "AHVRUTCONTIPCLIE" USING LNK-CTC-PARAMS
              END-CALL
              IF LNK-CTC-O-FLG-TIPO = 'T' THEN
*FIN LGA (ALPHATEC) 19/05/2009
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                    COMPUTE WS-TRM-I-NUM-TRXS ROUNDED = WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB
                    PERFORM CALC-Y-VERIF-COMISION
                 END-IF
                 IF MONTO-COMISION > 0 THEN
                    ADD 1       TO  WS-CON-TXADICI
                    MOVE 1002   TO  WS-TRCODE
                    PERFORM GRABA-SALIDA-1002
*--> Si la comisión es por Tramificación...
                    IF WS-SI-COM-TRAMIF THEN
                       MOVE  'C'  TO  WS-FLG-ESTADO
                    ELSE
                       MOVE  'A'  TO  WS-FLG-ESTADO
                    END-IF
                    PERFORM GRABA-FACXTRM
                 END-IF
                 PERFORM GRABA-DETCOMATM
*INI LGA (ALPHATEC) 19/05/2009
              ELSE
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                     PERFORM GENERA-COMIS-SEGMENTADO
                 END-IF
              END-IF
*FIN LGA (ALPHATEC) 19/05/2009
           ELSE
              MOVE ZEROES       TO  CNT-GIROS-BAE-E
                                    CNT-GIROS-N-BAE-E

              MOVE 0  TO  CNT-GIROS-BAE
              MOVE 0  TO  CNT-GIROS-N-BAE
              PERFORM START-GIRCAJAUT
              MOVE ZEROES       TO  MONTO-COMISION-E  MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E
                                    WS-MONTO-EMPRESA
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS
              PERFORM GRABA-DETCOMATM
           END-IF
        END-IF.



        IF ENT-MODELO = 2 THEN
* se lee con 1002 Para BAE
           INITIALIZE WS-VAR-DETCOMATM
           MOVE 1002    TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE 0 TO FIN-GIRCAJAUT

           INITIALIZE REG-AHVINFO2-A REG-AHVINFO2-1012
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-A
* se lee con 1012 Para NOBAE
           MOVE 1012            TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-1012
           MOVE REG-AHVINFO2-A  TO REG-AHVINFO2

           IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
              MOVE 0  TO  CNT-GIROS-BAE CNT-GIROS-BAE-E
              MOVE 0  TO  CNT-GIROS-N-BAE CNT-GIROS-N-BAE-E
              PERFORM START-GIRCAJAUT

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision que paga la empresa.
              MOVE ZEROES       TO  MONTO-COMISION-E MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E

              IF CNT-GIROS-BAE-E > 0 OR CNT-GIROS-N-BAE-E > 0 THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION1-E + MONTO-COMISION2-E
                 ELSE
                    COMPUTE MONTO-COMISION1-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E-B
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION1-E + MONTO-COMISION2-E
                 END-IF
              END-IF
              MOVE 0    TO WS-MONTO-EMPRESA
              IF MONTO-COMISION-E > 0 THEN
                 MOVE 1002              TO WS-TRCODE
                 MOVE MONTO-COMISION-E  TO WS-MONTO-EMPRESA
                 PERFORM GRABA-COMCLIEM
              END-IF

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision.
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS

              IF CNT-GIROS-BAE > 0 OR CNT-GIROS-N-BAE > 0 THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION1 + MONTO-COMISION2
                 ELSE
                    COMPUTE MONTO-COMISION1 ROUNDED = UF-DEL-DIA * COM-MONTO-COM-B
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION1 + MONTO-COMISION2
                 END-IF
              END-IF

*INI LGA (ALPHATEC) 19/05/2009 --> Verifico Tipo de Cliente: 'T'ramificado o 'S'egmentado...
              MOVE  ENT-CONVENIO  TO  LNK-CTC-I-NRO-CONV
              CALL "AHVRUTCONTIPCLIE" USING LNK-CTC-PARAMS
              END-CALL
              IF LNK-CTC-O-FLG-TIPO = 'T' THEN
*FIN LGA (ALPHATEC) 19/05/2009
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                    COMPUTE WS-TRM-I-NUM-TRXS ROUNDED = WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB
                    PERFORM CALC-Y-VERIF-COMISION
                 END-IF

                 IF MONTO-COMISION > 0 THEN
                    ADD 1  TO  WS-CON-TXADICI
                    MOVE 1002   TO WS-TRCODE
                    PERFORM GRABA-SALIDA-1002
*--> Si la comisión es por Tramificación...
                    IF WS-SI-COM-TRAMIF THEN
                       MOVE  'C'  TO  WS-FLG-ESTADO
                    ELSE
                       MOVE  'A'  TO  WS-FLG-ESTADO
                    END-IF
                    PERFORM GRABA-FACXTRM
                 END-IF
                 PERFORM GRABA-DETCOMATM
*INI LGA (ALPHATEC) 19/05/2009
              ELSE
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                     PERFORM GENERA-COMIS-SEGMENTADO
                 END-IF
              END-IF
*FIN LGA (ALPHATEC) 19/05/2009
           ELSE
              MOVE ZEROES       TO  CNT-GIROS-BAE-E
                                    CNT-GIROS-N-BAE-E
              MOVE 0  TO  CNT-GIROS-BAE
              MOVE 0  TO  CNT-GIROS-N-BAE
              PERFORM START-GIRCAJAUT

              MOVE ZEROES       TO  MONTO-COMISION-E  MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E
                                    WS-MONTO-EMPRESA

              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS
              PERFORM GRABA-DETCOMATM
           END-IF
        END-IF.


        IF ENT-MODELO = 3 THEN
* se lee con 1002 Para BAE
           INITIALIZE WS-VAR-DETCOMATM
           MOVE 1002    TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE 0 TO FIN-GIRCAJAUT

           INITIALIZE REG-AHVINFO2-A REG-AHVINFO2-1012
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-A
* se lee con 1012 Para NOBAE
           MOVE 1012            TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-1012
           MOVE REG-AHVINFO2-A  TO REG-AHVINFO2

           IF FLAG-COMISION = 1 AND
              (COM-FLG-TXGRT = 'N' OR COM-FLG-TXGRT-B = 'N') THEN
              MOVE 0  TO  CNT-GIROS-BAE CNT-GIROS-BAE-E
              MOVE 0  TO  CNT-GIROS-N-BAE CNT-GIROS-N-BAE-E
              PERFORM START-GIRCAJAUT

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision que paga la empresa.
              MOVE ZEROES       TO  MONTO-COMISION-E MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E

              IF CNT-GIROS-BAE-E > 0 AND COM-FLG-TXGRT = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION2-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                 ELSE
                    COMPUTE MONTO-COMISION2-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                 END-IF
              END-IF

              IF CNT-GIROS-N-BAE-E > 0 AND COM-FLG-TXGRT-B = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                 ELSE
                    COMPUTE MONTO-COMISION1-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E-B
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                 END-IF
              END-IF
              COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION1-E + MONTO-COMISION2-E
              MOVE 0    TO WS-MONTO-EMPRESA
              IF MONTO-COMISION-E > 0 THEN
                 MOVE 1002              TO WS-TRCODE
                 MOVE MONTO-COMISION-E  TO WS-MONTO-EMPRESA
                 PERFORM GRABA-COMCLIEM
              END-IF

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision que paga el cliente.
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS

              IF CNT-GIROS-BAE > 0 AND COM-FLG-TXGRT = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION2 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                 ELSE
                    COMPUTE MONTO-COMISION2 ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                 END-IF
              END-IF

              IF CNT-GIROS-N-BAE > 0 AND COM-FLG-TXGRT-B = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                 ELSE
                    COMPUTE MONTO-COMISION1 ROUNDED = UF-DEL-DIA * COM-MONTO-COM-B
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                 END-IF
              END-IF
              COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION1 + MONTO-COMISION2

*INI LGA (ALPHATEC) 19/05/2009 --> Verifico Tipo de Cliente: 'T'ramificado o 'S'egmentado...
              MOVE  ENT-CONVENIO  TO  LNK-CTC-I-NRO-CONV
              CALL "AHVRUTCONTIPCLIE" USING LNK-CTC-PARAMS
              END-CALL
              IF LNK-CTC-O-FLG-TIPO = 'T' THEN
*FIN LGA (ALPHATEC) 19/05/2009
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                    COMPUTE WS-TRM-I-NUM-TRXS ROUNDED = WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB
                    PERFORM CALC-Y-VERIF-COMISION
                 END-IF
                 IF MONTO-COMISION > 0 THEN
                    ADD 1  TO  WS-CON-TXADICI
                    MOVE 1002   TO WS-TRCODE
                    PERFORM GRABA-SALIDA-1002
*--> Si la comisión es por Tramificación...
                    IF WS-SI-COM-TRAMIF THEN
                       MOVE  'C'  TO  WS-FLG-ESTADO
                    ELSE
                       MOVE  'A'  TO  WS-FLG-ESTADO
                    END-IF
                    PERFORM GRABA-FACXTRM
                 END-IF
                 PERFORM GRABA-DETCOMATM
*INI LGA (ALPHATEC) 19/05/2009
              ELSE
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                     PERFORM GENERA-COMIS-SEGMENTADO
                 END-IF
              END-IF
*FIN LGA (ALPHATEC) 19/05/2009
           ELSE
              MOVE ZEROES       TO  CNT-GIROS-BAE-E
                                    CNT-GIROS-N-BAE-E
              MOVE 0  TO  CNT-GIROS-BAE
              MOVE 0  TO  CNT-GIROS-N-BAE
              PERFORM START-GIRCAJAUT

              MOVE ZEROES       TO  MONTO-COMISION-E  MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E
                                    WS-MONTO-EMPRESA
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS
              PERFORM GRABA-DETCOMATM
           END-IF
        END-IF.


        IF ENT-MODELO = 4 THEN
* se lee con 1002 Para GIRO BAE
           INITIALIZE WS-VAR-DETCOMATM
           MOVE 1002    TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE 0 TO FIN-GIRCAJAUT

           INITIALIZE REG-AHVINFO2-A    REG-AHVINFO2-1012
                      REG-AHVINFO2-1022 REG-AHVINFO2-1032
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-A

* se lee con 1012 Para GIRO NOBAE
           MOVE 1012            TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-1012

* se lee con 1022 Para CONS NOBAE
           MOVE 1022            TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-1022

* se lee con 1032 Para CONS NOBAE
           MOVE 1032            TO WS-TRCODE
           PERFORM LEE-COMISION
           MOVE REG-AHVINFO2    TO REG-AHVINFO2-1032

* Restable lectura para 1002
           MOVE REG-AHVINFO2-A  TO REG-AHVINFO2

           IF FLAG-COMISION = 1 AND
              (COM-FLG-TXGRT = 'N' OR COM-FLG-TXGRT-C = 'N') THEN
              MOVE 0  TO  CNT-GIROS-BAE  CNT-CONSULTAS-BAE
              MOVE 0  TO  CNT-GIROS-N-BAE CNT-CONSULTAS-N-BAE
              MOVE 0  TO  CNT-GIROS-BAE-E  CNT-CONSULTAS-BAE-E
              MOVE 0  TO  CNT-GIROS-N-BAE-E CNT-CONSULTAS-N-BAE-E
              PERFORM START-GIRCAJAUT

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision que paga la empresa.
              MOVE ZEROES       TO  MONTO-COMISION-E MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E

              IF (CNT-GIROS-BAE-E > 0 OR CNT-GIROS-N-BAE-E > 0) AND COM-FLG-TXGRT = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION1-E + MONTO-COMISION2-E
                 ELSE
                    COMPUTE MONTO-COMISION1-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E-B
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-GIROS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-GIROS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION1-E + MONTO-COMISION2-E
                 END-IF
              END-IF

              IF (CNT-CONSULTAS-BAE-E > 0 OR CNT-CONSULTAS-N-BAE-E > 0) AND COM-FLG-TXGRT-C = 'N' THEN
                 IF COM-FLG-IVA-C = 'S' THEN
                    COMPUTE MONTO-COMISION1-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E-D) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-CONSULTAS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E-C) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-CONSULTAS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION-E + MONTO-COMISION1-E + MONTO-COMISION2-E
                 ELSE
                    COMPUTE MONTO-COMISION1-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E-D
                    COMPUTE MONTO-COMISION1-E ROUNDED = MONTO-COMISION1-E * CNT-CONSULTAS-N-BAE-E
                    COMPUTE MONTO-COMISION2-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E-C
                    COMPUTE MONTO-COMISION2-E ROUNDED = MONTO-COMISION2-E * CNT-CONSULTAS-BAE-E
                    COMPUTE MONTO-COMISION-E  ROUNDED = MONTO-COMISION-E + MONTO-COMISION1-E + MONTO-COMISION2-E
                 END-IF
              END-IF
              MOVE 0    TO WS-MONTO-EMPRESA
              IF MONTO-COMISION-E > 0 THEN
                 MOVE 1002              TO WS-TRCODE
                 MOVE MONTO-COMISION-E  TO WS-MONTO-EMPRESA
                 PERFORM GRABA-COMCLIEM
              END-IF

* Si cliente tiene giros por cajero automatico (bae o no bae) calcula monto de
* la comision que paga el cliente.
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS

              IF (CNT-GIROS-BAE > 0 OR CNT-GIROS-N-BAE > 0) AND COM-FLG-TXGRT = 'N' THEN
                 IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION1 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-B) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION1 + MONTO-COMISION2
                 ELSE
                    COMPUTE MONTO-COMISION1 ROUNDED = UF-DEL-DIA * COM-MONTO-COM-B
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-GIROS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-GIROS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION1 + MONTO-COMISION2
                 END-IF
              END-IF

              IF (CNT-CONSULTAS-BAE > 0 OR CNT-CONSULTAS-N-BAE > 0) AND COM-FLG-TXGRT-C = 'N' THEN
                 IF COM-FLG-IVA-C = 'S' THEN
                    COMPUTE MONTO-COMISION1 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-D) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-CONSULTAS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-C) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-CONSULTAS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION + MONTO-COMISION1 + MONTO-COMISION2
                 ELSE
                    COMPUTE MONTO-COMISION1 ROUNDED = UF-DEL-DIA * COM-MONTO-COM-D
                    COMPUTE MONTO-COMISION1 ROUNDED = MONTO-COMISION1 * CNT-CONSULTAS-N-BAE
                    COMPUTE MONTO-COMISION2 ROUNDED = UF-DEL-DIA * COM-MONTO-COM-C
                    COMPUTE MONTO-COMISION2 ROUNDED = MONTO-COMISION2 * CNT-CONSULTAS-BAE
                    COMPUTE MONTO-COMISION  ROUNDED = MONTO-COMISION + MONTO-COMISION1 + MONTO-COMISION2
                 END-IF
              END-IF

*INI LGA (ALPHATEC) 19/05/2009 --> Verifico Tipo de Cliente: 'T'ramificado o 'S'egmentado...
              MOVE  ENT-CONVENIO  TO  LNK-CTC-I-NRO-CONV
              CALL "AHVRUTCONTIPCLIE" USING LNK-CTC-PARAMS
              END-CALL
              IF LNK-CTC-O-FLG-TIPO = 'T' THEN
*FIN LGA (ALPHATEC) 19/05/2009
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                    COMPUTE WS-TRM-I-NUM-TRXS ROUNDED = WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB
                    PERFORM CALC-Y-VERIF-COMISION
                 END-IF
                 IF MONTO-COMISION > 0 THEN
                    ADD 1  TO  WS-CON-TXADICI
                    MOVE 1002   TO WS-TRCODE
                    PERFORM GRABA-SALIDA-1002
*--> Si la comisión es por Tramificación...
                    IF WS-SI-COM-TRAMIF THEN
                       MOVE  'C'  TO  WS-FLG-ESTADO
                    ELSE
                       MOVE  'A'  TO  WS-FLG-ESTADO
                    END-IF
                    PERFORM GRABA-FACXTRM
                 END-IF
                 PERFORM GRABA-DETCOMATM
*INI LGA (ALPHATEC) 19/05/2009
              ELSE
                 IF (WSS-CNT-FAC-GB + WSS-CNT-FAC-GNB + WSS-CNT-FAC-CB + WSS-CNT-FAC-CNB) > 0 THEN
                     PERFORM GENERA-COMIS-SEGMENTADO
                 END-IF
              END-IF
*FIN LGA (ALPHATEC) 19/05/2009
           ELSE
              MOVE ZEROES       TO  CNT-GIROS-BAE-E CNT-CONSULTAS-BAE-E
                                    CNT-GIROS-N-BAE-E CNT-CONSULTAS-N-BAE-E
                                    WS-MONTO-EMPRESA

              MOVE 0  TO  CNT-GIROS-BAE  CNT-CONSULTAS-BAE
              MOVE 0  TO  CNT-GIROS-N-BAE CNT-CONSULTAS-N-BAE
              PERFORM START-GIRCAJAUT

              MOVE ZEROES       TO  MONTO-COMISION-E  MONTO-COMISION1-E
                                    MONTO-COMISION2-E MONTO-COMISION-CONS-E
              MOVE ZEROES       TO  MONTO-COMISION MONTO-COMISION1
                                    MONTO-COMISION2 MONTO-COMISION-CONS
              PERFORM GRABA-DETCOMATM
           END-IF
        END-IF.

********************************************************************************
* Comision por consulta extra micromatico.
********************************************************************************
        MOVE 1004       TO WS-TRCODE.
        MOVE 0 TO  NUMERO-TRN.
* (se cambia acceso a tabla ahv_comision por una
*  lectura a un archivo indexado ahvinfo2)
        PERFORM LEE-COMISION.
* INI VLS 16-03-2007
        IF COM-TIPAG NOT= 'C' THEN
           MOVE COM-MONTO-COM-E TO COM-MONTO-COM
        END-IF
* FIN VLS 16-03-2007
        IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
           PERFORM LEE-CONSMIC
           MOVE MIC-CONS-NUMCON  TO  NUMERO-TRN
           IF NUMERO-TRN > COM-NUM-TXGRT THEN
              IF COM-FLG-IVA = 'S' THEN
                 COMPUTE MONTO-COMISION ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                 COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * (NUMERO-TRN - COM-NUM-TXGRT)
              ELSE
                 COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                 COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * (NUMERO-TRN - COM-NUM-TXGRT)
              END-IF
              ADD 1  TO  WS-CON-CONSMIC
              PERFORM GRABA-SALIDA
* INI VLS 16-03-2007
              IF COM-TIPAG = 'E' THEN
                 MOVE MONTO-COMISION    TO WS-MONTO-EMPRESA
                 PERFORM GRABA-COMCLIEM
              END-IF
* FIN VLS 16-03-2007
           END-IF
        END-IF.

* INI VLS 16-03-2007
********************************************************************************
* Comision por TX de Giro Adicional CAJA-CHILE
********************************************************************************
        MOVE 1013       TO WS-TRCODE
        PERFORM LEE-COMISION
        MOVE 0 TO FIN-ARCHIVO-CC

        IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
              MOVE 0  TO  CNT-TX-CC CNT-TX-CC-E
              PERFORM START-GIROS-CC
              PERFORM CALCULA-COMISION-CC
        END-IF.

********************************************************************************
* Comision por TX de Consulta Adicional CAJA-CHILE
********************************************************************************
        MOVE 1014       TO WS-TRCODE
        PERFORM LEE-COMISION
        MOVE 0 TO FIN-ARCHIVO-CC

        IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
              MOVE 0  TO  CNT-TX-CC CNT-TX-CC-E
              PERFORM START-CONSU-CC
              PERFORM CALCULA-COMISION-CC
        END-IF.

********************************************************************************
* Comision por TX de Deposito Adicional CAJA-CHILE
********************************************************************************
        MOVE 1015       TO WS-TRCODE
        PERFORM LEE-COMISION
        MOVE 0 TO FIN-ARCHIVO-CC

        IF FLAG-COMISION = 1 AND COM-FLG-TXGRT = 'N' THEN
              MOVE 0  TO  CNT-TX-CC CNT-TX-CC-E
              PERFORM START-DEPOS-CC
              PERFORM CALCULA-COMISION-CC
        END-IF.
* FIN VLS 16-03-2007


CALCULA-FECHA-180.
*-----------------
        MOVE  -180              TO SND-DIAS
        MOVE  ZEROS             TO SND-FECHA-2
        MOVE  FECHA-AMD-R       TO SND-FECHA-1
        CALL "SND104" USING SND-FECHA-1, SND-FECHA-2, SND-DIAS.

START-GIRCAJAUT.
*---------------
        INITIALIZE REG-GIRCAJAUT WS-NO-GIRCAJAUT
        MOVE ENT-NRO-CUENTA  TO GIRO-CUENTA
        MOVE 0               TO GIRO-FECHA
        START GIRCAJAUT KEY IS >= REG-LLAVE
          INVALID KEY
             MOVE 1    TO  WS-NO-GIRCAJAUT
          NOT INVALID KEY
             IF ENT-MODELO = 1 THEN
                PERFORM PROCESA-MODELO1
             END-IF
             IF ENT-MODELO = 2 THEN
                PERFORM PROCESA-MODELO2
             END-IF
             IF ENT-MODELO = 3 THEN
                PERFORM PROCESA-MODELO3
             END-IF
             IF ENT-MODELO = 4 THEN
                PERFORM PROCESA-MODELO4
             END-IF
        END-START.

PROCESA-MODELO1.
*---------------
*INI LGA (ALPHATEC) 01/12/2008
        MOVE  0  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

        COMPUTE WS-VAL-CONS = 1 / ENT-RELACION
        MOVE 0 TO  CON-GRATIS
        MOVE 0 TO  WS-DIF-TX-GRATIS
* INI VLS 16-03-2007
        MOVE 0 TO  WS-DIF-TX-GRATIS-E
* FIN VLS 16-03-2007
        MOVE 1 TO  WS-TOT-DIF-TX
        IF ENT-NRO-CUENTA = GIRO-CUENTA THEN
                PERFORM LEE-GIRCAJAUT
                PERFORM UNTIL FIN-GIRCAJAUT = 1
                              OR ENT-NRO-CUENTA NOT= GIRO-CUENTA

*INI LGA (ALPHATEC) 01/12/2008
                        ADD  1  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

                        IF GIRO-FLG-GC = 'G' THEN
                           ADD 1                TO CON-GRATIS
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-GB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-GNB
                           END-IF
                        ELSE
                           ADD WS-VAL-CONS      TO CON-GRATIS
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-CB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-CNB
                           END-IF
                        END-IF
* INI VLS 16-03-2007
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP = COM-NUM-TXGRT + COM-NUM-TXGRT-E
                        IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP THEN
*--> Tx. que paga el cliente
                           IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-BAE
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    ADD 1               TO WSS-CNT-FAC-GB
                                 ELSE
                                    ADD 1               TO WSS-CNT-FAC-GNB
                                 END-IF
                           ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-BAE
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    ADD 1               TO WSS-CNT-FAC-CB
                                 ELSE
                                    ADD 1               TO WSS-CNT-FAC-CNB
                                 END-IF
                           END-IF
*--> Resta
                           IF WS-DIF-TX-GRATIS = 0 AND WS-TOTAL-TXGRT-BCO-EMP > 0 AND GIRO-FLG-GC = 'G' THEN
                              COMPUTE WS-TOT-DIF-TX = CON-GRATIS - WS-TOTAL-TXGRT-BCO-EMP
                              IF WS-TOT-DIF-TX < 1 THEN
                                 SUBTRACT  WS-TOT-DIF-TX        FROM  CNT-GIROS-BAE
                                 IF CNT-GIROS-BAE-E > 0 THEN
                                    ADD WS-TOT-DIF-TX           TO CNT-GIROS-BAE-E
                                 END-IF
                                 IF GIRO-FLG-GC = 'G' THEN
                                    IF GIRO-FLG-B-NB = 'S' THEN
                                       SUBTRACT WS-TOT-DIF-TX   FROM    WSS-CNT-FAC-GB
                                       IF COM-NUM-TXGRT-E > 0 THEN
                                          ADD WS-TOT-DIF-TX     TO WSS-CNT-FAC-GB-E
                                       END-IF
                                    ELSE
                                       SUBTRACT WS-TOT-DIF-TX   FROM    WSS-CNT-FAC-GNB
                                       IF COM-NUM-TXGRT-E > 0 THEN
                                          ADD WS-TOT-DIF-TX     TO WSS-CNT-FAC-GNB-E
                                       END-IF
                                    END-IF
                                 ELSE
                                    IF GIRO-FLG-B-NB = 'S' THEN
                                       SUBTRACT 1       FROM    WSS-CNT-FAC-CB
                                       IF COM-NUM-TXGRT-E > 0 THEN
                                          ADD WS-TOT-DIF-TX     TO WSS-CNT-FAC-CB-E
                                       END-IF
                                    ELSE
                                       SUBTRACT    1    FROM    WSS-CNT-FAC-CNB
                                       IF COM-NUM-TXGRT-E > 0 THEN
                                          ADD WS-TOT-DIF-TX     TO WSS-CNT-FAC-CNB-E
                                       END-IF
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                           MOVE      1          TO  WS-DIF-TX-GRATIS

                        ELSE
                           IF CON-GRATIS > COM-NUM-TXGRT THEN
*--> Tx. que paga la empresa
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-BAE-E
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    ADD 1               TO WSS-CNT-FAC-GB-E
                                 ELSE
                                    ADD 1               TO WSS-CNT-FAC-GNB-E
                                 END-IF
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-BAE-E
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    ADD 1               TO WSS-CNT-FAC-CB-E
                                 ELSE
                                    ADD 1               TO WSS-CNT-FAC-CNB-E
                                 END-IF
                              END-IF
*--> Resta
                              IF WS-DIF-TX-GRATIS-E = 0 AND COM-NUM-TXGRT > 0 AND GIRO-FLG-GC = 'G' THEN
                                 COMPUTE WS-TOT-DIF-TX = CON-GRATIS - COM-NUM-TXGRT
                                 IF WS-TOT-DIF-TX < 1 THEN
                                    SUBTRACT  WS-TOT-DIF-TX     FROM  CNT-GIROS-BAE-E
                                    IF GIRO-FLG-GC = 'G' THEN
                                       IF GIRO-FLG-B-NB = 'S' THEN
                                          SUBTRACT WS-TOT-DIF-TX        FROM    WSS-CNT-FAC-GB-E
                                       ELSE
                                          SUBTRACT WS-TOT-DIF-TX   FROM    WSS-CNT-FAC-GNB-E
                                       END-IF
                                    ELSE
                                       IF GIRO-FLG-B-NB = 'S' THEN
                                          SUBTRACT      1       FROM    WSS-CNT-FAC-CB-E
                                       ELSE
                                          SUBTRACT    1    FROM    WSS-CNT-FAC-CNB-E
                                       END-IF
                                    END-IF
                                 END-IF

                              END-IF
                              MOVE      1               TO  WS-DIF-TX-GRATIS-E
                           END-IF
                        END-IF
                        PERFORM LEE-GIRCAJAUT
                END-PERFORM
        END-IF.

PROCESA-MODELO2.
*---------------
*INI LGA (ALPHATEC) 01/12/2008
        MOVE  0  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

        COMPUTE WS-VAL-CONS = 1 / ENT-RELACION
        MOVE 0 TO  CON-GRATIS
        MOVE 0 TO  WS-DIF-TX-GRATIS
* INI VLS 16-03-2007
        MOVE 0 TO  WS-DIF-TX-GRATIS-E
* FIN VLS 16-03-2007
        MOVE 1 TO  WS-TOT-DIF-TX
        IF ENT-NRO-CUENTA = GIRO-CUENTA THEN
                PERFORM LEE-GIRCAJAUT
                PERFORM UNTIL FIN-GIRCAJAUT = 1
                              OR ENT-NRO-CUENTA NOT= GIRO-CUENTA

*INI LGA (ALPHATEC) 01/12/2008
                        ADD  1  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

                        IF GIRO-FLG-GC = 'G' THEN
                           ADD 1                TO CON-GRATIS
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-GB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-GNB
                           END-IF
                        ELSE
                           ADD WS-VAL-CONS      TO CON-GRATIS
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-CB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-CNB
                           END-IF
                        END-IF

* INI VLS 16-03-2007
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP = COM-NUM-TXGRT + COM-NUM-TXGRT-E
                        IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP THEN
*--> Tx. que paga el cliente
*--> Calcula diferencia
                           IF WS-DIF-TX-GRATIS = 0 AND WS-TOTAL-TXGRT-BCO-EMP > 0 THEN
                              COMPUTE WS-TOT-DIF-TX = CON-GRATIS - WS-TOTAL-TXGRT-BCO-EMP
                           END-IF
*--> Suma transacciones a cobrar
                           IF GIRO-FLG-B-NB = 'S' THEN
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-BAE
                                 ADD 1          TO WSS-CNT-FAC-GB
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-BAE
                                 ADD 1          TO WSS-CNT-FAC-CB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX      FROM    WSS-CNT-FAC-GB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-GB-E
                                    END-IF
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-CB-E
                                    END-IF
                                 END-IF
                              END-IF
                           ELSE
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-GNB
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-CNB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-N-BAE
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX      FROM    WSS-CNT-FAC-GNB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-N-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-GNB-E
                                    END-IF
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CNB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-N-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-CNB-E
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                           MOVE      1           TO  WS-DIF-TX-GRATIS

                        ELSE
                           IF CON-GRATIS > COM-NUM-TXGRT THEN
*--> Tx. que paga la empresa
*--> Calcula diferencia
                              IF WS-DIF-TX-GRATIS-E = 0 AND COM-NUM-TXGRT > 0 THEN
                                 COMPUTE WS-TOT-DIF-TX = CON-GRATIS - COM-NUM-TXGRT
                              END-IF
*--> Suma transacciones a cobrar
                              IF GIRO-FLG-B-NB = 'S' THEN
                                 IF GIRO-FLG-GC = 'G' THEN
                                    ADD 1               TO CNT-GIROS-BAE-E
                                    ADD 1               TO WSS-CNT-FAC-GB-E
                                 ELSE
                                    ADD WS-VAL-CONS     TO CNT-GIROS-BAE-E
                                    ADD 1               TO WSS-CNT-FAC-CB-E
                                 END-IF
*--> Resta diferencia
                                 IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE-E
                                    IF GIRO-FLG-GC = 'G' THEN
                                       SUBTRACT WS-TOT-DIF-TX   FROM    WSS-CNT-FAC-GB-E
                                    ELSE
                                       SUBTRACT 1               FROM    WSS-CNT-FAC-CB-E
                                    END-IF
                                 END-IF
                              ELSE
                                 IF GIRO-FLG-GC = 'G' THEN
                                    ADD 1               TO CNT-GIROS-N-BAE-E
                                    ADD 1               TO WSS-CNT-FAC-GNB-E
                                 ELSE
                                    ADD WS-VAL-CONS     TO CNT-GIROS-N-BAE-E
                                    ADD 1               TO WSS-CNT-FAC-CNB-E
                                 END-IF
*--> Resta diferencia
                                 IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-N-BAE-E
                                    IF GIRO-FLG-GC = 'G' THEN
                                       SUBTRACT WS-TOT-DIF-TX   FROM    WSS-CNT-FAC-GNB-E
                                    ELSE
                                       SUBTRACT 1               FROM    WSS-CNT-FAC-CNB-E
                                    END-IF
                                 END-IF
                              END-IF
                              MOVE      1                TO  WS-DIF-TX-GRATIS-E
                           END-IF
                        END-IF
                        PERFORM LEE-GIRCAJAUT
                END-PERFORM
        END-IF.

PROCESA-MODELO3.
*---------------
*INI LGA (ALPHATEC) 01/12/2008
        MOVE  0  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

        COMPUTE WS-VAL-CONS = 1 / ENT-RELACION
        MOVE 0 TO  CON-GRATIS
        MOVE 0 TO  CON-GRATIS1
        MOVE 0 TO  WS-DIF-TX-GRATIS
        MOVE 0 TO  WS-DIF-TX-GRATIS1
* INI VLS 16-03-2007
        MOVE 0 TO  WS-DIF-TX-GRATIS-E
        MOVE 0 TO  WS-DIF-TX-GRATIS1-E
* FIN VLS 16-03-2007
        MOVE 1 TO  WS-TOT-DIF-TX WS-TOT-DIF-TX1
        IF ENT-NRO-CUENTA = GIRO-CUENTA THEN
                PERFORM LEE-GIRCAJAUT
                PERFORM UNTIL FIN-GIRCAJAUT = 1
                              OR ENT-NRO-CUENTA NOT= GIRO-CUENTA

*INI LGA (ALPHATEC) 01/12/2008
                        ADD  1  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

                        IF GIRO-FLG-B-NB = 'S' THEN
                           IF GIRO-FLG-GC = 'G' THEN
                              ADD 1             TO CON-GRATIS
                              ADD 1             TO WSS-CNT-TOT-GB
                           ELSE
                              ADD WS-VAL-CONS   TO CON-GRATIS
                              ADD 1             TO WSS-CNT-TOT-CB
                           END-IF
                        ELSE
                           IF GIRO-FLG-GC = 'G' THEN
                              ADD 1             TO CON-GRATIS1
                              ADD 1             TO WSS-CNT-TOT-GNB
                           ELSE
                              ADD WS-VAL-CONS   TO CON-GRATIS1
                              ADD 1             TO WSS-CNT-TOT-CNB
                           END-IF
                        END-IF

* INI VLS 16-03-2007
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP = COM-NUM-TXGRT + COM-NUM-TXGRT-E
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP-NB = COM-NUM-TXGRT-B + COM-NUM-TXGRT-E-B
                        IF (CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND COM-FLG-TXGRT = 'N') OR
                           (CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND COM-FLG-TXGRT-B = 'N') THEN
*--> Tx. que paga el cliente
*--> Calcula diferencia bae
                           IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND WS-DIF-TX-GRATIS = 0 AND WS-TOTAL-TXGRT-BCO-EMP > 0 THEN
                              COMPUTE WS-TOT-DIF-TX  = CON-GRATIS - WS-TOTAL-TXGRT-BCO-EMP
                           END-IF
*--> Calcula diferencia nobae
                           IF CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND WS-DIF-TX-GRATIS1 = 0 AND WS-TOTAL-TXGRT-BCO-EMP-NB > 0 THEN
                              COMPUTE WS-TOT-DIF-TX1 = CON-GRATIS1 - WS-TOTAL-TXGRT-BCO-EMP-NB
                           END-IF

                           IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND GIRO-FLG-B-NB = 'S' THEN
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-BAE
                                 ADD 1                  TO WSS-CNT-FAC-GB
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-BAE
                                 ADD 1                  TO WSS-CNT-FAC-CB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX      FROM    WSS-CNT-FAC-GB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-GB-E
                                    END-IF
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CB
                                    IF COM-NUM-TXGRT-E > 0 THEN
                                       ADD WS-TOT-DIF-TX        TO CNT-GIROS-BAE-E
                                       ADD WS-TOT-DIF-TX        TO WSS-CNT-FAC-CB-E
                                    END-IF
                                 END-IF
                              END-IF
                              MOVE      1                TO  WS-DIF-TX-GRATIS
                           END-IF

                           IF CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND GIRO-FLG-B-NB = 'N' THEN
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-GNB
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-CNB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX1 < 1 AND WS-DIF-TX-GRATIS1 = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-GIROS-N-BAE
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX1     FROM    WSS-CNT-FAC-GNB
                                    IF COM-NUM-TXGRT-E-B > 0 THEN
                                       ADD WS-TOT-DIF-TX1       TO CNT-GIROS-N-BAE-E
                                       ADD WS-TOT-DIF-TX1       TO WSS-CNT-FAC-GNB-E
                                    END-IF
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CNB
                                    IF COM-NUM-TXGRT-E-B > 0 THEN
                                       ADD WS-TOT-DIF-TX1       TO CNT-GIROS-N-BAE-E
                                       ADD WS-TOT-DIF-TX1       TO WSS-CNT-FAC-CNB-E
                                    END-IF
                                 END-IF
                              END-IF
                              MOVE      1                 TO  WS-DIF-TX-GRATIS1
                           END-IF
                        ELSE
*--> Tx. que paga la empresa
*--> Calcula diferencia bae
                           IF CON-GRATIS > COM-NUM-TXGRT AND WS-DIF-TX-GRATIS-E = 0 AND COM-NUM-TXGRT > 0 THEN
                              COMPUTE WS-TOT-DIF-TX  = CON-GRATIS - COM-NUM-TXGRT
                           END-IF
*--> Calcula diferencia nobae
                           IF CON-GRATIS1 > COM-NUM-TXGRT-B AND WS-DIF-TX-GRATIS1-E = 0 AND COM-NUM-TXGRT-B > 0 THEN
                              COMPUTE WS-TOT-DIF-TX1 = CON-GRATIS1 - COM-NUM-TXGRT-B
                           END-IF

                           IF CON-GRATIS > COM-NUM-TXGRT AND GIRO-FLG-B-NB = 'S' THEN
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-GB-E
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-CB-E
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE-E
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX      FROM    WSS-CNT-FAC-GB-E
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CB-E
                                 END-IF
                              END-IF
                              MOVE      1                TO  WS-DIF-TX-GRATIS-E
                           END-IF

                           IF CON-GRATIS1 > COM-NUM-TXGRT-B AND GIRO-FLG-B-NB = 'N' THEN
                              IF GIRO-FLG-GC = 'G' THEN
                                 ADD 1                  TO CNT-GIROS-N-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-GNB-E
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-GIROS-N-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-CNB-E
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX1 < 1 AND WS-DIF-TX-GRATIS1-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                 SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-GIROS-N-BAE-E
                                 IF GIRO-FLG-GC = 'G' THEN
                                    SUBTRACT WS-TOT-DIF-TX1     FROM    WSS-CNT-FAC-GNB-E
                                 ELSE
                                    SUBTRACT    1               FROM    WSS-CNT-FAC-CNB-E
                                 END-IF
                              END-IF
                              MOVE      1                 TO  WS-DIF-TX-GRATIS1-E
                           END-IF
                        END-IF
                        PERFORM LEE-GIRCAJAUT
                END-PERFORM
        END-IF.

PROCESA-MODELO4.
*---------------
*INI LGA (ALPHATEC) 01/12/2008
        MOVE  0  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

        COMPUTE WS-VAL-CONS = 1
        MOVE 0 TO  CON-GRATIS
        MOVE 0 TO  CON-GRATIS1
        MOVE 0 TO  WS-DIF-TX-GRATIS
        MOVE 0 TO  WS-DIF-TX-GRATIS1
* INI VLS 16-03-2007
        MOVE 0 TO  WS-DIF-TX-GRATIS-E
        MOVE 0 TO  WS-DIF-TX-GRATIS1-E
* FIN VLS 16-03-2007
        MOVE 1 TO  WS-TOT-DIF-TX WS-TOT-DIF-TX1
        IF ENT-NRO-CUENTA = GIRO-CUENTA THEN
                PERFORM LEE-GIRCAJAUT
                PERFORM UNTIL FIN-GIRCAJAUT = 1
                              OR ENT-NRO-CUENTA NOT= GIRO-CUENTA

*INI LGA (ALPHATEC) 01/12/2008
                        ADD  1  TO  WS-TRM-TOT-TX
*FIN LGA (ALPHATEC) 01/12/2008

                        IF GIRO-FLG-GC = 'G' THEN
                           ADD 1                TO CON-GRATIS
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-GB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-GNB
                           END-IF
                        ELSE
                           ADD WS-VAL-CONS      TO CON-GRATIS1
                           IF GIRO-FLG-B-NB = 'S' THEN
                              ADD 1             TO WSS-CNT-TOT-CB
                           ELSE
                              ADD 1             TO WSS-CNT-TOT-CNB
                           END-IF
                        END-IF

* INI VLS 16-03-2007
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP = COM-NUM-TXGRT + COM-NUM-TXGRT-E
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP-NB = COM-NUM-TXGRT-C + COM-NUM-TXGRT-E-C
                        IF (CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND COM-FLG-TXGRT = 'N') OR
                           (CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND COM-FLG-TXGRT-C = 'N') THEN
*--> Tx. que paga el cliente
*--> Calcula diferencia bae
                           IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND WS-DIF-TX-GRATIS = 0 AND WS-TOTAL-TXGRT-BCO-EMP > 0 THEN
                              COMPUTE WS-TOT-DIF-TX  = CON-GRATIS - WS-TOTAL-TXGRT-BCO-EMP
                           END-IF
*--> Calcula diferencia nobae
                           IF CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND WS-DIF-TX-GRATIS1 = 0 AND WS-TOTAL-TXGRT-BCO-EMP-NB > 0 THEN
                              COMPUTE WS-TOT-DIF-TX1 = CON-GRATIS1 - WS-TOTAL-TXGRT-BCO-EMP-NB
                           END-IF

                           IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP AND GIRO-FLG-GC = 'G' THEN
                              IF GIRO-FLG-B-NB = 'S' THEN
                                 ADD 1                  TO CNT-GIROS-BAE
                                 ADD 1                  TO WSS-CNT-FAC-GB
                              ELSE
                                 ADD 1                  TO CNT-GIROS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-GNB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS = 0 AND GIRO-FLG-GC = 'G' THEN
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE
                                    SUBTRACT  WS-TOT-DIF-TX FROM  WSS-CNT-FAC-GB
                                 ELSE
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-N-BAE
                                    SUBTRACT  WS-TOT-DIF-TX FROM  WSS-CNT-FAC-GNB
                                 END-IF
                              END-IF
                              MOVE      1             TO  WS-DIF-TX-GRATIS
                           END-IF

                           IF CON-GRATIS1 > WS-TOTAL-TXGRT-BCO-EMP-NB AND GIRO-FLG-GC = 'C' THEN
                              IF GIRO-FLG-B-NB = 'S' THEN
                                 ADD WS-VAL-CONS        TO CNT-CONSULTAS-BAE
                                 ADD 1                  TO WSS-CNT-FAC-CB
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-CONSULTAS-N-BAE
                                 ADD 1                  TO WSS-CNT-FAC-CNB
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX1 < 1 AND WS-DIF-TX-GRATIS1 = 0 AND GIRO-FLG-GC = 'G' THEN
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-CONSULTAS-BAE
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  WSS-CNT-FAC-CB
                                 ELSE
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-CONSULTAS-N-BAE
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  WSS-CNT-FAC-CNB
                                 END-IF
                              END-IF
                              MOVE      1             TO  WS-DIF-TX-GRATIS1
                           END-IF
                        ELSE
*--> Tx. que paga la Empresa
*--> Calcula diferencia bae
                           IF CON-GRATIS > COM-NUM-TXGRT AND WS-DIF-TX-GRATIS-E = 0 AND COM-NUM-TXGRT > 0 THEN
                              COMPUTE WS-TOT-DIF-TX  = CON-GRATIS - COM-NUM-TXGRT
                           END-IF
*--> Calcula diferencia nobae
                           IF CON-GRATIS1 > COM-NUM-TXGRT-C AND WS-DIF-TX-GRATIS1-E = 0 AND COM-NUM-TXGRT-C > 0 THEN
                              COMPUTE WS-TOT-DIF-TX1 = CON-GRATIS1 - COM-NUM-TXGRT-C
                           END-IF

                           IF CON-GRATIS > COM-NUM-TXGRT AND GIRO-FLG-GC = 'G' THEN
                              IF GIRO-FLG-B-NB = 'S' THEN
                                 ADD 1                  TO CNT-GIROS-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-GB-E
                              ELSE
                                 ADD 1                  TO CNT-GIROS-N-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-GNB-E
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX < 1 AND WS-DIF-TX-GRATIS-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-BAE-E
                                    SUBTRACT  WS-TOT-DIF-TX FROM  WSS-CNT-FAC-GB-E
                                 ELSE
                                    SUBTRACT  WS-TOT-DIF-TX FROM  CNT-GIROS-N-BAE-E
                                    SUBTRACT  WS-TOT-DIF-TX FROM  WSS-CNT-FAC-GNB-E
                                 END-IF
                              END-IF
                              MOVE      1             TO  WS-DIF-TX-GRATIS-E
                           END-IF

                           IF CON-GRATIS1 > COM-NUM-TXGRT-C AND GIRO-FLG-GC = 'C' THEN
                              IF GIRO-FLG-B-NB = 'S' THEN
                                 ADD WS-VAL-CONS        TO CNT-CONSULTAS-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-CB-E
                              ELSE
                                 ADD WS-VAL-CONS        TO CNT-CONSULTAS-N-BAE-E
                                 ADD 1                  TO WSS-CNT-FAC-CNB-E
                              END-IF
*--> Resta diferencia
                              IF WS-TOT-DIF-TX1 < 1 AND WS-DIF-TX-GRATIS1-E = 0 AND GIRO-FLG-GC = 'G' THEN
                                 IF GIRO-FLG-B-NB = 'S' THEN
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-CONSULTAS-BAE-E
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  WSS-CNT-FAC-CB-E
                                 ELSE
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  CNT-CONSULTAS-N-BAE-E
                                    SUBTRACT  WS-TOT-DIF-TX1 FROM  WSS-CNT-FAC-CNB-E
                                 END-IF
                              END-IF
                              MOVE      1             TO  WS-DIF-TX-GRATIS1-E
                           END-IF
                        END-IF
                        PERFORM LEE-GIRCAJAUT
                END-PERFORM
        END-IF.

LEE-GIRCAJAUT.
*-------------
        READ GIRCAJAUT NEXT RECORD
          AT END
             MOVE 1 TO FIN-GIRCAJAUT
          NOT AT END
             CONTINUE
        END-READ.

LEE-CONSMIC.
*-----------
        INITIALIZE REG-CONSMIC
        MOVE ENT-NRO-CUENTA  TO MIC-CONS-CUENTA
        READ  CONSMIC KEY IS MIC-CONS-CUENTA
          INVALID KEY
                CONTINUE
        END-READ.

LEE-COMISION.
*------------
        INITIALIZE REG-AHVINFO2
        MOVE 0 TO FLAG-COMISION
        MOVE ENT-PLAN         TO COM-TIPO-COM
        MOVE WS-TRCODE        TO COM-CODIGO-TRAN
        READ AHVINFO2 KEY IS COM-LLAVE
          INVALID KEY
                DISPLAY "NO EXISTE REGISTRO DE COMISION"
                MOVE 1 TO FLAG-COMISION                                 *>NARCISO SE AGREGA PARA CARGAR COMISIONES
                CONTINUE
          NOT INVALID KEY
                      MOVE 1 TO FLAG-COMISION
        END-READ.

*INI LGA (ALPHATEC) 19/05/2009
*SACA-COMISION-MANTENCION.
**------------------------
*       PERFORM SACA-NUMERO-DE-PRODUCTOS
*        PERFORM LEER-COMISION-PRODUCTO
*       IF R-AHV-COMPROD-VALOR > 0 THEN
** EL VALOR EN LA TABLA TIENE EL IVA INCLUIDO.
*               COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * R-AHV-COMPROD-VALOR
*               MOVE 'S' TO COM-FLG-IVA
*               PERFORM GRABA-SALIDA-2DO
*               ADD 1  TO  WS-CON-MANTENCION
*       END-IF.
*
*SACA-NUMERO-DE-PRODUCTOS.
**------------------------
*       MOVE ENT-NRT-CLIENT   TO WS-NRT-CLIEN
*       MOVE 0                TO WS-DIG
*       CALL "NUM_PRODUC" USING WS-NRT-CLIEN, WS-DIG, NUM-PRODUCT
*        END-CALL
*       PERFORM SUMA-PRODUCTO-LCR.
*
*SUMA-PRODUCTO-LCR.
**-----------------
*       INITIALIZE REG-RUTLCR
*       MOVE 0 TO WS-RUTLCR
*       MOVE ENT-NRT-CLIENT   TO RUTLCR-RUT
*        READ RUTLCR   KEY IS RUTLCR-RUT
*          INVALID KEY
*               MOVE 1 TO WS-RUTLCR
*        END-READ
*       IF WS-RUTLCR = 0 THEN
*               ADD 1 TO NUM-PRODUCT
*       END-IF.
*
*LEER-COMISION-PRODUCTO.
**----------------------
*       INITIALIZE R-AHV-COMPROD-REC
*       MOVE ENT-PLAN              TO R-AHV-COMPROD-TIP_COMISION
*       MOVE NUM-PRODUCT           TO R-AHV-COMPROD-NUM_PRODUCTOS
*        MOVE TOT-MTO-SLDPRM        TO R-AHV-COMPROD-SALDO_PROM_INI
*        MOVE TOT-MTO-SLDPRM        TO R-AHV-COMPROD-SALDO_PROM_FIN
*       EXEC SQL SELECT * INTO:R-AHV-COMPROD-REC
*               FROM AHV.AHV_COMPROD
*               WHERE (TIP_COMISION   =:   R-AHV-COMPROD-TIP_COMISION   AND
*                      NUM_PRODUCTOS  =:   R-AHV-COMPROD-NUM_PRODUCTOS  AND
*                      SALDO_PROM_INI <= : R-AHV-COMPROD-SALDO_PROM_INI AND
*                      SALDO_PROM_FIN >= : R-AHV-COMPROD-SALDO_PROM_FIN)
*                LIMIT TO 1 ROW
*       END-EXEC
*       PERFORM KIO-MAKE-ERROR
*       IF PV-NO-SUCCESS THEN
**            DISPLAY "NO EXISTE COMISION A COBRAR EN TABLA AHV_COMPROD "
*             ADD 1 TO WS-NO-COBRA-1000
*          ELSE
*             MOVE R-AHV-COMPROD-TIP_COMISION TO COM-TIPO-COM
*       END-IF.
*
*CALCULA-SALDO-MENSUAL.
**---------------------
*       MOVE 0      TO  TOT-MTO-SLDPRM
*       PERFORM OPEN-AHV_TRN_HIS_MES
*       IF PV-SUCCESS THEN
*          PERFORM LEER-AHV_TRN_HIS_MES
*          PERFORM UNTIL PV-NO-SUCCESS
**--> Actualiza arreglo con saldos
*               ADD    AHV_TRN_HIS_MES-MTO_TRN    TO   TOT-MTO-SLDPRM
*               PERFORM LEER-AHV_TRN_HIS_MES
*          END-PERFORM
*          PERFORM CLOSE-AHV_TRN_HIS_MES
*       END-IF.
*
*OPEN-AHV_TRN_HIS_MES.
**----------------
*
*% FOR I=1 TO 11
*       IF WS-TABLA = {I}
*          EXEC SQL OPEN AHV_TRN_HIS_MES_CURSOR{I} END-EXEC
*       ELSE
*%NEXT I
*          EXEC SQL OPEN AHV_TRN_HIS_MES_CURSOR12 END-EXEC
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF.
*
*       PERFORM KIO-MAKE-ERROR.
*
*CLOSE-AHV_TRN_HIS_MES.
**-----------------
*
*% FOR I=1 TO 11
*       IF WS-TABLA = {I}
*          EXEC SQL CLOSE AHV_TRN_HIS_MES_CURSOR{I} END-EXEC
*       ELSE
*%NEXT I
*          EXEC SQL CLOSE AHV_TRN_HIS_MES_CURSOR12 END-EXEC
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF.
*
*LEER-AHV_TRN_HIS_MES.
**----------------
*
*
*% FOR I=1 TO 11
*       IF WS-TABLA = {I}
*          EXEC SQL FETCH AHV_TRN_HIS_MES_CURSOR{I} INTO :AHV_TRN_HIS_MES-REC   END-EXEC
*       ELSE
*%NEXT I
*          EXEC SQL FETCH AHV_TRN_HIS_MES_CURSOR12 INTO :AHV_TRN_HIS_MES-REC    END-EXEC
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF
*       END-IF.
*       PERFORM KIO-MAKE-ERROR.
*FIN LGA (ALPHATEC) 19/05/2009

GRABA-SALIDA-APER.
*----------------*
        MOVE MONTO-COMISION         TO MTO-COMIS-TUNC.
        INITIALIZE REG-SALIDA R-AHV-PND-REC.
        MOVE 23590000               TO  R-AHV-PND-HRS-TRN.
        MOVE 1001                   TO  COM-TRN         R-AHV-PND-COD-TRN.
        MOVE R-AHV-APERTURA-KEY     TO  COM-KEY         R-AHV-PND-KEY.
        MOVE 'C'                    TO  COM-TIPOPER     R-AHV-PND-ABO-CAR.
        MOVE MTO-COMIS-TUNC         TO  COM-MONTO       R-AHV-PND-MTO-TRN.
        MOVE FECHA-AMD-R            TO  COM-FECHA-COB   R-AHV-PND-FEC-TRN.
        MOVE R-AHV-APERTURA-FLG-IVA   TO  COM-FLAG-IVA  R-AHV-PND-FLG-EST.
        MOVE ZEROES                 TO  COM-FLG-PAGA    R-AHV-PND-SUC-ORI.
*       MOVE ZEROES                 TO  COM-DIAS-PAGO   R-AHV-PND-COD_OPE.
        MOVE ZEROES                 TO  COM-DIAS-PAGO.
        MOVE 99                     TO R-AHV-PND-COD-OPE.
* Ini Servinfor
        MOVE R-AHV-APERTURA-TIP-COM TO  COM-CODEMP.
        MOVE ZEROES                 TO  R-AHV-PND-SLD-MOV
                                        R-AHV-PND-FEC-EST.
* Fin Servinfor
        MOVE R-AHV-APERTURA-TIP-COM TO  ENT-PLAN
        MOVE 1001                   TO  WS-TRCODE
        PERFORM LEE-COMISION


        WRITE REG-SALIDA.
        IF COM-TIPAG = "C"  THEN
                PERFORM INSERT-PND
        END-IF.
        ADD 1   TO   WS-TOT-GRABADOS.

INSERT-PND.
*----------
           EXEC SQL INSERT INTO AHV.AHV_PND
      *>          (R-AHV-PND-KEY
      *>          ,R-AHV-PND-FEC-TRN
      *>          ,R-AHV-PND-HRS-TRN
      *>          ,R-AHV-PND-COD-TRN
      *>          ,R-AHV-PND-NRO-DOC
      *>          ,R-AHV-PND-MTO-TRN
      *>          ,R-AHV-PND-ABO-CAR
      *>          ,R-AHV-PND-FLG-EST
      *>          ,R-AHV-PND-FEC-EST
      *>          ,R-AHV-PND-SUC-ORI
      *>          ,R-AHV-PND-COD-OPE
      *>          ,R-AHV-PND-SLD-MOV
                 VALUES (:R-AHV-PND-REC)
           END-EXEC.
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
                DISPLAY "PROBLEMAS AL INSERTAR REGISTRO EN AHV-PND", R-AHV-PND-KEY CONVERSION
        END-IF.

GRABA-SALIDA.
*-----------*
        MOVE MONTO-COMISION   TO MTO-COMIS-TUNC.
        INITIALIZE REG-SALIDA R-AHV-PND-REC.
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE WS-TRCODE          TO      COM-TRN          R-AHV-PND-COD-TRN.
        MOVE ENT-NRO-CUENTA     TO      COM-KEY          R-AHV-PND-KEY.
        MOVE 'C'                TO      COM-TIPOPER      R-AHV-PND-ABO-CAR.
        MOVE MTO-COMIS-TUNC     TO      COM-MONTO        R-AHV-PND-MTO-TRN.
        MOVE FECHA-AMD-R        TO      COM-FECHA-COB    R-AHV-PND-FEC-TRN.
        MOVE COM-FLG-IVA        TO      COM-FLAG-IVA     R-AHV-PND-FLG-EST.
        MOVE ENT-FLG-PAGA       TO      COM-FLG-PAGA     R-AHV-PND-SUC-ORI.
*       MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO    R-AHV-PND-COD_OPE.
        MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO.
        MOVE 99                 TO      R-AHV-PND-COD-OPE.
* Ini Servinfor
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE ZEROES             TO      R-AHV-PND-SLD-MOV
                                        R-AHV-PND-FEC-EST.
* Fin Servinfor

        WRITE REG-SALIDA.
        IF COM-TIPAG-A = "C" OR COM-TIPAG = "C" THEN                    *> aca lo hago saltar adentro
           IF WS-TRCODE = 1111 THEN
                PERFORM VALIDA-EXISTENCIA-PND
           ELSE
                PERFORM INSERT-PND
           END-IF
        END-IF.
        ADD 1   TO   WS-TOT-GRABADOS.

VALIDA-EXISTENCIA-PND.
*---------------------
        MOVE ZEROES     TO WS-CNT-MOV1111
           EXEC SQL SELECT COUNT(*) INTO : WS-CNT-MOV1111
                FROM AHV.AHV_PND
                WHERE KEY       =: R-AHV-PND-KEY AND
                      COD-TRN   =: WS-TRCODE
                LIMIT TO 1 ROW
           END-EXEC.
        IF WS-CNT-MOV1111 = 0 THEN
                PERFORM INSERT-PND
                ADD 1 TO WS-CNT-INS1111
        ELSE
                ADD 1 TO WS-CNT-MOD1111
                EXEC SQL UPDATE AHV.AHV_PND SET
                        FEC-TRN =: R-AHV-PND-FEC-TRN,
                        MTO-TRN =: R-AHV-PND-MTO-TRN,
                        COD-OPE = 99
                        WHERE KEY       =: R-AHV-PND-KEY AND
                              COD-TRN   =: WS-TRCODE
                END-EXEC
                PERFORM KIO-MAKE-ERROR
                IF PV-NO-SUCCESS THEN
                   DISPLAY "PROBLEMAS AL INSERTAR REGISTRO EN AHV-PND", R-AHV-PND-KEY CONVERSION
                END-IF
        END-IF.

* INI VLS ACCESSO 03-02-2004
GRABA-DETCOMATM.
*---------------
        INITIALIZE REG-DETCOMATM
        MOVE ENT-NRO-CUENTA     TO      DET-KEY
        MOVE ENT-PLAN           TO      DET-PLAN
        MOVE ENT-CONVENIO       TO      DET-CONVENIO
        MOVE COM-TIPAG          TO      DET-MARCA-PAGO
        MOVE SPACES             TO      DET-BANCA
        EVALUATE ENT-MODELO
           WHEN 1       MOVE COM-MONTO-COM      TO DET-MTO-FAC-GB
                        MOVE COM-MONTO-COM      TO DET-MTO-FAC-GNB
                        COMPUTE DET-MTO-FAC-CB  = COM-MONTO-COM / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB = COM-MONTO-COM / ENT-RELACION
                        MOVE COM-NUM-TXGRT      TO DET-TX-GRATIS
* INI VLS 16-03-2007
                        MOVE COM-MONTO-COM-E    TO DET-MTO-FAC-GB-E
                        MOVE COM-MONTO-COM-E    TO DET-MTO-FAC-GNB-E
                        COMPUTE DET-MTO-FAC-CB-E  = COM-MONTO-COM-E / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB-E = COM-MONTO-COM-E / ENT-RELACION
                        MOVE COM-NUM-TXGRT-E    TO DET-TX-GRATIS-E
* FIN VLS 16-03-2007
           WHEN 2       MOVE COM-MONTO-COM      TO DET-MTO-FAC-GB
                        MOVE COM-MONTO-COM-B    TO DET-MTO-FAC-GNB
                        COMPUTE DET-MTO-FAC-CB  = COM-MONTO-COM / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB = COM-MONTO-COM-B / ENT-RELACION
                        MOVE COM-NUM-TXGRT      TO DET-TX-GRATIS
* INI VLS 16-03-2007
                        MOVE COM-MONTO-COM-E    TO DET-MTO-FAC-GB-E
                        MOVE COM-MONTO-COM-E-B  TO DET-MTO-FAC-GNB-E
                        COMPUTE DET-MTO-FAC-CB-E  = COM-MONTO-COM-E / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB-E = COM-MONTO-COM-E-B / ENT-RELACION
                        MOVE COM-NUM-TXGRT-E    TO DET-TX-GRATIS-E
* FIN VLS 16-03-2007
           WHEN 3       MOVE COM-MONTO-COM      TO DET-MTO-FAC-GB
                        MOVE COM-MONTO-COM-B    TO DET-MTO-FAC-GNB
                        COMPUTE DET-MTO-FAC-CB  = COM-MONTO-COM / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB = COM-MONTO-COM-B / ENT-RELACION
                        MOVE COM-NUM-TXGRT      TO DET-TX-B-GRATIS
                        MOVE COM-NUM-TXGRT-B    TO DET-TX-NB-GRATIS
* INI VLS 16-03-2007
                        MOVE COM-MONTO-COM-E      TO DET-MTO-FAC-GB-E
                        MOVE COM-MONTO-COM-E-B    TO DET-MTO-FAC-GNB-E
                        COMPUTE DET-MTO-FAC-CB-E  = COM-MONTO-COM-E / ENT-RELACION
                        COMPUTE DET-MTO-FAC-CNB-E = COM-MONTO-COM-E-B / ENT-RELACION
                        MOVE COM-NUM-TXGRT-E    TO DET-TX-B-GRATIS-E
                        MOVE COM-NUM-TXGRT-E-B  TO DET-TX-NB-GRATIS-E
* FIN VLS 16-03-2007
           WHEN 4       MOVE COM-MONTO-COM      TO DET-MTO-FAC-GB
                        MOVE COM-MONTO-COM-B    TO DET-MTO-FAC-GNB
                        MOVE COM-MONTO-COM-C    TO DET-MTO-FAC-CB
                        MOVE COM-MONTO-COM-D    TO DET-MTO-FAC-CNB
                        MOVE COM-NUM-TXGRT      TO DET-GIROS-GRATIS
                        MOVE COM-NUM-TXGRT-C    TO DET-CONSU-GRATIS
* INI VLS 16-03-2007
                        MOVE COM-MONTO-COM-E    TO DET-MTO-FAC-GB-E
                        MOVE COM-MONTO-COM-E-B  TO DET-MTO-FAC-GNB-E
                        MOVE COM-MONTO-COM-E-C  TO DET-MTO-FAC-CB-E
                        MOVE COM-MONTO-COM-E-D  TO DET-MTO-FAC-CNB-E
                        MOVE COM-NUM-TXGRT-E    TO DET-GIROS-GRATIS-E
                        MOVE COM-NUM-TXGRT-E-C  TO DET-CONSU-GRATIS-E
* FIN VLS 16-03-2007
        END-EVALUATE
        MOVE WSS-CNT-TOT-GB     TO DET-CNT-TOT-GB
        MOVE WSS-CNT-TOT-GNB    TO DET-CNT-TOT-GNB
        MOVE WSS-CNT-TOT-CB     TO DET-CNT-TOT-CB
        MOVE WSS-CNT-TOT-CNB    TO DET-CNT-TOT-CNB
        MOVE WSS-CNT-FAC-GB     TO DET-CNT-FAC-GB
        MOVE WSS-CNT-FAC-GNB    TO DET-CNT-FAC-GNB
        MOVE WSS-CNT-FAC-CB     TO DET-CNT-FAC-CB
        MOVE WSS-CNT-FAC-CNB    TO DET-CNT-FAC-CNB
        MOVE MONTO-COMISION     TO MTO-COMIS-TUNC
        MOVE MTO-COMIS-TUNC     TO DET-MTO-TOT-FAC
        MOVE UF-DEL-DIA         TO DET-UF-FACTURACION
* INI VLS 16-03-2007
        MOVE WSS-CNT-FAC-GB-E   TO DET-CNT-FAC-GB-E
        MOVE WSS-CNT-FAC-GNB-E  TO DET-CNT-FAC-GNB-E
        MOVE WSS-CNT-FAC-CB-E   TO DET-CNT-FAC-CB-E
        MOVE WSS-CNT-FAC-CNB-E  TO DET-CNT-FAC-CNB-E
        MOVE WS-MONTO-EMPRESA   TO DET-MTO-TOT-FAC-E
* FIN VLS 16-03-2007
        ADD 1                   TO WS-GRABADOS-DET

        WRITE REG-DETCOMATM.
* FIN VLS ACCESSO 03-02-2004

GRABA-SALIDA-2DO.
*---------------*
        MOVE MONTO-COMISION   TO MTO-COMIS-TUNC.
        INITIALIZE REG-SALIDA R-AHV-PND-REC.
        MOVE WS-TRCODE                  TO      COM-TRN          R-AHV-PND-COD-TRN.
        MOVE ENT-NRO-CUENTA             TO      COM-KEY          R-AHV-PND-KEY.
        MOVE 'C'                        TO      COM-TIPOPER      R-AHV-PND-ABO-CAR.
        MOVE MTO-COMIS-TUNC             TO      COM-MONTO        R-AHV-PND-MTO-TRN.
        MOVE FECHA-AMD-R                TO      COM-FECHA-COB    R-AHV-PND-FEC-TRN.
        MOVE COM-FLG-IVA                TO      COM-FLAG-IVA     R-AHV-PND-FLG-EST.
        MOVE ZEROES                     TO      COM-FLG-PAGA     R-AHV-PND-SUC-ORI.
*       MOVE ZEROES                     TO      COM-DIAS-PAGO    R-AHV-PND-COD_OPE.
        MOVE ZEROES                     TO      COM-DIAS-PAGO.
        MOVE 99                         TO      R-AHV-PND-COD-OPE.
*INI LGA (ALPHATEC) 19/05/2009
*       MOVE R-AHV-COMPROD-TIP_COMISION TO      COM-CODEMP.
        MOVE ENT-PLAN                   TO      COM-CODEMP.
*FIN LGA (ALPHATEC) 19/05/2009
        MOVE ZEROES                     TO      R-AHV-PND-SLD-MOV
                                                R-AHV-PND-FEC-EST.
        MOVE 1000                       TO WS-TRCODE
        PERFORM LEE-COMISION

        WRITE REG-SALIDA.
        IF COM-TIPAG = "C"  THEN
                PERFORM INSERT-PND
        ELSE
                MOVE MTO-COMIS-TUNC     TO WS-MONTO-EMPRESA
                PERFORM GRABA-COMCLIEM
        END-IF
        ADD 1   TO   WS-TOT-GRABADOS.

GRABA-COMCLIEM.
*--------------
        INITIALIZE REG-COMCLIEM
        MOVE  ENT-CONVENIO      TO  EMP-AHV-CONVENIO
        MOVE  ENT-NRO-CUENTA    TO  EMP-AHV-KEY
        MOVE  WS-MONTO-EMPRESA  TO  EMP-AHV-MONTO
        MOVE  COM-TIPAG         TO  EMP-AHV-CARGO
        MOVE  FECHA-AMD-R       TO  EMP-AHV-FECHA
        MOVE  WS-TRCODE         TO  EMP-TRCODE
        WRITE REG-COMCLIEM END-WRITE
        ADD 1   TO  WS-GRABADOS-COMCLIEM.

TERMINO.
*------*
        PERFORM ACTUALIZA-FEC-CTAMAS.
        DISPLAY "                      E S T A D I S T I C A".
        DISPLAY "--------------------------------------------------------------------------- ".
        DISPLAY " ".
        DISPLAY "LEIDOS PARA COMISION APERTURA CTA MAS ----------------> " WS-CON-APERTURA    CONVERSION.
        DISPLAY "ELIMINADOS DE TABLA AHV-APERTURA-CTD -----------------> " WS-ELIM-APERTURA   CONVERSION.
        DISPLAY "ACTUALIZADOS DE TABLA AHV-APERTURA-CTD ---------------> " WS-UPD-APERTURA   CONVERSION.
        DISPLAY " ".
        DISPLAY "LEIDOS COMISION MANTENCION 2do ANO CTA MAS -----------> " WS-CON-MANTENCION  CONVERSION.
        DISPLAY "LEIDOS COMISION MANTENCION 2do ANO SIN PARAMETROS ----> " WS-NO-COBRA-1000   CONVERSION.
        DISPLAY " ".
        DISPLAY "LEIDOS COMISION ADMINISTRACIÓN ANUAL -----------------> " WS-CON-ADM-ANUAL   CONVERSION.
        DISPLAY "INSERTADOS POR  ADMINISTRACIÓN ANUAL -----------------> " WS-CNT-INS1111     CONVERSION.
        DISPLAY "ACTUALIZADOS POR ADMINISTRACIÓN ANUAL ----------------> " WS-CNT-MOD1111     CONVERSION.
        DISPLAY " ".
        DISPLAY "LEIDOS PARA COMISION TX ADIC CAJ UTOM. CTA MAS  ------> " WS-CON-TXADICI     CONVERSION.
        DISPLAY "LEIDOS PARA COMISION POR CONS. ADIC. MICROMATICO -----> " WS-CON-CONSMIC     CONVERSION.
        DISPLAY " ".
        DISPLAY "TOTAL COMISIONES COBRADAS POR USO CAJA-CHILE ---------> "  WS-TOT-GRABADOS-CC.
        DISPLAY " ".
        DISPLAY "TOTAL REGISTROS GRABADOS PARA COBRO DE COMISIONES ----> "  WS-TOT-GRABADOS.
        DISPLAY "TOTAL REGISTROS GRABADOS A DETALLE DE COMISIONES -----> "  WS-GRABADOS-DET.
        CLOSE AHVINFO SALIDA GIRCAJAUT.
        CLOSE CONSMIC RUTLCR AHVINFO2.
* INI VLS ACCESSO 03-02-2004
        CLOSE DETCOMATM COMCLIEM
* FIN VLS ACCESSO 03-02-2004
        PERFORM KIO-COMMIT.
        CALL "LIB$SHOW_TIMER"
                on overflow
                   continue
        end-call.

ACTUALIZA-FEC-CTAMAS.
*-------------------
        MOVE '5'  TO WS-STATUS
           EXEC SQL UPDATE BCT.FECSCTAMAS
                SET   STATUS =:WS-STATUS
                WHERE FECHA  =:FECHA-PROC1 AND
                      TIPO   = 1           AND
                      STATUS = '4'
           END-EXEC.
        PERFORM KIO-MAKE-ERROR.
        IF PV-NO-SUCCESS THEN
                DISPLAY "ERROR AL ACTUALIZAR STATUS EN FEC-CTAMAS"
        ELSE
                DISPLAY "STATUS ACTUALIZADO A : ", WS-STATUS
        END-IF.

KIO-START-TRANSACTION.
*---------------------
        SET PV-SUCCESS TO TRUE.
        MOVE SPACES TO K-IO-RECORD.
        MOVE "START-TX" TO K-IO-OPERATION.
*
* Here we tell Sql which tables we want to play with
*
*       EXEC SQL SET TRANSACTION                                                *> TRANSACTION
*               ON BCT USING (READ WRITE                                        *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       BCT.FEC                 FOR SHARED READ,                *> TRANSACTION
*                       BCT.BCT$VAL             FOR SHARED READ,                *> TRANSACTION
*INI LGA (ALPHATEC) 01/12/2008                                                  *> TRANSACTION
*                       BCT.EMP                 FOR SHARED READ,                *> TRANSACTION
*FIN LGA (ALPHATEC) 01/12/2008                                                  *> TRANSACTION
*                       BCT.FEC$CTAMAS          FOR SHARED WRITE)               *> TRANSACTION
*               AND                                                             *> TRANSACTION
*               ON AHV USING  (READ WRITE                                       *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       AHV.AHV_APERTURA_CTD     FOR SHARED WRITE,              *> TRANSACTION
*                       AHV.AHV_PND              FOR SHARED WRITE,              *> TRANSACTION
*                       AHV.FACXTRM              FOR SHARED WRITE,              *> TRANSACTION
*                       AHV.PLN_ASOC             FOR SHARED READ,               *> TRANSACTION
*                       AHV.PLN_TRM              FOR SHARED READ,               *> TRANSACTION
*                       AHV.TRM_ATM              FOR SHARED READ,               *> TRANSACTION
*INI LGA (ALPHATEC) 19/05/2009                                                  *> TRANSACTION
*                       AHV.COB_SEG              FOR SHARED WRITE,              *> TRANSACTION
*                       AHV.COM_SEG              FOR SHARED READ,               *> TRANSACTION
*                       AHV.COM_SERV             FOR SHARED READ,               *> TRANSACTION
*                       AHV.CTA_SEG              FOR SHARED READ,               *> TRANSACTION
*                       AHV.CTA_SERV             FOR SHARED READ,               *> TRANSACTION
*FIN LGA (ALPHATEC) 19/05/2009                                                  *> TRANSACTION
*                       AHV.AHV_COMPROD          FOR SHARED READ)               *> TRANSACTION
*               AND                                                             *> TRANSACTION
*               ON AHVHIS USING  (READ ONLY                                     *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       AHVHIS.AHV_PND_HIS               FOR SHARED READ,       *> TRANSACTION
* FOR I=1 TO 12                                                                 *> TRANSACTION
*                       AHVHIS.AHV_TRN_HIS{I}            FOR SHARED READ,       *> TRANSACTION
*NEXT I                                                                         *> TRANSACTION
*                       AHVHIS.AHVHIS_PERIODOS           FOR SHARED READ)       *> TRANSACTION
*               AND                                                             *> TRANSACTION
*               ON AHP USING  (READ ONLY                                        *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       AHP.AHP_CUENTA           FOR SHARED READ)               *> TRANSACTION
*               AND                                                             *> TRANSACTION
*               ON CRD USING  (READ ONLY                                        *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       CRD.CRE                  FOR SHARED READ)               *> TRANSACTION
*               AND                                                             *> TRANSACTION
*               ON TCR USING  (READ ONLY                                        *> TRANSACTION
*               RESERVING                                                       *> TRANSACTION
*                       TCR.TITCR                FOR SHARED READ)               *> TRANSACTION
*       END-EXEC.                                                               *> TRANSACTION

KIO-COMMIT.
*----------
        MOVE SPACES TO K-IO-RECORD.
        MOVE "COMMIT" TO K-IO-OPERATION.
*
* Commit all database changes
*
           EXEC SQL COMMIT END-EXEC.
        PERFORM KIO-MAKE-ERROR.

KIO-ROLLBACK.
*------------
        MOVE SPACES TO K-IO-RECORD.
        MOVE "ROLLBACK" TO K-IO-OPERATION.
*
* Undo the whole thing
*
           EXEC SQL ROLLBACK END-EXEC.
        PERFORM KIO-MAKE-ERROR.

** Ini Servinfor Ltda

TERM.
*----*
TE-B.
*>INCLUDE PD_RUT OF "LBR:COMMON_RUT.LBR"
*>INCLUDE IO_PD  OF "GEN:RECAHV.GLB"


TE-EX.
        EXIT.
* Fin Servinfor Ltda


* INI VLS 16-03-2007
START-GIROS-CC.
*--------------
        INITIALIZE REG-GIROS-CC WS-NO-GIROS-CC WS-CC-CUENTA
        MOVE ENT-NRO-CUENTA  TO GIRO-CC-CUENTA
        MOVE 0               TO GIRO-CC-FECHA
        START GIROS-CC KEY IS >= GIRO-CC-K
          INVALID KEY
             MOVE 1    TO  WS-NO-GIROS-CC
          NOT INVALID KEY
             MOVE GIRO-CC-CUENTA        TO WS-CC-CUENTA
             PERFORM PROCESA-ARCHIVO-CC
        END-START.

START-CONSU-CC.
*--------------
        INITIALIZE REG-CONSU-CC WS-NO-CONSU-CC WS-CC-CUENTA
        MOVE ENT-NRO-CUENTA  TO CONS-CC-CUENTA
        MOVE 0               TO CONS-CC-FECHA
        START CONSU-CC KEY IS >= CONS-CC-K
          INVALID KEY
             MOVE 1    TO  WS-NO-CONSU-CC
          NOT INVALID KEY
             MOVE CONS-CC-CUENTA        TO WS-CC-CUENTA
             PERFORM PROCESA-ARCHIVO-CC
        END-START.

START-DEPOS-CC.
*--------------
        INITIALIZE REG-DEPOS-CC WS-NO-DEPOS-CC WS-CC-CUENTA
        MOVE ENT-NRO-CUENTA  TO DEPO-CC-CUENTA
        MOVE 0               TO DEPO-CC-FECHA
        START DEPOS-CC KEY IS >= DEPO-CC-K
          INVALID KEY
             MOVE 1    TO  WS-NO-DEPOS-CC
          NOT INVALID KEY
             MOVE DEPO-CC-CUENTA        TO WS-CC-CUENTA
             PERFORM PROCESA-ARCHIVO-CC
        END-START.

PROCESA-ARCHIVO-CC.
*------------------
        MOVE 0 TO  CON-GRATIS
        IF ENT-NRO-CUENTA = WS-CC-CUENTA THEN
                EVALUATE WS-TRCODE
                   WHEN 1013
                      PERFORM LEE-GIRO-CC
                   WHEN 1014
                      PERFORM LEE-CONS-CC
                   WHEN 1015
                      PERFORM LEE-DEPO-CC
                END-EVALUATE
                PERFORM UNTIL FIN-ARCHIVO-CC = 1
                              OR ENT-NRO-CUENTA NOT= WS-CC-CUENTA

                        ADD 1           TO CON-GRATIS
* INI VLS 16-03-2007
                        COMPUTE WS-TOTAL-TXGRT-BCO-EMP = COM-NUM-TXGRT + COM-NUM-TXGRT-E
                        IF CON-GRATIS > COM-NUM-TXGRT AND CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP THEN
*--> Tx. que paga el cliente
                           IF CON-GRATIS > WS-TOTAL-TXGRT-BCO-EMP THEN
                              ADD 1             TO CNT-TX-CC
                           END-IF
                        ELSE
                           IF CON-GRATIS > COM-NUM-TXGRT THEN
*--> Tx. que paga la empresa
                              ADD 1             TO CNT-TX-CC-E
                           END-IF
                        END-IF

                        EVALUATE WS-TRCODE
                           WHEN 1013
                              PERFORM LEE-GIRO-CC
                           WHEN 1014
                              PERFORM LEE-CONS-CC
                           WHEN 1015
                              PERFORM LEE-DEPO-CC
                        END-EVALUATE
                END-PERFORM
        END-IF.

LEE-GIRO-CC.
*-----------
        READ GIROS-CC NEXT RECORD
          AT END
             MOVE 1 TO FIN-ARCHIVO-CC
          NOT AT END
             MOVE GIRO-CC-CUENTA        TO WS-CC-CUENTA
        END-READ.

LEE-CONS-CC.
*-----------
        READ CONSU-CC NEXT RECORD
          AT END
             MOVE 1 TO FIN-ARCHIVO-CC
          NOT AT END
             MOVE CONS-CC-CUENTA        TO WS-CC-CUENTA
        END-READ.

LEE-DEPO-CC.
*-----------
        READ DEPOS-CC NEXT RECORD
          AT END
             MOVE 1 TO FIN-ARCHIVO-CC
          NOT AT END
             MOVE DEPO-CC-CUENTA        TO WS-CC-CUENTA
        END-READ.

CALCULA-COMISION-CC.
*-------------------
* Si cliente tiene transacciones, calcula monto de la comision que pagara la empresa.
        MOVE ZEROES     TO  MONTO-COMISION-E
        IF CNT-TX-CC-E > 0 THEN
           IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION-E ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM-E) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION-E ROUNDED = MONTO-COMISION-E * CNT-TX-CC-E
           ELSE
                    COMPUTE MONTO-COMISION-E ROUNDED = UF-DEL-DIA * COM-MONTO-COM-E
                    COMPUTE MONTO-COMISION-E ROUNDED = MONTO-COMISION-E * CNT-TX-CC-E
           END-IF
        END-IF
        IF MONTO-COMISION-E > 0 THEN
           MOVE MONTO-COMISION-E        TO WS-MONTO-EMPRESA
           PERFORM GRABA-COMCLIEM
           ADD 1                        TO WS-TOT-GRABADOS-CC
        END-IF

* Si cliente tiene Transacciones adicionales, calcula monto de la comision que pagará el cliente.
        MOVE ZEROES     TO  MONTO-COMISION
        IF CNT-TX-CC > 0 THEN
           IF COM-FLG-IVA = 'S' THEN
                    COMPUTE MONTO-COMISION ROUNDED = ((UF-DEL-DIA * COM-MONTO-COM) * WS-FLAG-IVA)
                    COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * CNT-TX-CC
           ELSE
                    COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * COM-MONTO-COM
                    COMPUTE MONTO-COMISION ROUNDED = MONTO-COMISION * CNT-TX-CC
           END-IF
        END-IF

        IF MONTO-COMISION > 0 THEN
           ADD 1        TO  WS-CON-TXADICI
           PERFORM GRABA-SALIDA-CC
        END-IF.

GRABA-SALIDA-CC.
*-----------------*
        MOVE MONTO-COMISION   TO MTO-COMIS-TUNC.
        INITIALIZE REG-SALIDA R-AHV-PND-REC.
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE WS-TRCODE          TO      COM-TRN          R-AHV-PND-COD-TRN.
        MOVE ENT-NRO-CUENTA     TO      COM-KEY          R-AHV-PND-KEY.
        MOVE 'C'                TO      COM-TIPOPER      R-AHV-PND-ABO-CAR.
        MOVE MTO-COMIS-TUNC     TO      COM-MONTO        R-AHV-PND-MTO-TRN.
        MOVE FECHA-AMD-R        TO      COM-FECHA-COB    R-AHV-PND-FEC-TRN.
        MOVE COM-FLG-IVA        TO      COM-FLAG-IVA     R-AHV-PND-FLG-EST.
        MOVE ENT-FLG-PAGA       TO      COM-FLG-PAGA     R-AHV-PND-SUC-ORI.
*       MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO    R-AHV-PND-COD_OPE.
        MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO.
        MOVE 99                 TO      R-AHV-PND-COD-OPE.
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE ZEROES             TO      R-AHV-PND-SLD-MOV
                                        R-AHV-PND-FEC-EST.

        WRITE REG-SALIDA.
        PERFORM INSERT-PND
        ADD 1   TO   WS-TOT-GRABADOS.
        ADD 1   TO   WS-TOT-GRABADOS-CC.

GRABA-SALIDA-1002.
*-----------------*
        MOVE MONTO-COMISION   TO MTO-COMIS-TUNC.
        INITIALIZE REG-SALIDA R-AHV-PND-REC.
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE WS-TRCODE          TO      COM-TRN          R-AHV-PND-COD-TRN.
        MOVE ENT-NRO-CUENTA     TO      COM-KEY          R-AHV-PND-KEY.
        MOVE 'C'                TO      COM-TIPOPER      R-AHV-PND-ABO-CAR.
        MOVE MTO-COMIS-TUNC     TO      COM-MONTO        R-AHV-PND-MTO-TRN.
        MOVE FECHA-AMD-R        TO      COM-FECHA-COB    R-AHV-PND-FEC-TRN.
        MOVE COM-FLG-IVA        TO      COM-FLAG-IVA     R-AHV-PND-FLG-EST.
        MOVE ENT-FLG-PAGA       TO      COM-FLG-PAGA     R-AHV-PND-SUC-ORI.
*       MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO    R-AHV-PND-COD_OPE.
        MOVE ENT-DIAS-PAGO      TO      COM-DIAS-PAGO.
        MOVE 99                 TO      R-AHV-PND-COD-OPE.
        MOVE ENT-PLAN           TO      COM-CODEMP.
        MOVE ZEROES             TO      R-AHV-PND-SLD-MOV
                                        R-AHV-PND-FEC-EST.

        WRITE REG-SALIDA.
        PERFORM INSERT-PND
        ADD 1   TO   WS-TOT-GRABADOS.
* FIN VLS 16-03-2007

*INI LGA (ALPHATEC) 01/12/2008
LEER-BCT-EMP.
*------------
           EXEC SQL
                SELECT  FECHA-APER
                INTO    :EMP-FECHA-APER
                FROM    BCT.EMP
                WHERE   EMPRESA-NUM = :ENT-CONVENIO
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
           DISPLAY " "
           DISPLAY "Convenio de Chequera NO existe... KEY --> ", ENT-NRO-CUENTA
           DISPLAY "Nro. Convenio --> ", ENT-CONVENIO
           DISPLAY "Se asume Calculo Transaccional..."
           SET  PV-SUCCESS    TO  TRUE
           SET  WS-NO-TRAMIF  TO  TRUE
        ELSE
           IF EMP-FECHA-APER < WS-FEC-TOPE THEN
              SET  WS-NO-TRAMIF  TO  TRUE
           ELSE
              SET  WS-SI-TRAMIF  TO  TRUE
           END-IF
        END-IF.

CALC-Y-VERIF-COMISION.
*---------------------
        SET   WS-NO-COM-TRAMIF  TO  TRUE

        MOVE  ENT-NRO-CUENTA  TO  WS-TRM-I-NRO-CUENTA
        MOVE  ENT-PLAN        TO  WS-TRM-I-NUM-PLAN
        MOVE  UF-DEL-DIA      TO  WS-TRM-I-UF-DEL-DIA
        MOVE  FECHA-AMD-R     TO  WS-TRM-I-FEC-PROC
        CALL "CALCOMTRAMIF" USING WS-TRM-PARAMS
        END-CALL
        IF WS-TRM-O-FLG-ERROR = 'S' THEN
           DISPLAY "Problemas Rutina CALCOMTRAMIF..."
           DISPLAY "Se conserva Monto Comision Transaccional"
           DISPLAY "KEY --> ", ENT-NRO-CUENTA
           DISPLAY "PLAN --> ", ENT-PLAN
           DISPLAY "CONVENIO --> ", ENT-CONVENIO
           DISPLAY " "
        ELSE
           PERFORM LEER-BCT-EMP
           IF WS-SI-TRAMIF THEN
              MOVE  WS-TRM-O-MTO-TRAMIF  TO  MONTO-COMISION
              SET   WS-SI-COM-TRAMIF     TO  TRUE
           ELSE
              IF WS-TRM-O-MTO-TRAMIF < MONTO-COMISION THEN
                 MOVE  WS-TRM-O-MTO-TRAMIF  TO  MONTO-COMISION
                 SET   WS-SI-COM-TRAMIF     TO  TRUE
              END-IF
           END-IF
        END-IF.

GRABA-FACXTRM.
*-------------
        INITIALIZE  R-FACXTRM-REC
        MOVE  ENT-NRO-CUENTA       TO  R-FACXTRM-KEY
        MOVE  FECHA-AMD-R          TO  R-FACXTRM-FEC-TRN
        MOVE  WS-TRM-O-NRO-TRAMO   TO  R-FACXTRM-NRO-TRAMO
        MOVE  WS-TRM-O-INI-TRAMO   TO  R-FACXTRM-INI-TRAMO
        MOVE  WS-TRM-O-FIN-TRAMO   TO  R-FACXTRM-FIN-TRAMO
        MOVE  WS-TRM-O-UF-TRAMIF   TO  R-FACXTRM-VAL-UNIT
        MOVE  WS-TRM-I-NUM-TRXS    TO  R-FACXTRM-NRO-TXS
        MOVE  WS-TRM-I-UF-DEL-DIA  TO  R-FACXTRM-VAL-UF
        MOVE  R-AHV-PND-MTO-TRN    TO  R-FACXTRM-MTO-COMIS
        MOVE  WS-FLG-ESTADO        TO  R-FACXTRM-FLG-EST
        MOVE   0                   TO  R-FACXTRM-PER-PROCESO

           EXEC SQL
                INSERT
                INTO    AHV.FACXTRM
                VALUES  (:R-FACXTRM-REC)
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
           DISPLAY "Problemas al insertar registro en FACXTRM", ENT-NRO-CUENTA
        END-IF.
*FIN LGA (ALPHATEC) 01/12/2008

*INI LGA (ALPHATEC) 19/05/2009
GENERA-COMIS-SEGMENTADO.
*-----------------------
        MOVE  ENT-NRO-CUENTA  TO  LNK-CCS-I-NRO-CUENTA
        CALL "AHVRUTCONCLISEG" USING LNK-CCS-PARAMS
        END-CALL
        IF LNK-CCS-O-VAL-TARIF > 0 THEN
           MOVE  1000  TO  WS-TRCODE
           COMPUTE MONTO-COMISION ROUNDED = UF-DEL-DIA * LNK-CCS-O-VAL-TARIF * WS-FLAG-IVA
           MOVE  'S'  TO  COM-FLG-IVA
           PERFORM GRABA-SALIDA-2DO
           ADD 1  TO  WS-CON-MANTENCION
           PERFORM GRABA-COB-SEG
        END-IF.

GRABA-COB-SEG.
*-------------
        INITIALIZE  COBSEG-REC
        MOVE  ENT-NRO-CUENTA  TO  COBSEG-KEY
        MOVE  WS-TRCODE       TO  COBSEG-COD-TX
        MOVE  FECHA-AMD-R     TO  COBSEG-FEC-CALC
        COMPUTE COBSEG-VAL-COMIS = LNK-CCS-O-VAL-TARIF * WS-FLAG-IVA
        MOVE  UF-DEL-DIA      TO  COBSEG-VAL-UF
        MOVE  MTO-COMIS-TUNC  TO  COBSEG-VAL-PESOS
        MOVE  ZEROES          TO  COBSEG-FEC-CART
        MOVE  ' '             TO  COBSEG-FLG-EST
           EXEC SQL
                INSERT
                INTO    AHV.COB-SEG
                VALUES  (:COBSEG-REC)
           END-EXEC
        PERFORM KIO-MAKE-ERROR
        IF PV-NO-SUCCESS THEN
           DISPLAY "Error, al grabar COB-SEG... KEY --> ", ENT-NRO-CUENTA
        END-IF.
*FIN LGA (ALPHATEC) 19/05/2009
        COPY "KIOPRO.CPY".
