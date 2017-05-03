	IDENTIFICATION DIVISION.
	PROGRAM-ID. ALGO4-TP-PARTE1.

      
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT CONS1      ASSING TO DISK
                          FILE STATUS IS FS_CONS1.	 
        SELECT CONS2      ASSING TO DISK
                          FILE STATUS IS FS_CONS2.	 
        SELECT CONS3      ASSING TO DISK
                          FILE STATUS IS FS_CONS3.	 

        SELECT CUENTAS    ASSING TO DISK
                          FILE STATUS IS FS_CTAS.	 

        SELECT ESTADOS    ASSING TO DISK
                          FILE STATUS IS FS_EST.

        SELECT MAESTRO    ASSING TO DISK
                          FILE STATUS IS FS_MAE.

      * SELECT LISTADO    ASSING TO DISK
      *                   FILE STATUS IS FS_LIST.
      
	DATA DIVISION.
	FILE SECTION.
        FD CONS1 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "cons1.dat".

        FD CONS1 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "cons2.dat".
		 
        FD CONS1 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "cons3.dat".
		 
        01 CONS.	
           03 CONS-CUIT-CONS          PIC 9(15).
           03 CONS-FECHA-ALTA         PIC X(10).
           03 CONS-FECHA-BAJA         PIC X(10).
           03 CONS-ESTADO             PIC 9(02).
           03 CONS-NOMBRE-CONSORCIO   PIC X(30).
           03 CONS-TEL                PIC X(15).
           03 CONS-DIR                PIC X(30).
      * No se si esta bien uno solo o hay que hacer uno por Archivo Cons
      
        FD CUENTAS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "cuentas.dat".

        01 CTA. 
           03 CTA-CUIT-CONS           PIC 9(15).
           03 CTA-NRO-CTA             PIC 9(08).
           03 CTA-FECHA-ALTA          PIC X(10).
           03 CTA-ENTIDAD             PIC 9(03).
           03 CTA-SUCURSAL            PIC 9(03).
      
         FD ESTADOS LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS "estados.dat".   

         01 EST.
            03 EST-ESTADO              PIC 9(02).
            03 EST-DESCRIP             PIC X(15).

         FD MAESTRO LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS "maestro.dat".

         01 MAE.
            03 MAE-CUIT-CONS           PIC 9(15).
            03 MAE-FECHA-ALTA          PIC X(10).
            03 MAE-DESCRIP-ESTADO      PIC X(15).
            03 MAE-NOMBRE-CONSORCIO    PIC X(30).
            03 MAE-TEL                 PIC X(15).
            03 MAE-DIR                 PIC X(30).
            03 MAE-NRO-CTA             PIC 9(08).
      
         FD LISTADO_BAJAS LABEL RECORD OMITTED
      
         01 LINEA                      PIC x(80).
      
	WORKING-STORAGE SECTION.
	77 VARI PICTURE 99.
        77 FS_CONS1       PIC 99. 
        77 FS_CONS2       PIC 99.  
        77 FS_CONS3       PIC 99. 
        77 FS_CTAS        PIC 99. 
        77 FS_EST         PIC 99.      
        77 FS_MAE         PIC 99. 
        77 WS_CANT_CONSORCIOS PIC 9(10).
        77 WS_CANT_BAJAS      PIC 9(10).
        77 WS_CANT_LINEAS     PIC 99.
        77 WS_NRO_HOJA        PIC 99.
        77 WS_CONT_ANIO       PIC 9(10).
      
	PROCEDURE DIVISION.
		perform INICIALIZAR.
		perform ABRIR-ARCHIVOS.
		perform GEN-TABLA-ESTADOS.
		perform LEO-CONSORCIOS.
		perform LEO-CUENTAS.
		perform IMPRIMO-ENCABEZADO.
		perform CICLO-CONSORCIO.
		perform IMPRIMO-BAJAS.
		perform MOSTRAR-ESTADISTICAS.
		perform CERRAR-ARCHIVOS.
	STOP RUN.

	INICIALIZAR.
		DISPLAY "INICIALIZAR INICIA".
                MOVE 0 TO WS_CANT_CONSORCIOS.
                MOVE 0 TO WS_CANT_BAJAS.
                MOVE 1 TO WS_NRO_HOJA.
                MOVE 0 TO WS_CONT_ANIO.
                DISPLAY "INICIALIZAR FIN".
      
	ABRIR-ARCHIVOS.
		DISPLAY "ABRIR-ARCHIVOS INICIA".
                OPEN INPUT CONS1.
                IF FS_CONS1 NOT = ZERO
                   DISPLAY "Error al abrir Archivo de Consorcios 1: " FS_CONS1
                   STOP RUN.
                OPEN INPUT CONS2.
                IF FS_CONS2 NOT = ZERO
                   DISPLAY "Error al abrir Archivo de Consorcios 2: " FS_CONS2
                   STOP RUN.
                OPEN INPUT CONS3.
                IF FS_CONS3 NOT = ZERO
                   DISPLAY "Error al abrir Archivo de Consorcios 3: " FS_CONS3
                   STOP RUN.
      
                OPEN INPUT CUENTAS.
                IF FS_CTA NOT = ZERO
                   DISPLAY "Error al abrir Archivo de Cuentas: " FS_CTA
                   STOP RUN.
      
                OPEN INPUT ESTADOS.
                IF FS_EST NOT = ZERO
                   DISPLAY "Error al abrir Archivo de Estados: " FS_EST
                   STOP RUN.
      
                OPEN OUTPUT MESTRO.
                OPEN OUTPUT LISTADO_BAJAS.
                DISPLAY "ABRIR-ARCHIVOS FIN".
      
	GEN-TABLA-ESTADOS.
		DISPLAY "GEN-TABLA-ESTADOS".
	LEO-CONSORCIOS.
		DISPLAY "LEO-CONSORCIOS".
	LEO-CUENTAS.
		DISPLAY "LEO-CUENTAS".
	IMPRIMO-ENCABEZADO.
		DISPLAY "IMPRIMO-ENCABEZADO".
	IMPRIMO-BAJAS.
		DISPLAY "IMPRIMO-BAJAS".
	CICLO-CONSORCIO.
		DISPLAY "CICLO-CONSORCIO".
	MOSTRAR-ESTADISTICAS.
		DISPLAY "MOSTRAR-ESTADISTICAS".
	CERRAR-ARCHIVOS.
		DISPLAY "CERRAR-ARCHIVOS".


----------------------------------------
