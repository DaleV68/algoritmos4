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
		DISPLAY "INICIALIZAR".
	ABRIR-ARCHIVOS.
		DISPLAY "ABRIR-ARCHIVOS".
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
