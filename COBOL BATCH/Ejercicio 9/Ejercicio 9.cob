***************************** Top of Data *****************************
       IDENTIFICATION DIVISION.                                        
      *                                                        *       
        PROGRAM-ID. PGMLIS00.                                          
      **********************************************************       
      *                                                        *       
      *  PROGRAMA LISTADOR                                     *       
      *                                                        *       
      **********************************************************       
      *      MANTENIMIENTO DE PROGRAMA                         *       
      **********************************************************       
      *  FECHA   *    DETALLE        * COD *                           
      **************************************                           
      *          *                   *     *                           
      *          *                   *     *                           
      **************************************                           
       ENVIRONMENT DIVISION.                                           
       CONFIGURATION SECTION.                                          
       SPECIAL-NAMES.                                                  
           DECIMAL-POINT IS COMMA.                                     
                                                                       
       INPUT-OUTPUT SECTION.                                           
       FILE-CONTROL.                                                   
             SELECT FILE1   ASSIGN DDFILE1                             
                    FILE STATUS IS WS-FIL1-CODE.                       
                                                                       
                                                                       
             SELECT LISTADO   ASSIGN DDLISTA                           
                    FILE STATUS IS WS-FS-LISTADO.                      
                                                                       
       DATA DIVISION.                                                  
       FILE SECTION.                                                   
       FD FILE1                                                        
            BLOCK CONTAINS 0 RECORDS                                   
            RECORDING MODE IS F.                                       
                                                                       
     01 REG-FILE1      PIC X(93).                                    
                                                                     
                                                                     
     FD LISTADO                                                      
          BLOCK CONTAINS 0 RECORDS                                   
          RECORDING MODE IS F.                                       
                                                                     
     01 REG-SALIDA     PIC  X(132).                                  
                                                                     
    **************************************                           
     WORKING-STORAGE SECTION.                                        
    **************************************                           
     77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'. 
     77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'. 
     77  WS-FIL1-CODE      PIC XX    VALUE ZEROS.                    
     77  WS-FS-LISTADO     PIC XX    VALUE ZEROS.                    
                                                                     
     01  WS-STATUS-FIN    PIC X.                                     
         88  WS-FIN-LECTURA         VALUE 'Y'.                       
         88  WS-NO-FIN-LECTURA      VALUE 'N'.                       
                                                                     
    **************************************                           
    *         LAYOUT FILE1               *                           
    *     LARGO REGISTRO = 93 BYTES      *                           
    **************************************                           
     01  WS-REG-FILE1.                                               
                                                                     
         03  WS-SUC-NRO1         PIC 9(03)    VALUE ZEROS.           
         03  WS-SUC-TIP-DOC1     PIC XX       VALUE SPACES.          
         03  WS-SUC-NRO-DOC1     PIC 9(11)    VALUE ZEROS.           
         03  WS-SUC-NOMAPE1      PIC X(30)    VALUE SPACES.          
         03  WS-SUC-EST-CIV1     PIC X(10)    VALUE SPACES.          
         03  WS-SUC-SEXO1        PIC X        VALUE SPACES.          
         03  WS-SUC-CTA1         PIC 9(05)    VALUE ZEROS.           
         03  WS-SUC-SALDO1       PIC S9(7)V99 COMP-3  VALUE ZEROS.   
         03  FILLER              PIC X(26)    VALUE SPACES.          
                                                                    
   **************************************                           
   *         LAYOUT LISTADO             *                           
   *     LARGO REGISTRO = 133 BYTES     *                           
   **************************************                           
    01  WS-REG-LISTADO.                                             
                                                                    
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-NRO3         PIC ZZ9      VALUE ZEROS.           
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-TIP-DOC3     PIC XX       VALUE SPACES.          
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-NRO-DOC3     PIC 9(11)    VALUE ZEROS.           
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-NOMAPE3      PIC X(30)    VALUE SPACES.          
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-EST-CIV3     PIC X(10)    VALUE SPACES.          
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-SEXO3        PIC X        VALUE SPACES.          
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-CTA3         PIC 9(05)    VALUE ZEROS.           
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC-SALDO3       PIC ZZZZ.ZZ9,99-   VALUE ZEROS.     
        03  FILLER              PIC X(01)    VALUE SPACES.          
                                                                    
   * ULTIMA LINEA POR PáGINA 60                                     
    77  WS-CUENTA-LINEA         PIC 9(02)    VALUE ZEROS.           
    77  WS-CUENTA-PAGINA        PIC 9(02)    VALUE 01.              
                                                                    
    01  WS-TITULO.                                                  
        03  FILLER              PIC X        VALUE SPACES.          
        03  WS-SUC              PIC ZZ9      VALUE ZEROS.           
        03  FILLER              PIC X(05)    VALUE SPACES.          
        03  FILLER              PIC X(35)    VALUE                  
          'TOTAL CLIENTE/CUENTA POR SUCURSAL: ' .                   
        03  FILLER              PIC X        VALUE SPACES.          
         03  WS-DD               PIC Z9       VALUE ZEROS.            
         03  FILLER              PIC X        VALUE '-'.              
         03  WS-MM               PIC Z9       VALUE ZEROS.            
         03  FILLER              PIC X        VALUE '-'.              
         03  FILLER              PIC 99       VALUE 20.               
         03  WS-AA               PIC 99       VALUE ZEROS.            
         03  FILLER              PIC X(4)     VALUE SPACES.           
         03  FILLER              PIC X(15)   VALUE                    
              'NUMERO PAGINA: '.                                      
         03  WS-PAGINA           PIC Z9       VALUE ZEROS.            
         03  FILLER              PIC X(41)    VALUE SPACES.           
                                                                      
    ********     CONTADOR DE LEIDOS Y GRABADOS  *                     
                                                                      
     01  WS-LEIDOS-FILE1      PIC 9(05)        VALUE ZEROS.           
     01  WS-IMPRESOS          PIC 9(05)        VALUE ZEROS.           
                                                                      
                                                                      
    ********     FECHA DE PROCESO ***************                     
     01  WS-FECHA.                                                    
         03  WS-FECHA-AA      PIC 99            VALUE ZEROS.          
         03  WS-FECHA-MM      PIC 99            VALUE ZEROS.          
         03  WS-FECHA-DD      PIC 99            VALUE ZEROS.          
                                                                      
     01  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.  
                                                                      
    ***************************************************************.  
     PROCEDURE DIVISION.                                              
    **************************************                            
    *                                    *                            
    *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
    *                                    *                            
    **************************************                            
     MAIN-PROGRAM.                                                    
                                                                      
         PERFORM 1000-INICIO  THRU   F-1000-INICIO.                   
                                                                       
           PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                  
                   UNTIL WS-FIN-LECTURA.                               
                                                                       
           PERFORM 9999-FINAL    THRU  F-9999-FINAL.                   
                                                                       
       F-MAIN-PROGRAM. GOBACK.                                         
                                                                       
      **************************************                           
      *                                    *                           
      *  CUERPO INICIO APERTURA ARCHIVOS   *                           
      *                                    *                           
      **************************************                           
       1000-INICIO.                                                    
           ACCEPT WS-FECHA FROM DATE.                                  
           MOVE WS-FECHA-AA TO WS-AA.                                  
           MOVE WS-FECHA-MM TO WS-MM.                                  
           MOVE WS-FECHA-DD TO WS-DD.                                  
           MOVE 62 TO WS-CUENTA-LINEA.                                 
                                                                       
                                                                       
           SET WS-NO-FIN-LECTURA TO TRUE.                              
                                                                       
           OPEN INPUT  FILE1.                                          
           IF WS-FIL1-CODE IS NOT EQUAL '00'                           
              DISPLAY '* ERROR EN OPEN FILE1   = ' WS-FIL1-CODE        
              MOVE 9999 TO RETURN-CODE                                 
              SET  WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                     
                                                                       
           OPEN OUTPUT LISTADO.                                        
           IF WS-FS-LISTADO IS NOT EQUAL '00'                          
              DISPLAY '* ERROR EN OPEN LISTADO   = ' WS-FS-LISTADO     
              MOVE 9999 TO RETURN-CODE                                 
              SET  WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                     
                                                                      
          PERFORM 3000-LEER-FILE1  THRU F-3000-LEER-FILE1.            
                                                                      
     F-1000-INICIO.   EXIT.                                           
                                                                      
    **************************************                            
    *                                    *                            
    *  CUERPO PRINCIPAL DE PROCESOS      *                            
    *                                    *                            
    **************************************                            
     2000-PROCESO.                                                    
                                                                      
                                                                      
         MOVE SPACES         TO WS-REG-LISTADO.                       
         MOVE  WS-SUC-NRO1       TO                                   
               WS-SUC-NRO3                                            
         MOVE  WS-SUC-TIP-DOC1   TO                                   
               WS-SUC-TIP-DOC3                                        
         MOVE  WS-SUC-NRO-DOC1   TO                                   
               WS-SUC-NRO-DOC3                                        
         MOVE  WS-SUC-NOMAPE1    TO                                   
               WS-SUC-NOMAPE3                                         
         MOVE  WS-SUC-EST-CIV1   TO                                   
               WS-SUC-EST-CIV3                                        
         MOVE  WS-SUC-SEXO1      TO                                   
               WS-SUC-SEXO3                                           
         MOVE  WS-SUC-CTA1       TO                                   
               WS-SUC-CTA3                                            
         MOVE  WS-SUC-SALDO1     TO                                   
               WS-SUC-SALDO3.                                         
                                                                      
         PERFORM  6000-GRABAR-SALIDA  THRU F-6000-GRABAR-SALIDA.      
         PERFORM 3000-LEER-FILE1  THRU F-3000-LEER-FILE1.             
                                                                      
     F-2000-PROCESO. EXIT.                                            
                                                                      
     **************************************                           
     * LECTURA FILE1                      *                           
     **************************************                           
      3000-LEER-FILE1.                                                
                                                                      
          READ FILE1   INTO WS-REG-FILE1.                             
                                                                      
          EVALUATE WS-FIL1-CODE                                       
            WHEN '00'                                                 
                        ADD 1 TO WS-LEIDOS-FILE1                      
             WHEN '10'                                                
             SET WS-FIN-LECTURA TO TRUE                               
                                                                      
                                                                      
          WHEN OTHER                                                  
             DISPLAY '* ERROR EN LECTURA FILE1   = ' WS-FIL1-CODE     
             MOVE 9999 TO RETURN-CODE                                 
             SET WS-FIN-LECTURA  TO TRUE                              
                                                                      
          END-EVALUATE.                                               
                                                                      
      F-3000-LEER-FILE1. EXIT.                                        
                                                                      
     ***************************************************              
     *PARRAFO PARA GRABAR LA SALIDA ACTUALIZADA                       
     ***************************************************              
                                                                      
      6000-GRABAR-SALIDA.                                             
                                                                      
          IF WS-CUENTA-LINEA GREATER 60                               
              PERFORM 6500-IMPRIMIR-TITULOS THRU                      
                    F-6500-IMPRIMIR-TITULOS                           
          END-IF.                                                     
                                                                      
          WRITE REG-SALIDA   FROM WS-REG-LISTADO AFTER 1.             
             IF WS-FS-LISTADO IS NOT EQUAL '00'                       
              DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                          WS-FS-LISTADO               
              MOVE 9999 TO RETURN-CODE                                
              SET WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                    
                                                                      
           ADD 1 TO WS-IMPRESOS.                                      
           ADD 1 TO WS-CUENTA-LINEA.                                  
                                                                      
     F-6000-GRABAR-SALIDA. EXIT.                                      
                                                                      
     6500-IMPRIMIR-TITULOS.                                           
         MOVE WS-CUENTA-PAGINA      TO WS-PAGINA.                     
         MOVE 1     TO WS-CUENTA-LINEA.                               
         ADD  1     TO WS-CUENTA-PAGINA.                              
         MOVE  WS-SUC-NRO1       TO    WS-SUC.                        
         WRITE   REG-SALIDA    FROM WS-TITULO   AFTER PAGE.           
                                                                      
            IF WS-FS-LISTADO IS NOT EQUAL '00'                        
              DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                          WS-FS-LISTADO               
              MOVE 9999 TO RETURN-CODE                                
              SET WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                    
                                                                      
     F-6500-IMPRIMIR-TITULOS. EXIT.                                   
                                                                      
    **************************************                            
    *                                    *                            
    *  CUERPO FINAL CIERRE DE FILES      *                            
    *                                    *                            
    **************************************                            
     9999-FINAL.                                                      
                                                                      
         CLOSE FILE1.                                                 
            IF WS-FIL1-CODE IS NOT EQUAL '00'                         
               DISPLAY '* ERROR EN CLOSE FILE1   = '                  
                                           WS-FIL1-CODE               
               MOVE 9999 TO RETURN-CODE                               
               SET WS-FIN-LECTURA TO TRUE                             
            END-IF.                                                   
                                                                      
          CLOSE LISTADO                                               
             IF WS-FS-LISTADO IS NOT EQUAL '00'                       
               DISPLAY '* ERROR EN CLOSE LISTADO = '                  
                                           WS-FS-LISTADO              
               MOVE 9999 TO RETURN-CODE                               
               SET WS-FIN-LECTURA TO TRUE                             
            END-IF.                                                   
                                                                      
     **************************************                           
     *   MOSTRAR TOTALES DE CONTROL                                   
     **************************************                           
                                                                      
          DISPLAY 'LEIDOS:     ' WS-LEIDOS-FILE1.                     
          DISPLAY 'IMPRESOS:   ' WS-IMPRESOS.                         
                                                                      
      F-9999-FINAL.                                                   
          EXIT.                                                       