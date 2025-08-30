***************************** Top of Data *****************************
       IDENTIFICATION DIVISION.                                        
       PROGRAM-ID. PGMVACBF.                                           
       AUTHOR.    FEDERICO FALCON.                                     
      **************************************************************   
       ENVIRONMENT DIVISION.                                           
      **************************************************************   
       CONFIGURATION SECTION.                                          
                                                                       
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                          
      **************************************************************   
       INPUT-OUTPUT SECTION.                                           
       FILE-CONTROL.                                                   
                                                                       
            SELECT ENTRADA ASSIGN TO DDENTRA                           
                 FILE STATUS IS FS-ENTRADA.                            
                                                                       
            SELECT SALIDA ASSIGN TO DDSALI                             
                 FILE STATUS IS FS-SALIDA.                             
      **************************************************************   
       I-O-CONTROL.                                                    
                                                                       
       DATA DIVISION.                                                  
       FILE SECTION.                                                   
       FD   ENTRADA                                                    
           BLOCK CONTAINS 0 RECORDS                                    
           RECORDING MODE IS F.                                        
                                                                       
       01   REG-ENTRADA             PIC X(50).                         
      **************************************************************   
       FD   SALIDA                                                     
           BLOCK CONTAINS 0 RECORDS                                    
           RECORDING MODE IS F.                                        
                                                                       
       01   REG-SALIDA              PIC X(55).                         
      **************************************************************   
            WORKING-STORAGE SECTION.                                         
                                                                       
      77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
     **************************************************************    
                                                                       
          COPY CPNOVCLI.                                               
          COPY CPNCLIV.                                                
                                                                       
     *********************VARIABLES DE FILE STATUS*****************    
                                                                       
      77  FS-ENTRADA      PIC XX    VALUE SPACES.                      
          88  FS-ENTRADA-FIN        VALUE '10'.                        
                                                                       
      77  FS-SALIDA       PIC XX    VALUE SPACES.                      
          88  FS-SALIDA-FIN         VALUE '10'.                        
                                                                       
     *********************CONTADORES*******************************    
      77 WS-CANT-LEIDOS             PIC 9(5)    VALUE ZEROES.          
                                                                       
      77 WS-CANT-GRABADOS           PIC 9(5)    VALUE ZEROES.          
                                                                       
      77 WS-CANT-ERRONEOS           PIC 9(5)    VALUE ZEROES.          
                                                                       
     *********************VERIFICADORES****************************    
      77 WS-VERIFICA                PIC 9(3)    VALUE ZEROES.          
      77 WS-VERI-DOC                PIC 9       VALUE ZERO.            
      77 WS-VERI-SUC                PIC 9       VALUE ZERO.            
      77 WS-VERI-TIP-CLI            PIC 9       VALUE ZERO.            
      77 WS-VERI-ANIO               PIC 9       VALUE ZERO.            
      77 WS-VERI-ANIO-BI            PIC 9       VALUE ZERO.            
      77 WS-VERI-MES                PIC 9       VALUE ZERO.            
      77 WS-VERI-DIA                PIC 9       VALUE ZERO.            
      77 WS-VERI-DIA2               PIC 9       VALUE ZERO.            
                                                                       
     *********************VARIABLES A USAR*************************    
      77 WS-TIP-DOC                 PIC X(2)    VALUE SPACES.          
                                                                           
     77 WS-NRO-DOC                 PIC 9(11)    VALUE ZEROS.         
                                                                     
     77 WS-NRO-REG                 PIC 9(5)    VALUE ZEROES.         
                                                                     
     77 WS-RESTO                   PIC X(50)   VALUE SPACES.         
                                                                     
     77 WS-NRO-SUC                 PIC 9(2)    VALUE ZEROES.         
                                                                     
     01 WS-FECHA-COMPLETA.                                           
        05 ANIO                    PIC 9(4)    VALUE ZEROES.         
        05 MES                     PIC 9(2)    VALUE ZEROES.         
        05 DIA                     PIC 9(2)    VALUE ZEROES.         
                                                                     
     01 WS-COCIENTE                PIC 9(4)    VALUE ZEROES.         
                                                                     
     01 WS-RESTO1                  PIC S9(2)V99 VALUE ZEROES.        
     01 WS-RESTO2                  PIC S9(2)V99 VALUE ZEROES.        
     01 WS-RESTO3                  PIC S9(2)V99 VALUE ZEROES.        
                                                                     
     01 BISIESTO                   PIC X.                            
        88 BISIESTO-S                           VALUE 'S'.           
        88 BISIESTO-N                           VALUE 'N'.           
    **************************************************************   
     01  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'. 
    **************************************************************   
     PROCEDURE DIVISION.                                             
    **************************************                           
    *                                    *                           
    *  CUERPO PRINCIPAL DEL PROGRAMA     *                           
    *                                    *                           
    **************************************                           
     MAIN-PROGRAM.                                                   
                                                                     
         PERFORM 1000-INICIO  THRU   F-1000-INICIO.                  
                                                                     
          PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                   
                  UNTIL FS-ENTRADA-FIN.                                
                                                                       
          PERFORM 3000-FINAL    THRU  F-3000-FINAL.                    
                                                                       
      F-MAIN-PROGRAM. GOBACK.                                          
                                                                       
     **************************************                            
     *                                    *                            
     *  CUERPO INICIO APERTURA ARCHIVOS   *                            
     *                                    *                            
     **************************************                            
      1000-INICIO.                                                     
                                                                       
          OPEN INPUT  ENTRADA.                                         
                                                                       
          IF FS-ENTRADA  IS NOT EQUAL '00'                             
             DISPLAY '* ERROR EN OPEN SUCURSAL = ' FS-ENTRADA          
             MOVE 9999 TO RETURN-CODE                                  
             SET  FS-ENTRADA-FIN TO TRUE                               
                                                                       
          END-IF                                                       
                                                                       
          OPEN OUTPUT SALIDA                                           
                                                                       
          IF FS-SALIDA  IS NOT EQUAL '00'                              
             DISPLAY '* ERROR EN OPEN SALIDA = ' FS-SALIDA             
             MOVE 9999 TO RETURN-CODE                                  
             SET  FS-ENTRADA-FIN TO TRUE                               
                                                                       
          END-IF.                                                      
                                                                       
      F-1000-INICIO.   EXIT.                                           
     **************************************************************    
                                                                       
      2000-PROCESO.                                                    
                                                                     
         PERFORM 2100-LEER     THRU F-2100-LEER                      
                                                                     
         PERFORM 2400-CONTROLAR-DOCU THRU F-2400-CONTROLAR-DOCU      
                                                                     
         IF WS-VERIFICA = 0                                          
                                                                     
             PERFORM 2200-GRABAR THRU F-2200-GRABAR                  
                                                                     
         END-IF                                                      
                                                                     
             PERFORM 4000-MOSTRAR-ERRORES                            
                     THRU F-4000-MOSTRAR-ERRORES                     
                                                                     
             INITIALIZE WS-VERIFICA.                                 
                                                                     
     F-2000-PROCESO. EXIT.                                           
    **************************************************************   
                                                                     
     2100-LEER.                                                      
         READ ENTRADA           INTO WS-REG-NOVCLIE                  
                                                                     
         EVALUATE FS-ENTRADA                                         
           WHEN '00'                                                 
                MOVE NOV-TIP-DOC  TO WS-TIP-DOC                      
                MOVE NOV-NRO-DOC  TO WS-NRO-DOC                      
                MOVE NOV-SUC      TO WS-NRO-SUC                      
                 ADD 1 TO WS-CANT-LEIDOS                             
                                                                     
            WHEN '10'                                                
              CONTINUE                                               
                                                                     
            WHEN OTHER                                               
            DISPLAY '* ERROR EN LECTURA DE ENTRADA ' FS-ENTRADA      
            MOVE 9999 TO RETURN-CODE                                 
            SET FS-ENTRADA-FIN TO TRUE                               
                                                                       
          END-EVALUATE.                                                
                                                                       
      F-2100-LEER. EXIT.                                               
     **************************************************************    
                                                                       
      2200-GRABAR.                                                     
                                                                       
          EVALUATE FS-SALIDA                                           
            WHEN '00'                                                  
                   ADD 1 TO  WS-NRO-REG                                
                   ADD 1 TO  WS-CANT-GRABADOS                          
                  MOVE WS-NRO-REG TO NOV-SECUEN OF REG-NOVCLIE-VAL     
                  MOVE WS-REG-NOVCLIE TO NOV-RESTO OF REG-NOVCLIE-VAL  
                  MOVE REG-NOVCLIE-VAL TO REG-SALIDA                   
                    WRITE REG-SALIDA                                   
             WHEN '10'                                                 
               CONTINUE                                                
                                                                       
          WHEN OTHER                                                   
             DISPLAY '* ERROR EN GRABAR SALIDA   = ' FS-SALIDA         
             MOVE 9999 TO RETURN-CODE                                  
             SET FS-ENTRADA-FIN  TO TRUE                               
                                                                       
          END-EVALUATE.                                                
                                                                       
      F-2200-GRABAR. EXIT.                                             
     **************************************************************    
                                                                       
      2400-CONTROLAR-DOCU.                                             
                                                                       
          IF WS-TIP-DOC = 'DU' OR WS-TIP-DOC = 'PA'                    
          OR WS-TIP-DOC = 'PE' OR WS-TIP-DOC = 'CI'                    
                                                                       
             CONTINUE                                                  
                                                                       
         ELSE                                                        
                  ADD 1 TO WS-CANT-ERRONEOS                          
                  ADD 1 TO WS-VERIFICA                               
                  ADD 1 TO WS-VERI-DOC                               
         END-IF                                                      
                                                                     
            PERFORM 2500-CONT-SUCURSAL THRU F-2500-CONT-SUCURSAL.    
                                                                     
     F-2400-CONTROLAR-DOCU. EXIT.                                    
    **************************************************************   
     2500-CONT-SUCURSAL.                                             
                                                                     
         IF WS-NRO-SUC > 0 AND WS-NRO-SUC < 100                      
                                                                     
            CONTINUE                                                 
                                                                     
         ELSE                                                        
                  ADD 1 TO WS-CANT-ERRONEOS                          
                  ADD 1 TO WS-VERIFICA                               
                  ADD 1 TO WS-VERI-SUC                               
                                                                     
         END-IF                                                      
                                                                     
            PERFORM 2600-CLI-TIPO  THRU F-2600-CLI-TIPO.             
                                                                     
     F-2500-CONT-SUCURSAL. EXIT.                                     
    **************************************************************   
     2600-CLI-TIPO.                                                  
                                                                     
         IF    NOV-CLI-TIPO = 1                                      
            OR NOV-CLI-TIPO = 2                                      
            OR NOV-CLI-TIPO = 3                                      
                                                                     
            CONTINUE                                                 
                                                                     
         ELSE                                                        
                                                                      
                   ADD 1 TO WS-CANT-ERRONEOS                          
                   ADD 1 TO WS-VERIFICA                               
                   ADD 1 TO WS-VERI-TIP-CLI                           
                                                                      
          END-IF                                                      
                                                                      
             PERFORM 2700-CONT-ANIO THRU F-2700-CONT-ANIO.            
                                                                      
      F-2600-CLI-TIPO. EXIT.                                          
     **************************************************************   
      2700-CONT-ANIO.                                                 
                                                                      
          MOVE NOV-CLI-FECHA TO WS-FECHA-COMPLETA                     
                                                                      
          PERFORM 2800-BISIESTO THRU F-2800-BISIESTO.                 
                                                                      
          IF BISIESTO-S AND  MES = 2                                  
                                                                      
             AND DIA = 29                                             
                                                                      
             CONTINUE                                                 
                                                                      
          ELSE                                                        
                                                                      
              IF MES = 2 AND DIA = 29                                 
                                                                      
                   ADD 1 TO WS-CANT-ERRONEOS                          
                   ADD 1 TO WS-VERIFICA                               
                   ADD 1 TO WS-VERI-ANIO-BI                           
                                                                      
              END-IF                                                  
                                                                      
          END-IF                                                      
                                                                      
          IF ANIO > 2024                                              
                                                                     
            CONTINUE                                                 
                                                                     
         ELSE                                                        
                                                                     
                  ADD 1 TO WS-CANT-ERRONEOS                          
                  ADD 1 TO WS-VERIFICA                               
                  ADD 1 TO WS-VERI-ANIO                              
                                                                     
         END-IF                                                      
                                                                     
         IF MES > 0 AND MES < 13                                     
                                                                     
            CONTINUE                                                 
                                                                     
         ELSE                                                        
                                                                     
                  ADD 1 TO WS-CANT-ERRONEOS                          
                  ADD 1 TO WS-VERIFICA                               
                  ADD 1 TO WS-VERI-MES                               
                                                                     
         END-IF                                                      
                                                                     
         IF ( MES = 4 OR MES = 6 OR MES = 9 OR MES = 11 )            
                                                                     
           AND DIA = 31                                              
                                                                     
                      ADD 1 TO WS-CANT-ERRONEOS                      
                      ADD 1 TO WS-VERIFICA                           
                      ADD 1 TO WS-VERI-DIA                           
         END-IF                                                      
                                                                     
         IF DIA > 0 AND DIA < 32                                     
                                                                     
            CONTINUE                                                 
                                                                     
         ELSE                                                         
                                                                      
                      ADD 1 TO WS-CANT-ERRONEOS                       
                      ADD 1 TO WS-VERIFICA                            
                      ADD 1 TO WS-VERI-DIA2                           
                                                                      
         END-IF.                                                      
                                                                      
     F-2700-CONT-ANIO. EXIT.                                          
    **************************************************************    
     2800-BISIESTO.                                                   
                                                                      
         INITIALIZE BISIESTO                                          
                                                                      
         DIVIDE ANIO BY 4   GIVING WS-COCIENTE REMAINDER WS-RESTO1    
         DIVIDE ANIO BY 400 GIVING WS-COCIENTE REMAINDER WS-RESTO2    
         DIVIDE ANIO BY 100 GIVING WS-COCIENTE REMAINDER WS-RESTO3    
                                                                      
         IF WS-RESTO = 0 AND WS-RESTO2 > 0 AND WS-RESTO3 = 0          
                                                                      
                     CONTINUE                                         
                                                                      
            ELSE                                                      
                                                                      
                IF WS-RESTO > 0 AND WS-RESTO2 > 0 AND WS-RESTO3 > 0   
                                                                      
                     CONTINUE                                         
                                                                      
                ELSE                                                  
                                                                      
                     SET BISIESTO-S TO TRUE                           
                                                                      
                END-IF                                                
         END-IF.                                                      
                                                                      
     F-2800-BISIESTO. EXIT.                                           
      **************************************************************   
       3000-FINAL.                                                     
                                                                       
           IF RETURN-CODE NOT EQUAL 9999                               
             CLOSE ENTRADA                                             
                 IF FS-ENTRADA  IS NOT EQUAL '00'                      
                  DISPLAY '* ERROR EN CLOSE SUCURSAL = '               
                                            FS-ENTRADA                 
                  MOVE 9999 TO RETURN-CODE                             
                                                                       
                 END-IF                                                
                                                                       
             CLOSE SALIDA                                              
                 IF FS-SALIDA   IS NOT EQUAL '00'                      
                  DISPLAY '* ERROR EN CLOSE SUCURSAL = '               
                                            FS-SALIDA                  
                  MOVE 9999 TO RETURN-CODE                             
                 END-IF                                                
                                                                       
                                                                       
      **************************************                           
      *   MOSTRAR TOTALES DE CONTROL       *                           
      **************************************                           
                                                                       
           DISPLAY '******************************'                    
           DISPLAY ' CANTIDAD DE REGISTROS LEIDOS       :  '           
                                           WS-CANT-LEIDOS              
           DISPLAY '******************************'                    
           DISPLAY ' CANTIDAD DE REGISTROS GRABADOS     :  '           
                                           WS-CANT-GRABADOS            
           DISPLAY '******************************'                    
           DISPLAY ' CANTIDAD DE ERRORES EN EL REGISTRO :  '           
                                           WS-CANT-ERRONEOS            
           DISPLAY '******************************'                    
                                                                       
           END-IF.                                                     
                                                                     
     F-3000-FINAL.                                                   
         EXIT.                                                       
    **************************************************************   
     4000-MOSTRAR-ERRORES.                                           
                                                                     
         IF WS-VERIFICA > 0                                          
              DISPLAY '---------------------------------------------'
              DISPLAY ' ERRORES ENCONTRADOS EN EL DOCUMENTO  :  '    
                                                       WS-TIP-DOC    
              DISPLAY ' NUMERO DE DOCUMENTO                  :  '    
                                                       WS-NRO-DOC    
         END-IF                                                      
                                                                     
         IF WS-VERI-DOC > 0                                          
            PERFORM 4100-ERROR-DOCU THRU F-4100-ERROR-DOCU           
            INITIALIZE WS-VERI-DOC                                   
         END-IF                                                      
                                                                     
         IF WS-VERI-SUC > 0                                          
            PERFORM 4200-ERROR-SUCURSAL THRU F-4200-ERROR-SUCURSAL   
            INITIALIZE WS-VERI-SUC                                   
         END-IF                                                      
                                                                     
         IF WS-VERI-TIP-CLI > 0                                      
                                                                     
            PERFORM 4300-ERROR-TIPO-CUENTA THRU                      
                                   F-4300-ERROR-TIPO-CUENTA          
                                                                     
            INITIALIZE WS-VERI-TIP-CLI                               
         END-IF                                                      
                                                                     
         IF WS-VERI-ANIO-BI > 0                                      
            PERFORM 4450-ERROR-ANIO-BI THRU F-4450-ERROR-ANIO-BI     
            INITIALIZE WS-VERI-ANIO-BI                               
         END-IF                                                      
                                                                      
         IF WS-VERI-ANIO > 0                                          
            PERFORM 4400-ERROR-ANIO THRU F-4400-ERROR-ANIO            
            INITIALIZE WS-VERI-ANIO                                   
         END-IF                                                       
                                                                      
         IF WS-VERI-MES > 0                                           
            PERFORM 4500-ERROR-MES THRU F-4500-ERROR-MES              
            INITIALIZE WS-VERI-MES                                    
         END-IF                                                       
                                                                      
         IF WS-VERI-DIA > 0                                           
            PERFORM 4600-ERROR-DIA THRU F-4600-ERROR-DIA              
            INITIALIZE WS-VERI-DIA                                    
         END-IF                                                       
                                                                      
         IF WS-VERI-DIA2 > 0                                          
            PERFORM 4650-ERROR-DIA THRU F-4650-ERROR-DIA              
            INITIALIZE WS-VERI-DIA2                                   
         END-IF                                                       
                                                                      
         IF WS-VERIFICA > 0                                           
         DISPLAY '---------------------------------------------'      
         END-IF.                                                      
                                                                      
     F-4000-MOSTRAR-ERRORES. EXIT.                                    
    **************************************************************    
     4100-ERROR-DOCU.                                                 
                                                                      
    *         DISPLAY '-----------------------------'                 
              DISPLAY ' TIPO DOCUMENTO INVALIDO      '  .             
    *         DISPLAY '-----------------------------'                 
                                                                      
     F-4100-ERROR-DOCU. EXIT.                                         
    **************************************************************    
     4200-ERROR-SUCURSAL.                                             
                                                                          
    *         DISPLAY '-----------------------------'                
              DISPLAY ' NRO DE SUCURSAL NO VALIDO :  ' NOV-SUC.      
    *         DISPLAY '-----------------------------'                
                                                                     
     F-4200-ERROR-SUCURSAL. EXIT.                                    
    **************************************************************   
     4300-ERROR-TIPO-CUENTA.                                         
                                                                     
    *         DISPLAY '-----------------------------'                
              DISPLAY ' NRO DE CUENTA NO VALIDO   :  ' NOV-CLI-TIPO. 
    *         DISPLAY '-----------------------------'                
                                                                     
     F-4300-ERROR-TIPO-CUENTA. EXIT.                                 
    **************************************************************   
     4400-ERROR-ANIO.                                                
                                                                     
    *         DISPLAY '-----------------------------'                
              DISPLAY ' EL AñO DEBE SER MAYOR A 2024 '               
              DISPLAY ' AñO INGRESADO : ' ANIO.                      
    *         DISPLAY '-----------------------------'                
                                                                     
     F-4400-ERROR-ANIO. EXIT.                                        
    **************************************************************   
     4450-ERROR-ANIO-BI.                                             
                                                                     
    *         DISPLAY '-----------------------------'                
              DISPLAY ' EL AñO NO ES BISIESTO '.                     
    *         DISPLAY '-----------------------------'                
                                                                     
     F-4450-ERROR-ANIO-BI. EXIT.                                     
    **************************************************************   
     4500-ERROR-MES.                                                 
                                                                     
    *         DISPLAY '-----------------------------'                
              DISPLAY ' EL MES INGRESADO ES INCORRECTO '             
             DISPLAY ' MES INGRESADO : ' MES.                        
   *         DISPLAY '-----------------------------'                 
                                                                     
    F-4500-ERROR-MES. EXIT.                                          
   **************************************************************    
    4600-ERROR-DIA.                                                  
                                                                     
   *             DISPLAY '-----------------------------'             
                 DISPLAY ' FECHA NO VALIDA'                          
                 DISPLAY ' MES           : ' MES                     
                 DISPLAY ' DIA INGRESADO : ' DIA.                    
   *             DISPLAY '-----------------------------'             
                                                                     
    F-4600-ERROR-DIA. EXIT.                                          
                                                                     
   **************************************************************    
    4650-ERROR-DIA.                                                  
                                                                     
   *             DISPLAY '-----------------------------'             
                 DISPLAY ' FECHA NO VALIDA'                          
                 DISPLAY ' DIA INGRESADO : ' DIA.                    
   *             DISPLAY '-----------------------------'             
                                                                     
    F-4650-ERROR-DIA. EXIT.                                          
************************* Bottom of Data ****************************