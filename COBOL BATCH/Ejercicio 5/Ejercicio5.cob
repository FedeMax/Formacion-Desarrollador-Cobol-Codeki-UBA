***************************** Top of Data *****************************
       IDENTIFICATION DIVISION.                                        
       PROGRAM-ID. PGMC1CBF.                                           
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
      **************************************************************   
       I-O-CONTROL.                                                    
                                                                       
       DATA DIVISION.                                                  
       FILE SECTION.                                                   
       FD   ENTRADA                                                    
           BLOCK CONTAINS 0 RECORDS                                    
           RECORDING MODE IS F.                                        
       01   REG-ENTRADA             PIC X(93).                         
      **************************************************************   
       WORKING-STORAGE SECTION.                                        
      **************************************************************   
                                                                       
           COPY CLICOB.                                                
                                                                       
      **************************************************************   
       77  FS-ENTRADA               PIC XX      VALUE SPACES.          
      **************************************************************   
                                                                       
       77  WS-STATUS                  PIC X.                           
           88  WS-FIN                 VALUE 'Y'.                       
        88  WS-NO-FIN              VALUE 'N'.                       
                                                                    
   *********************VARIABLES A USAR*************************   
    77 WS-TOTAL-LEI                PIC 9(5)    VALUE ZEROES.        
                                                                    
    01 WS-ESTADO-CIV.                                               
       05 SOLTERO                  PIC 9(5)    VALUE ZEROES.        
       05 CASADO                   PIC 9(5)    VALUE ZEROES.        
       05 VIUDO                    PIC 9(5)    VALUE ZEROES.        
       05 DIVORCIADO               PIC 9(5)    VALUE ZEROES.        
       05 OTRO                     PIC 9(5)    VALUE ZEROES.        
                                                                    
    01 WS-EST-CIV                  PIC X(10)    VALUE SPACES.       
                                                                    
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
                UNTIL WS-FIN.                                       
                                                                    
        PERFORM 9999-FINAL    THRU  F-9999-FINAL.                   
                                                                    
    F-MAIN-PROGRAM. GOBACK.                                         
                                                                    
   **************************************                           
   *                                    *                           
   *  CUERPO INICIO APERTURA ARCHIVOS   *                           
   *                                    *                           
   **************************************                           
    1000-INICIO.                                                    
     SET WS-NO-FIN TO TRUE.                                      
                                                                 
     OPEN INPUT  ENTRADA.                                        
                                                                 
                                                                 
     IF FS-ENTRADA  IS NOT EQUAL '00'                            
        DISPLAY '* ERROR EN OPEN SUCURSAL = ' FS-ENTRADA         
        MOVE 9999 TO RETURN-CODE                                 
        SET  WS-FIN     TO TRUE                                  
     END-IF.                                                     
                                                                 
     PERFORM 2500-LEER     THRU F-2500-LEER.                     
                                                                 
 F-1000-INICIO.   EXIT.                                          
**************************************************************   
                                                                 
 2000-PROCESO.                                                   
                                                                 
                                                                 
      EVALUATE WS-EST-CIV                                        
          WHEN 'SOLTERO'                                         
           ADD 1 TO SOLTERO                                      
                                                                 
          WHEN 'CASADO'                                          
           ADD 1 TO CASADO                                       
                                                                 
          WHEN 'DIVORCIADO'                                      
           ADD 1 TO DIVORCIADO                                   
                                                                 
          WHEN 'VIUDO'                                           
           ADD 1 TO VIUDO                                        
                                                                 
          WHEN OTHER                                             
           ADD 1 TO OTRO                                         
                                                                 
      END-EVALUATE.                                               
                                                                  
                                                                  
     PERFORM 2500-LEER     THRU F-2500-LEER.                      
                                                                  
 F-2000-PROCESO. EXIT.                                            
                                                                  
**************************************************************    
 2500-LEER.                                                       
     READ ENTRADA   INTO WS-REG-CLICOB                            
                                                                  
     EVALUATE FS-ENTRADA                                          
       WHEN '00'                                                  
        MOVE WS-SUC-EST-CIV TO   WS-EST-CIV                       
        ADD 1 TO WS-TOTAL-LEI                                     
                                                                  
        WHEN '10'                                                 
        SET WS-FIN          TO TRUE                               
                                                                  
     WHEN OTHER                                                   
        DISPLAY '* ERROR EN LECTURA SUCURSAL = ' FS-ENTRADA       
        MOVE 9999 TO RETURN-CODE                                  
        SET WS-FIN  TO TRUE                                       
                                                                  
     END-EVALUATE.                                                
                                                                  
 F-2500-LEER. EXIT.                                               
                                                                  
**************************************************************    
                                                                  
 9999-FINAL.                                                      
                                                                  
     IF RETURN-CODE = 9999                                        
      CONTINUE                                                    
           ELSE                                                        
             CLOSE ENTRADA                                             
                 IF FS-ENTRADA  IS NOT EQUAL '00'                      
                  DISPLAY '* ERROR EN CLOSE SUCURSAL = '               
                                            FS-ENTRADA                 
                  MOVE 9999 TO RETURN-CODE                             
                  SET WS-FIN     TO TRUE                               
                 END-IF                                                
                                                                       
                                                                       
      **************************************                           
      *   MOSTRAR TOTALES DE CONTROL       *                           
      **************************************                           
                DISPLAY '-----------------------------------------'    
                DISPLAY ' TOTAL DE REGISTROS LEIDOS  ' WS-TOTAL-LEI    
                                                                       
                DISPLAY 'SOLTEROS    --> ' SOLTERO                     
                DISPLAY 'CASADOS     --> ' CASADO                      
                DISPLAY 'VIUDOS      --> ' VIUDO                       
                DISPLAY 'DIVORCIADOS --> ' DIVORCIADO                  
                DISPLAY 'OTRO        --> ' OTRO                        
                                                                       
           END-IF.                                                     
                                                                       
       F-9999-FINAL.                                                   
           EXIT.                                                       