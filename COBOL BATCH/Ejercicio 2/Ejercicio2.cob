       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. EJERCI02.                                            
       AUTHOR.    FEDERICO FALCON.                                      
      **************************************************************    
      *  PRIMER CORTE DE CONTROL                                   *    
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
       01   REG-ENTRADA             PIC X(30).                          
      **************************************************************    
       WORKING-STORAGE SECTION.                                         
      **************************************************************    
                                                                        
           COPY CORTE.                                                  
                                                                        
      **************************************************************    
       77  FS-ENTRADA               PIC XX      VALUE SPACES.           
      **************************************************************    
                                                                        
 77  WS-STATUS                  PIC X.                            
     88  WS-FIN                 VALUE 'Y'.                        
     88  WS-NO-FIN              VALUE 'N'.                        
                                                                  
*********************VARIABLES A USAR*************************    
 01  WS-SUC-TIPO              PIC 9(02) VALUE ZEROES.             
 01  WS-SUC-TIPO-ANT          PIC 9(02) VALUE ZEROES.             
                                                                  
 01  MONTO                    PIC S9(7)V99 COMP-3  VALUE ZEROS.   
 01  TOTAL                    PIC S9(8)V99 COMP-3  VALUE ZEROS.   
                                                                  
 01  MASCARA1                 PIC  $$$$$$9,99.                    
 01  MASCARA2                 PIC $$$$$$$9,99.                    
                                                                  
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
        DISPLAY '* ERROR EN OPEN ENTRADA  = ' FS-ENTRADA         
        MOVE 9999 TO RETURN-CODE                                 
        SET  WS-FIN     TO TRUE                                  
     END-IF.                                                     
                                                                 
     PERFORM 2500-LEER     THRU F-2500-LEER.                     
     MOVE WS-SUC-TIPO    TO    WS-SUC-TIPO-ANT.                  
                                                                 
 F-1000-INICIO.   EXIT.                                          
**************************************************************   
 2000-PROCESO.                                                   
                                                                 
     IF  WS-SUC-TIPO EQUAL WS-SUC-TIPO-ANT                       
          ADD WS-SUC-IMPORTE TO MONTO                            
     ELSE                                                        
        PERFORM  3000-MOSTRAR THRU                               
               F-3000-MOSTRAR                                    
        ADD  MONTO TO TOTAL                                      
        MOVE WS-SUC-IMPORTE TO MONTO                             
        MOVE WS-SUC-TIPO    TO    WS-SUC-TIPO-ANT                
     END-IF.                                                     
                                                                 
     PERFORM 2500-LEER     THRU F-2500-LEER.                     
                                                                 
 F-2000-PROCESO. EXIT.                                           
                                                                 
**************************************************************   
 2500-LEER.                                                      
       READ ENTRADA   INTO WS-REG-SUCURSAL                         
                                                                  
      EVALUATE FS-ENTRADA                                         
        WHEN '00'                                                 
          MOVE WS-SUC-NRO TO WS-SUC-TIPO                          
                                                                  
                                                                  
         WHEN '10'                                                
         SET WS-FIN          TO TRUE                              
         PERFORM  3000-MOSTRAR THRU                               
                F-3000-MOSTRAR                                    
                ADD  MONTO TO TOTAL                               
                                                                  
      WHEN OTHER                                                  
         DISPLAY '* ERROR EN LECTURA ENTRADA = ' FS-ENTRADA       
         MOVE 9999 TO RETURN-CODE                                 
         SET WS-FIN  TO TRUE                                      
                                                                  
      END-EVALUATE.                                               
                                                                  
  F-2500-LEER. EXIT.                                              
                                                                  
 **************************************************************   
  3000-MOSTRAR.                                                   
                                                                  
      MOVE MONTO TO MASCARA1                                      
      DISPLAY ' SUCURSAL  ' WS-SUC-TIPO-ANT '      ' MASCARA1     
                                                                  
              .                                                   
  F-3000-MOSTRAR. EXIT.                                           
 **************************************************************   
  9999-FINAL.                                                     
                                                                  
      IF RETURN-CODE = 9999                                       
       CONTINUE                                                   
      ELSE                                                        
             CLOSE ENTRADA                                             
           IF FS-ENTRADA  IS NOT EQUAL '00'                      
            DISPLAY '* ERROR EN CLOSE ENTRADA  = '               
                                      FS-ENTRADA                 
            MOVE 9999 TO RETURN-CODE                             
            SET WS-FIN     TO TRUE                               
           END-IF                                                
                                                                 
                                                                 
     MOVE TOTAL TO MASCARA2                                      
     DISPLAY ' '                                                 
     DISPLAY ' TOTAL GENERAL    '  MASCARA2                      
                                                                 
     END-IF.                                                     
                                                                 
 F-9999-FINAL.                                                   
     EXIT.                                                       
                                                                