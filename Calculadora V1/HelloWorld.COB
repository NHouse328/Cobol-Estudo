      ******************************************************************
      * Author:    Gabriel Casanova Silva
      * Date:      15/01/2020
      * Purpose:   Estudo
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 N1   PIC 9(09).
       01 N2   PIC 9(09).
       01 N3   PIC 9(09).
       01 OP   PIC X(01).
       PROCEDURE DIVISION.
           DISPLAY "----INICIO----".
           DISPLAY "CALCULADORA V1".
           ACCEPT N1.
           ACCEPT OP.
           ACCEPT N2.
           IF OP EQUAL "+"
               COMPUTE N3 = N1 + N2
               DISPLAY N1 " + " N2 " = " N3
           END-IF.
           IF OP EQUAL "-"
               COMPUTE N3 = N1 - N2
               DISPLAY N1 " - " N2 " = " N3
           END-IF.
           IF OP EQUAL "*"
               COMPUTE N3 = N1 * N2
               DISPLAY N1 " * " N2 " = " N3
           END-IF.
           IF OP EQUAL "*"
               COMPUTE N3 = N1 * N2
               DISPLAY N1 " * " N2 " = " N3
           END-IF.
           DISPLAY "----FIM----".
           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
