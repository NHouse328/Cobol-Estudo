      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERA-CPF-DAT.

       ENVIRONMENT DIVISION.

               INPUT-OUTPUT SECTION.
                   FILE-CONTROL.
                       SELECT ARQUIVO ASSIGN TO DISK
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS FS-ENTRADA.

       DATA DIVISION.
           FILE SECTION.
                   FD ARQUIVO
                       LABEL RECORD STANDARD
                       VALUE OF FILE-ID IS
                   'C:\Users\re039833\Desktop\Cobol-Estudo\ARQCPF2.DAT'.

                       01 REG-ARQUIVO.
                           05 CPF-OUT  PIC 9(11).

               WORKING-STORAGE SECTION.

                   01 FS-ENTRADA       PIC X(02).

                   01 CPF              PIC 9(11) VALUE ZERO.
                   01 CPF-RED          REDEFINES CPF.
                       05 DIGITO       PIC 9(01) OCCURS 11.

                   01 VALIDO           PIC 9(01).
                   01 QTD-CPF-VAL      PIC 9(11).
                   01 IND              PIC 9(11).
                   01 IND-2            PIC 9(11).
                   01 MULT             PIC 9(02).
                   01 SOMA             PIC 9(04).
                   01 LIMITE           PIC 9(11) VALUES 99999999999.
                   01 DIV              PIC 9(02).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           PERFORM 1000-INICIAR.
           PERFORM 2000-PROCESSAR.
           PERFORM 3000-FINALIZAR.

           STOP RUN.

       1000-INICIAR.
           INITIALIZE CPF.
           OPEN OUTPUT ARQUIVO.

       2000-PROCESSAR.

           PERFORM 2100-GERA-CPF.

       2100-GERA-CPF.

           PERFORM VARYING IND-2 FROM 04568298652 BY 1
                   UNTIL   IND-2 GREATER LIMITE

               MOVE 0 TO VALIDO
               MOVE IND-2 TO CPF

               PERFORM 2110-VALIDA-DIGITO-1

               IF VALIDO = 0
                   PERFORM 2120-VALIDA-DIGITO-2
               END-IF

               IF VALIDO = 0
                   MOVE CPF TO CPF-OUT
                   WRITE REG-ARQUIVO
                   ADD 1 TO QTD-CPF-VAL
               END-IF
           END-PERFORM.

       2110-VALIDA-DIGITO-1.

               MOVE 0  TO SOMA
               MOVE 10 TO MULT.

               PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 9

                   COMPUTE SOMA = SOMA + (DIGITO(IND) * MULT)

                   COMPUTE MULT = MULT - 1

               END-PERFORM.

               COMPUTE SOMA = SOMA * 10.
               DIVIDE  SOMA BY 11 GIVING DIV REMAINDER SOMA .

               IF SOMA NOT EQUALS DIGITO(10)
                   MOVE 1 TO VALIDO
               END-IF.

       2120-VALIDA-DIGITO-2.

               MOVE 0  TO SOMA.
               MOVE 11 TO MULT.

               PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 10
                   COMPUTE SOMA = SOMA + (DIGITO(IND) * MULT)
                   COMPUTE MULT = MULT - 1
               END-PERFORM.

               COMPUTE SOMA = SOMA * 10.
               DIVIDE  SOMA BY 11 GIVING DIV REMAINDER SOMA .

               IF SOMA NOT EQUALS DIGITO(11)
                   MOVE 1 TO VALIDO
               END-IF.

       3000-FINALIZAR.

           CLOSE ARQUIVO.
           DISPLAY QTD-CPF-VAL ' CPF´S VALIDOS.'.
