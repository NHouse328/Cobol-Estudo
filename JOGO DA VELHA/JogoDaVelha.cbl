      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
           IDENTIFICATION DIVISION.
               PROGRAM-ID. YOUR-PROGRAM-NAME.

           DATA DIVISION.
               WORKING-STORAGE SECTION.

                   01 CORDENADA-X          PIC 9.
                       88 X-MAX            VALUE 1 THRU 3.
                   01 CORDENADA-Y          PIC 9.
                       88 Y-MAX            VALUE 1 THRU 3.

                   01 FIM                  PIC 9 VALUE 0.

                   01 JOGADOR              PIC X(1) VALUE 'O'.
                       88 QUEM             VALUE 'X'.

                   01 JOGO-TABLE.
                       05 X-TABLE          OCCURS 3 TIMES INDEXED BY X.
                           10 Y-TABLE      OCCURS 3 TIMES INDEXED BY Y.
                               15 CASA     PIC X(1).
                               88 OCUPADO  VALUE '-'.

                   01 PONTO-TABLE.
                       05 X-PONTO          OCCURS 3 TIMES INDEXED BY XP.
                           10 Y-PONTO      OCCURS 3 TIMES INDEXED BY YP.
                               15 PONTO    PIC 9.

                   01 PONTOS               PIC 9.

      *             01 GABRIEL.
      *                 03 STATUS-OCUPACAO PIC X(1).
      *                             88 OCUPADO  VALUE '-'.

                   01 APOIO                PIC 9(02).
                   01 IND                  PIC 9(01).

           PROCEDURE DIVISION.
               MAIN-PROCEDURE.

                   PERFORM PREPARA-TABLE.

                   PERFORM LOOP.

           STOP RUN.

               JOGO.

                   PERFORM VEZ-JOGADOR.
                   PERFORM JOGADA.
                   PERFORM VERIFICA-CASA.
                   PERFORM ALTERA-CASA.
                   PERFORM ALTERA-PONTO.
                   PERFORM TROCA-JOGADOR.
                   PERFORM EXIBE-TABLE.

                   PERFORM VERIFICA-GANHADOR.

               LOOP.

                   PERFORM JOGO VARYING APOIO FROM 1 BY 1 UNTIL FIM = 1.

               PREPARA-TABLE.

                   MOVE '---------' TO JOGO-TABLE.

               EXIBE-TABLE.

                   PERFORM X-CASA VARYING X FROM 1 BY 1 UNTIL X > 3.

               X-CASA.
      *             PERFORM Y-CASA VARYING Y FROM 1 BY 1 UNTIL Y > 3.
                   DISPLAY X-TABLE(X).

               Y-CASA.
                   DISPLAY CASA(X,Y).

               VERIFICA-CASA.

                   IF NOT OCUPADO(CORDENADA-X,CORDENADA-Y)
                       DISPLAY 'CASA OCUPADA'
                       GO TO   LOOP
                   END-IF.

               ALTERA-CASA.
                   IF QUEM
                       MOVE 'O' TO CASA(CORDENADA-X,CORDENADA-Y)
                   ELSE
                       MOVE 'X' TO CASA(CORDENADA-X,CORDENADA-Y)
                   END-IF.
      *             DISPLAY CASA(CORDENADA-X,CORDENADA-Y).

               ALTERA-PONTO.

                   IF QUEM
                       MOVE 0 TO PONTO(CORDENADA-X,CORDENADA-Y)
                   ELSE
                       MOVE 1 TO PONTO(CORDENADA-X,CORDENADA-Y)
                   END-IF.

               TROCA-JOGADOR.

                   IF QUEM
                       MOVE 'O' TO JOGADOR
                   ELSE
                       MOVE 'X' TO JOGADOR
                   END-IF.

               VEZ-JOGADOR.

                   IF QUEM
                       DISPLAY 'JOGADOR O.'
                   ELSE
                       DISPLAY 'JOGADOR X.'
                   END-IF.

               JOGADA.

                   DISPLAY 'ENTRE CORDENADA X :'
                   ACCEPT  CORDENADA-X.
                   DISPLAY 'ENTRE CORDENADA Y :'
                   ACCEPT  CORDENADA-Y.

               VERIFICA-GANHADOR.

               PERFORM VARYING XP FROM 1 BY 1 UNTIL XP > 3
                   PERFORM VARYING YP FROM 1 BY 1 UNTIL YP > 3
                       COMPUTE PONTOS = PONTOS + PONTO(XP,YP)
                   END-PERFORM

                   IF PONTOS = 3 OR -3
                       GO TO GANHOU
                   ELSE
                       MOVE 0 TO PONTOS
                   END-IF

               END-PERFORM.



               GANHOU.

                   DISPLAY 'JOGADOR ' JOGADOR ' GANHOU !'.
                   MOVE 1 TO FIM.



           END PROGRAM YOUR-PROGRAM-NAME.
