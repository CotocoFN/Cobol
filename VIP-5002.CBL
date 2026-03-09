       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-5002.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CONSULTA ESTOQUE                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-ESTOQUE.SL".
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-ESTOQUE.FD".
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STAUTS-DATA.
           05 FS-VIP-ESTOQUE           PIC X(02).
              88 VALID-VIP-ESTOQUE     VALUE "00" THRU "09".
       01  STAUTS-DATA.
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".

       01  NM-COD-FOR                  PIC ZZZZZZ9.
       01  NM-FOR                      PIC X(60).
       01  PRODUTO.
           03 TELA-CD-PRODUTO          PIC 9(07).
           03 TELA-NM-PRODUTO          PIC X(60).
           03 TELA-DS-PRODUTO          PIC X(20).
           03 TELA-QT-PRODUTO          PIC Z.ZZ9,99.
           03 TELA-VL-PRODUTO          PIC Z.ZZZ.ZZ9,99.
           03 TELA-VL-CUSTO-MEDIO      PIC Z.ZZZ.ZZ9,99.
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.

       01  OPC.
           03 COD-BUSCA                PIC 9(01).
           03 OPC-CANCELAR             PIC X(01).
      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  CONSULTA-PROD.
           03 LINE 08 COL 20 VALUE "CONSULTA DE ESTOQUE".
           03 LINE 10 COL 20 VALUE "CODIGO:".
           03 LINE 11 COL 20 VALUE "NOME:".
           03 LINE 12 COL 20 VALUE "DESCRICAO:".
           03 LINE 13 COL 20 VALUE "ESTOQUE:".
           03 LINE 14 COL 20 VALUE "VALOR:".
           03 LINE 15 COL 20 VALUE "CM...:".
           03 LINE 17 COL 20 VALUE "FORNECEDOR:".
      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
           OPEN INPUT VIP-ESTOQUE.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-ESTOQUE!"
                            X"0A" "FILES STATUS:" FS-VIP-ESTOQUE
              GO FIM-CONSULTA
           END-IF.
           OPEN INPUT VIP-CLIFOR.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-CLIFOR!"
                            X"0A" "FILES STATUS:" FS-VIP-CLIFOR
              GO FIM-CONSULTA
           END-IF.
           DISPLAY WINDOW ERASE.
           DISPLAY CONSULTA-PROD.
       CONSULTA-ESTOQUE.
           ACCEPT TELA-CD-PRODUTO      AT 1027 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              GO FIM-CONSULTA
           END-IF.
      *=================================================================
            IF F1-KEY
              INITIALIZE REG-ESTOQUE
              START VIP-ESTOQUE KEY > ESTOQUE-CHAVE
              READ VIP-ESTOQUE NEXT WITH NO LOCK
              IF NOT VALID-VIP-ESTOQUE
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR-PARA-TELA
              PERFORM RESULTADO-CONSULTA
              GO CONSULTA-ESTOQUE
           END-IF.
           IF F2-KEY
              READ VIP-ESTOQUE PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-ESTOQUE
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR-PARA-TELA
              PERFORM RESULTADO-CONSULTA
              GO CONSULTA-ESTOQUE
           END-IF.

           IF F3-KEY
              READ VIP-ESTOQUE NEXT WITH NO LOCK
              IF NOT VALID-VIP-ESTOQUE
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR-PARA-TELA
              PERFORM RESULTADO-CONSULTA
              GO CONSULTA-ESTOQUE
           END-IF.
           IF F4-KEY
              MOVE HIGH-VALUE          TO ESTOQUE-CD-PROD
              START VIP-ESTOQUE KEY < ESTOQUE-CHAVE
              READ VIP-ESTOQUE PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-ESTOQUE
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR-PARA-TELA
              PERFORM RESULTADO-CONSULTA
              GO CONSULTA-ESTOQUE
           END-IF.
      *=================================================================
           INITIALIZE REG-ESTOQUE.
           MOVE TELA-CD-PRODUTO        TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE WITH NO LOCK.
           IF NOT VALID-VIP-ESTOQUE
               DISPLAY MESSAGE BOX "CODIGO NAO ENCONTRADO!" X"0A"
                                   "FILE STATUS:" FS-VIP-ESTOQUE
               GO FIM-CONSULTA
           END-IF.
           PERFORM MOVER-FOR-PARA-TELA.
           PERFORM MOVER-REGISTRO-PARA-TELA.
           PERFORM RESULTADO-CONSULTA.
           GO CONSULTA-ESTOQUE.
       FIM-CONSULTA.
           CLOSE VIP-ESTOQUE.
           CLOSE VIP-CLIFOR.
           EXIT PROGRAM.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-FOR-PARA-TELA.
           INITIALIZE REG-CLIFOR.
           MOVE ESTOQUE-CD-FOR         TO CLIFOR-CD-CLIFOR.
           READ VIP-CLIFOR WITH NO LOCK.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERROR 404"
                             X"0A""FILE STATUS:" FS-VIP-CLIFOR
           ELSE
               MOVE CLIFOR-NM-CLIFOR   TO NM-FOR
           END-IF.

       MOVER-REGISTRO-PARA-TELA.
           MOVE ESTOQUE-CD-FOR         TO NM-COD-FOR.
           MOVE ESTOQUE-CD-PROD        TO TELA-CD-PRODUTO.
           MOVE ESTOQUE-NM-PROD        TO TELA-NM-PRODUTO.
           MOVE ESTOQUE-DESC-PROD      TO TELA-DS-PRODUTO.
           MOVE ESTOQUE-QT-PROD        TO TELA-QT-PRODUTO.
           MOVE ESTOQUE-VALOR-PROD     TO TELA-VL-PRODUTO.
           MOVE ESTOQUE-VALOR-CM       TO TELA-VL-CUSTO-MEDIO.

       RESULTADO-CONSULTA.
           DISPLAY TELA-CD-PRODUTO     AT 1027.
           DISPLAY TELA-NM-PRODUTO     AT 1125.
           DISPLAY TELA-DS-PRODUTO     AT 1230.
           DISPLAY TELA-QT-PRODUTO     AT 1329.
           DISPLAY TELA-VL-PRODUTO     AT 1426.
           DISPLAY TELA-VL-CUSTO-MEDIO AT 1526
           DISPLAY NM-COD-FOR          AT 1731.
           DISPLAY "/"                 AT 1738
           DISPLAY NM-FOR              AT 1739.
