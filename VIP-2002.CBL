       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-2002.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      VENDA / SAIDA                             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-VENDA.SL".
           COPY "VIP-ESTOQUE.SL".
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-VENDA.FD".
           COPY "VIP-ESTOQUE.FD".
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           05 FS-VIP-VENDA             PIC X(02).
              88 VALID-VIP-VENDA       VALUE "00" THRU "09".
       01  STATUS-DATA.
           05 FS-VIP-ESTOQUE           PIC X(02).
              88 VALID-VIP-ESTOQUE     VALUE "00" THRU "09".
       01  STATUS-DATA.
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".

       01  VENDA-ID                    PIC 9(02).

       01  ADD-CLIFOR.
           03 CD-CLIENTE               PIC 9(02).
           03 NM-CLIENTE               PIC X(15).
           03 CPF-CNPJ                 PIC 9(10).
           03 CEP                      PIC 9(09).
           03 ENDERECO                 PIC X(20).
           03 BAIRRO                   PIC X(20).
           03 CIDADE                   PIC X(20).
           03 ESTADO                   PIC X(20).

       01  ADD-PROD.
           03 CD-PROD                  PIC 9(02).
           03 NM-PROD                  PIC X(15).
           03 DESC-PROD                PIC X(15).
           03 QT-PROD                  PIC 9(10).
           03 VALOR-UNI-PROD           PIC 9(15)V99.
           03 VALOR-TOT-PROD           PIC ZZ.ZZZ.ZZ9,99.
       01  CONFIRMA-ALTERA             PIC X(01).
       01  FINALIZA                    PIC X(01).
       01  WSS-ID-VENDA                PIC 9(02).


       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.



      ******************************************************************
      *                     SCREEN-SECTION                             *
      ******************************************************************
       SCREEN SECTION.
       01  PRINCIPAL.
           03 LINE 05 COL 30           VALUE "VENDA/SAIDA".
           03 LINE 06 COL 30           VALUE "ID-VENDA:".
           03 LINE 08 COL 05           VALUE "CODIGO CLIENTE:".
           03 LINE 09 COL 05           VALUE "NOME CLIENTE:".
           03 LINE 10 COL 05           VALUE "CPF/CNPJ:".
           03 LINE 11 COL 05           VALUE "CEP:".
           03 LINE 12 COL 05           VALUE "ENDERECO:".
           03 LINE 13 COL 05           VALUE "BAIRRO:".
           03 LINE 14 COL 05           VALUE "CIDADE:".
           03 LINE 15 COL 05           VALUE "ESTADO:".
       01  DIVISORIA.
           03 LINE 08 COL 35           VALUE "|".
           03 LINE 09 COL 35           VALUE "|".
           03 LINE 10 COL 35           VALUE "|".
           03 LINE 11 COL 35           VALUE "|".
           03 LINE 12 COL 35           VALUE "|".
           03 LINE 13 COL 35           VALUE "|".
           03 LINE 14 COL 35           VALUE "|".
           03 LINE 15 COL 35           VALUE "|".
       01  PRODUTOS.
           03 LINE 08 COL 37           VALUE "CODIGO PRODUTO:".
           03 LINE 09 COL 37           VALUE "NOME PRODUTO:".
           03 LINE 10 COL 37           VALUE "DESCRICAO:".
           03 LINE 11 COL 37           VALUE "VALOR UNITARIO:".
           03 LINE 12 COL 37           VALUE "QUANTIDADE:".
       01  SOMA-TOTAL.
           03 LINE 14 COL 37           VALUE "TOTAL:".
       01  CONF-ALTERA.
           03 LINE 17 COL 37           VALUE "ALTERAR VALOR UNITARIO ?".
           03 LINE 18 COL 37           VALUE "SIM=F2/NAO=F3 < >".
       01  FINALIZA-NOTA.
           03 LINE 20 COL 37           VALUE "FINALIZAR A NOTA ?".
           03 LINE 21 COL 37           VALUE "SIM=F2/NAO=F3 < >".

      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
           OPEN I-O VIP-VENDA.
           IF NOT VALID-VIP-VENDA
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-VENDA!"
                            X"0A" "FILES STATUS:" FS-VIP-VENDA
              GO FIM-PROGRAMA
           END-IF.
           OPEN I-O VIP-ESTOQUE.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-ESTOQUE!"
                            X"0A" "FILES STATUS:" FS-VIP-ESTOQUE
              GO FIM-PROGRAMA
           END-IF.
           OPEN I-O VIP-CLIFOR.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-CLIFOR!"
                            X"0A" "FILES STATUS:" FS-VIP-CLIFOR
              GO FIM-PROGRAMA
           END-IF.
       INICIO.
           DISPLAY WINDOW ERASE.
           DISPLAY DIVISORIA.
       ATRIBUIR-VENDA-ID.
           INITIALIZE REG-VENDA.
           MOVE 99                     TO VENDA-ID-VENDA.
           START VIP-VENDA KEY NOT GREATER VENDA-CHAVE-1.
           READ VIP-VENDA  NEXT WITH NO LOCK.
           MOVE VENDA-ID-VENDA         TO WSS-ID-VENDA
           IF NOT VALID-VIP-VENDA
              MOVE 1                   TO WSS-ID-VENDA
           ELSE
              ADD  1                   TO WSS-ID-VENDA
           END-IF.
           MOVE WSS-ID-VENDA           TO VENDA-ID.
           DISPLAY VENDA-ID            AT 0639.
       ACEITA-COD-CLIFOR.
           DISPLAY PRINCIPAL.
           ACCEPT CD-CLIENTE           AT 0820 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              GO FIM-PROGRAMA
           END-IF.
           PERFORM MOVER-CLIFOR-PARA-TELA.
           PERFORM RESULTADO-CLI.
       ACEITA-COD-PROD.
           DISPLAY DIVISORIA.
           DISPLAY PRODUTOS.
           ACCEPT CD-PROD              AT 0852 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              DISPLAY WINDOW ERASE
              DISPLAY PRINCIPAL
              DISPLAY DIVISORIA
              GO ACEITA-COD-CLIFOR
           END-IF.
           PERFORM MOVER-PROD-PARA-TELA.
           PERFORM RESULTADO-PROD.
       ACEITA-QT-PROD.
           DISPLAY PRODUTOS.
           ACCEPT QT-PROD              AT 1248 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              DISPLAY WINDOW ERASE
              DISPLAY PRINCIPAL
              PERFORM RESULTADO-CLI
              DISPLAY PRODUTOS
              GO ACEITA-COD-PROD
           END-IF.
           PERFORM PROD-COM-ESTOQUE.
           DISPLAY CONF-ALTERA.
       CONFIRMA-ALT.
           PERFORM CALCULO-TOTAL.
           ACCEPT CONFIRMA-ALTERA      AT 1852 CONTROL KEY IN KEYSTATUS.
           IF F3-KEY
              GO FIM-NOTA
           ELSE
              IF F2-KEY
                 GO ACEITA-QT-PROD
              ELSE
                 DISPLAY MESSAGE BOX "ERRO"
                 DISPLAY CONF-ALTERA
                 GO CONFIRMA-ALT
              END-IF
           END-IF.

       FIM-NOTA.
           DISPLAY FINALIZA-NOTA.
           ACCEPT FINALIZA AT 2152 CONTROL KEY IN KEYSTATUS.
           IF F3-KEY
              GO ACEITA-COD-PROD
           ELSE
              IF F2-KEY
                 DISPLAY MESSAGE BOX "VENDIDO COM SUCESSO!"
                 MOVE WSS-ID-VENDA     TO VENDA-ID-VENDA
                 READ VIP-VENDA WITH LOCK
                 MOVE CD-CLIENTE       TO VENDA-CD-CLIFOR
                 MOVE CD-PROD          TO VENDA-CD-ESTOQUE
                 MOVE NM-PROD          TO VENDA-NM-ESTOQUE
                 MOVE QT-PROD          TO VENDA-QT-ESTOQUE
                 MOVE VALOR-UNI-PROD   TO VENDA-VALOR-UNI-ESTOQUE
                 MOVE VALOR-TOT-PROD   TO VENDA-VALOR-TOT-ESTOQUE
                 IF VALID-VIP-VENDA
                    REWRITE REG-VENDA
                 ELSE
                    WRITE REG-VENDA
                 END-IF
                 UNLOCK VIP-VENDA
                 IF VALID-VIP-ESTOQUE
                    REWRITE REG-ESTOQUE
                    GO FIM-PROGRAMA
                 ELSE
                    WRITE REG-ESTOQUE
                    GO FIM-PROGRAMA
                 END-IF
              ELSE
                 DISPLAY MESSAGE BOX "ERRO"
                 GO FIM-NOTA
              END-IF
           END-IF.

      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-CLIFOR-PARA-TELA.
           INITIALIZE REG-CLIFOR.
           MOVE CD-CLIENTE             TO CLIFOR-CD-CLIFOR.
           READ VIP-CLIFOR WITH NO LOCK
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO:"
                 X"0A" "FILE STATUS:"FS-VIP-CLIFOR
              GO INICIO
           ELSE
              MOVE CLIFOR-NM-CLIFOR    TO NM-CLIENTE
              MOVE CLIFOR-CPF-CNPJ     TO CPF-CNPJ
              MOVE CLIFOR-CEP          TO CEP
              MOVE CLIFOR-ENDERECO     TO ENDERECO
              MOVE CLIFOR-BAIRRO       TO BAIRRO
              MOVE CLIFOR-CIDADE       TO CIDADE
              MOVE CLIFOR-ESTADO       TO ESTADO
           END-IF.
       RESULTADO-CLI.
           DISPLAY CD-CLIENTE          AT 0820.
           DISPLAY NM-CLIENTE          AT 0918.
           DISPLAY CPF-CNPJ            AT 1014.
           DISPLAY CEP                 AT 1109.
           DISPLAY ENDERECO            AT 1214.
           DISPLAY BAIRRO              AT 1312.
           DISPLAY CIDADE              AT 1412.
           DISPLAY ESTADO              AT 1512.
           GO ACEITA-COD-PROD.
       MOVER-PROD-PARA-TELA.
           INITIALIZE REG-ESTOQUE.
           MOVE CD-PROD                TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE WITH NO LOCK
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO: MOVER-PRODUTO-PARA-TELA"
                            X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              GO INICIO
           ELSE
               MOVE ESTOQUE-CD-PROD    TO CD-PROD
               MOVE ESTOQUE-NM-PROD    TO NM-PROD
               MOVE ESTOQUE-DESC-PROD  TO DESC-PROD
               MOVE ESTOQUE-VALOR-PROD TO VALOR-UNI-PROD
           END-IF.
       PROD-COM-ESTOQUE.
           INITIALIZE REG-ESTOQUE.
           MOVE CD-PROD                TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE WITH NO LOCK
           IF QT-PROD > ESTOQUE-QT-PROD
              DISPLAY MESSAGE BOX "PRODUTO SEM ESTOQUE"
              GO ACEITA-QT-PROD
           ELSE
              SUBTRACT QT-PROD FROM ESTOQUE-QT-PROD
           END-IF.
       RESULTADO-PROD.
           DISPLAY CD-PROD             AT 0852.
           DISPLAY NM-PROD             AT 0950.
           DISPLAY DESC-PROD           AT 1047.
           DISPLAY VALOR-UNI-PROD      AT 1152.
       CALCULO-TOTAL.
           COMPUTE VALOR-TOT-PROD       = QT-PROD
                                        * VALOR-UNI-PROD
           END-COMPUTE.
           DISPLAY SOMA-TOTAL.
           DISPLAY VALOR-TOT-PROD      AT 1443.
      *=================================================================
       FIM-PROGRAMA.
           CLOSE VIP-VENDA.
           CLOSE VIP-ESTOQUE.
           CLOSE VIP-CLIFOR.
           EXIT PROGRAM.
           STOP RUN.
