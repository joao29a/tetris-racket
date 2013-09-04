#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require "tetra-tipos.rkt")
(require "base.rkt")
(require "tetris.rkt")

;; Constantes usadas nos testes
(define TIMEOUT 14)

(define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
(define TT1_POS_DIREITA (tetramino T_TIPOS 1 (posn 1 1) T_COR))
(define TT1_POS_BAIXO (tetramino T_TIPOS 1 (posn 2 0) T_COR))
(define TT1_ROTACIONADO (tetramino T_TIPOS 2 (posn 1 0) T_COR))
(define TT1_POS (list (posn 1 1)
                      (posn 2 1) (posn 2 2)
                      (posn 3 1)))
(define TT1_CENTRA_10 (tetramino T_TIPOS 1 (posn 1 3) T_COR))

(define TZ2 (tetramino Z_TIPOS 2 (posn 2 3) Z_COR))
(define TZ2_POS_ESQUERDA (tetramino Z_TIPOS 2 (posn 2 2) Z_COR))
(define TZ2_ROTACIONADO (tetramino Z_TIPOS 3 (posn 2 3) Z_COR))
(define TZ2_POS (list (posn 3 3) (posn 3 4)
                      (posn 4 4) (posn 4 5)))
(define TZ2_CENTRA_15 (tetramino Z_TIPOS 2 (posn 2 6) Z_COR))

(define TI0 (tetramino I_TIPOS 0 (posn -1 1) I_COR))
(define TI0_POS (list (posn 0 1) (posn 0 2) (posn 0 3) (posn 0 4)))
(define TI0_CENTRA_12 (tetramino I_TIPOS 0 (posn -1 4) I_COR))

(define T03 (tetramino O_TIPOS 0 (posn 2 3) O_COR))
(define T03_ROTACIONADO (tetramino O_TIPOS 0 (posn 2 3) O_COR))

(define TS4 (tetramino S_TIPOS 3 (posn 0 1) S_COR))
(define TS4_ROTACIONADO (tetramino S_TIPOS 0 (posn 0 1) S_COR))

(define C1 (list (list 0 0 0 0 0 0 0)   ; 0
                 (list 0 0 0 0 0 0 0)   ; 1
                 (list 6 0 0 0 0 0 0)   ; 2
                 (list 4 0 2 4 6 1 1)   ; 3
                 (list 3 4 0 0 0 0 0)   ; 4
                 (list 1 2 4 3 2 5 6))) ; 5
                 ;     0 1 2 3 4 5 6

(define C1_LARGURA 7)
(define C1_ALTURA 6)
;; algumas posições ocupadas em C1
(define C1_OCUPADAS (list (posn 2 0) (posn 3 2) (posn 4 1)))
;; algumas posições livres em C1
(define C1_LIVRES (list (posn 0 0) (posn 3 1) (posn 4 2)))

(define C T_COR)

; Representa C1 com o tetraminó TT1 fixado no campo
(define C1_FIXA_TT1 (list (list 0 0 0 0 0 0 0)   ; 0
                          (list 0 C 0 0 0 0 0)   ; 1
                          (list 6 C C 0 0 0 0)   ; 2
                          (list 4 C 2 4 6 1 1)   ; 3
                          (list 3 4 0 0 0 0 0)   ; 4
                          (list 1 2 4 3 2 5 6))) ; 5
                          ;     0 1 2 3 4 5 6

; Representa C1_FIXA_TT1 sem as linha completas
(define C1_FIXA_TT1_LIMPA (list (list 0 0 0 0 0 0 0)   ; 0
                                (list 0 0 0 0 0 0 0)   ; 1
                                (list 0 0 0 0 0 0 0)   ; 2
                                (list 0 C 0 0 0 0 0)   ; 3
                                (list 6 C C 0 0 0 0)   ; 4
                                (list 3 4 0 0 0 0 0))) ; 5
                                ;     0 1 2 3 4 5 6

(define C2 (list (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)))

(define C2_LARGURA 5)
(define C2_ALTURA 7)

(define set-tetris-timeout-tests
  (test-suite
   "set-tetris-timeout"
   (check-equal? (set-tetris-timeout empty 0) empty)
   (check-equal? (tetris-timeout (set-tetris-timeout (make-tetris-padrao) 0)) 
                 0)
   (check-equal? (tetris-timeout (set-tetris-timeout (make-tetris-padrao) 19)) 
                 19)
   (check-equal? (tetris-timeout (set-tetris-timeout (make-tetris-padrao) 100)) 
                 100)))

(define calc-new-timeout-tests
  (test-suite
   "calc-new-timeout"
   (check-equal? (calc-new-timeout 0) 1)
   (check-equal? (calc-new-timeout 5) 6)
   (check-equal? (calc-new-timeout 28) 0)
   (check-equal? (calc-new-timeout 100) 0)))

(define desenhar-campo-tests
  (test-suite
   "desenhar-campo tests"
   (check-equal? (desenhar-campo empty Q-LARGURA Q-ALTURA) BLANK)
   (check-equal? (desenhar-campo (list (list 0)) Q-LARGURA Q-ALTURA) 
                 (rectangle Q-LARGURA Q-ALTURA "solid" "black"))
   (check-equal? (desenhar-campo (list (list 1 0)
                                       (list 0 1)) 10 10)
                 (above (beside (rectangle 10 10 "solid" "cyan")
                                (rectangle 10 10 "solid" "black"))
                        (beside (rectangle 10 10 "solid" "black")
                                (rectangle 10 10 "solid" "cyan"))))))

(define desenhar-linha-tests
  (test-suite
   "desenhar-linha tests"
   (check-equal? (desenhar-linha empty Q-LARGURA Q-ALTURA) BLANK)
   (check-equal? (desenhar-linha (list 0) Q-LARGURA Q-ALTURA) 
                 (rectangle Q-LARGURA Q-ALTURA "solid" "black"))
   (check-equal? (desenhar-linha (list 3 1 4 5) Q-LARGURA Q-ALTURA) 
                 (beside (rectangle Q-LARGURA Q-ALTURA "solid" "orange")
                         (rectangle Q-LARGURA Q-ALTURA "solid" "cyan")
                         (rectangle Q-LARGURA Q-ALTURA "solid" "yellow")
                         (rectangle Q-LARGURA Q-ALTURA "solid" "green")))))

(define make-linha-tests
  (test-suite
   "make-linha tests"
   (check-equal? (make-linha 0) empty)
   (check-equal? (make-linha 5) (list 0 0 0 0 0))))

(define make-campo-tests
  (test-suite
   "make-campo tests"
   (check-equal? (make-campo C2_LARGURA C2_ALTURA) C2)))

(define centraliza-tests
  (test-suite
   "centraliza tests"
   (check-equal? (centraliza TT1 10)
                 TT1_CENTRA_10)
   (check-equal? (centraliza TZ2 15)
                 TZ2_CENTRA_15)
   (check-equal? (centraliza TI0 12)
                 TI0_CENTRA_12)))

(define make-tetris-tests
  (test-suite
   "make-tetris tests"
   (check-equal? (make-tetris C2_LARGURA C2_ALTURA (list TT1 TZ2 TI0) TIMEOUT)
                 (tetris C2
                         C2_LARGURA
                         C2_ALTURA
                         (centraliza TT1 C2_LARGURA)
                         (list TZ2 TI0)
                         TIMEOUT))))

(define tetramino->pos-tests
  (test-suite
   "tetramino->pos tests"
   (check-equal? (tetramino->lista-pos TT1) TT1_POS)
   (check-equal? (tetramino->lista-pos TZ2) TZ2_POS)
   (check-equal? (tetramino->lista-pos TI0) TI0_POS)))

(define lop-validas?-tests
  (test-suite
   "lop-validas? tests"
   (check-equal? (lop-validas? empty 5 8)
                 #t)
   ;; testa os extremos
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn (sub1 C1_ALTURA) 0)
                                     (posn 0 (sub1 C1_LARGURA))
                                     (posn (sub1 C1_ALTURA) (sub1 C1_LARGURA)))
                               C1_LARGURA
                               C1_ALTURA)
                 #t)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn C1_ALTURA 0) ; linha inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn  2 3)
                                     (posn -1 3)) ; linha inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 0 C1_LARGURA) ; coluna inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 1 -1)) ; coluna inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)))


(define lop-livres?-tests
  (test-suite
   "lop-livres? tests"
   (check-equal? (lop-livres? C1_LIVRES C1) #t)
   (check-equal? (lop-livres? C1_OCUPADAS C1) #f)
   (check-equal? (lop-livres? (append C1_LIVRES (list (first C1_OCUPADAS))) C1) #f)))



(define fixa-tests
  (test-suite
   "fixa tests"
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))))

(define addEmptysLinesNoTopo-tests
  (test-suite
   "addEmptysLinesNoTopo testes"
   (check-equal? (addEmptysLinesNoTopo 2 (list '(1 0) '(0 1)) 2) (list '(0 0) '(0 0) '(1 0) '(0 1)))   
   (check-equal? (addEmptysLinesNoTopo 0 (list '(1 0) '(0 1)) 2) (list '(1 0) '(0 1)))
   (check-equal? (addEmptysLinesNoTopo 2 empty 2) (list '(0 0) '(0 0) ))))

(define fullLine?-tests 
  (test-suite
   "fullLine tests"
   (check-equal? (fullLine? '(1 1 1 1 0 1 1)) #f)
   (check-equal? (fullLine? '(1 1 1 1 1 1 1 1)) #t)))

(define limpa-tests
  (test-suite
   "limpa tests"
   (check-equal? (limpa (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1_LIMPA C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))))

(define rotacionar-tests
  (test-suite
   "rotacionar tests"
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_ROTACIONADO empty TIMEOUT))
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TZ2 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TZ2_ROTACIONADO empty TIMEOUT))
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA T03 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA T03_ROTACIONADO empty TIMEOUT))
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TS4 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TS4_ROTACIONADO empty TIMEOUT))))
   
(define mover-direita-tests
  (test-suite
   "mover-direita tests"
   (check-equal? (mover-direita (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_POS_DIREITA empty TIMEOUT))))

(define mover-esquerda-tests
  (test-suite
   "mover-esquerda tests"
   (check-equal? (mover-esquerda (tetris C2 C2_LARGURA C2_ALTURA TZ2 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TZ2_POS_ESQUERDA empty TIMEOUT))))

(define mover-baixo-tests
  (test-suite
   "mover-baixo tests"
   (check-equal? (mover-baixo (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_POS_BAIXO empty TIMEOUT))))
;; ---------------------------------------------------------------------

;; Função que executa um grupo de testes.
(define (executar-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executar-testes make-linha-tests
                 make-campo-tests
                 centraliza-tests
                 make-tetris-tests
                 tetramino->pos-tests
                 lop-validas?-tests
                 lop-livres?-tests
                 fixa-tests
                 addEmptysLinesNoTopo-tests
                 limpa-tests
                 set-tetris-timeout-tests
                 calc-new-timeout-tests
                 desenhar-campo-tests
                 desenhar-linha-tests
                 rotacionar-tests
                 mover-direita-tests
                 mover-esquerda-tests
                 mover-baixo-tests)
