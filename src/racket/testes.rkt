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

(define C2_TT1 (list (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 6 0 0 0)
                 (list 0 6 6 0 0)
                 (list 0 6 0 0 0)))

(define C3 (list (list 1 1 1 1 1)
                 (list 1 1 1 1 1)
                 (list 1 1 1 1 1)
                 (list 1 1 1 1 1)
                 (list 1 1 1 1 1)
                 (list 1 1 1 1 1)
                 (list 1 1 1 1 1)))

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

(define calc-novo-timeout-tests
  (test-suite
   "calc-novo-timeout"
   (check-equal? (calc-novo-timeout 0 INIT-LEVEL) 1)
   (check-equal? (calc-novo-timeout 5 INIT-LEVEL) 6)
   (check-equal? (calc-novo-timeout 28 INIT-LEVEL) 0)
   (check-equal? (calc-novo-timeout 100 INIT-LEVEL) 0)))

(define desenhar-campo-tests
  (test-suite
   "desenhar-campo tests"
   (check-equal? (desenhar-campo empty Q-LARGURA Q-ALTURA) BLANK)
   (check-equal? (desenhar-campo (list (list 0)) Q-LARGURA Q-ALTURA) 
                 (rectangle Q-LARGURA Q-ALTURA "solid" "black"))
   (check-equal? (desenhar-campo (list (list 1 0)
                                       (list 0 1)) 10 10)
                 (above (beside (overlay BLOCO-BORDA
                                         (rectangle 10 10 "solid" "cyan"))
                                (rectangle 10 10 "solid" "black"))
                        (beside (rectangle 10 10 "solid" "black")
                                (overlay BLOCO-BORDA
                                         (rectangle 10 10 "solid" "cyan")))))))

(define desenhar-linha-tests
  (test-suite
   "desenhar-linha tests"
   (check-equal? (desenhar-linha empty Q-LARGURA Q-ALTURA) BLANK)
   (check-equal? (desenhar-linha (list 0) Q-LARGURA Q-ALTURA) 
                 (rectangle Q-LARGURA Q-ALTURA "solid" "black"))
   (check-equal? (desenhar-linha (list 3 1 4 5 0) Q-LARGURA Q-ALTURA) 
                 (beside 
                  (overlay BLOCO-BORDA
                           (rectangle Q-LARGURA Q-ALTURA "solid" "orange"))
                  (overlay BLOCO-BORDA
                           (rectangle Q-LARGURA Q-ALTURA "solid" "cyan"))
                  (overlay BLOCO-BORDA
                           (rectangle Q-LARGURA Q-ALTURA "solid" "yellow"))
                  (overlay BLOCO-BORDA
                           (rectangle Q-LARGURA Q-ALTURA "solid" "green"))
                  (rectangle Q-LARGURA Q-ALTURA "solid" "black")))))
                  
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
   (check-equal? (make-tetris C2_LARGURA C2_ALTURA (list TT1 TZ2 TI0) TIMEOUT INIT-JOGO 
                              INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)
                 (tetris C2
                         C2_LARGURA
                         C2_ALTURA
                         (centraliza TT1 C2_LARGURA)
                         (list TZ2 TI0)
                         TIMEOUT INIT-JOGO INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))

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
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))

(define add-linhas-vazias-no-topo-tests
  (test-suite
   "add-linhas-vazias-no-topo testes"
   (check-equal? (add-linhas-vazias-no-topo 2 (list '(1 0) '(0 1)) 2) (list '(0 0) '(0 0) '(1 0) '(0 1)))   
   (check-equal? (add-linhas-vazias-no-topo 0 (list '(1 0) '(0 1)) 2) (list '(1 0) '(0 1)))
   (check-equal? (add-linhas-vazias-no-topo 2 empty 2) (list '(0 0) '(0 0) ))))

(define fullLine?-tests 
  (test-suite
   "fullLine tests"
   (check-equal? (fullLine? '(1 1 1 1 0 1 1)) #f)
   (check-equal? (fullLine? '(1 1 1 1 1 1 1 1)) #t)))

(define limpa-tests
  (test-suite
   "limpa tests"
   (check-equal? (limpa (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C1_FIXA_TT1_LIMPA C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                         160 INIT-LEVEL 2 NOT-ACABOU))))

(define rotacionar-tests
  (test-suite
   "rotacionar tests"
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                     INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_ROTACIONADO empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
   
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TZ2 empty TIMEOUT INIT-JOGO 
                                     INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TZ2_ROTACIONADO empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
   
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA T03 empty TIMEOUT INIT-JOGO 
                                     INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA T03_ROTACIONADO empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
   
   (check-equal? (rotacionar (tetris C2 C2_LARGURA C2_ALTURA TS4 empty TIMEOUT INIT-JOGO 
                                     INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TS4_ROTACIONADO empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))
   
(define mover-direita-tests
  (test-suite
   "mover-direita tests"
   (check-equal? (mover-direita (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                        INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_POS_DIREITA empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))

(define mover-esquerda-tests
  (test-suite
   "mover-esquerda tests"
   (check-equal? (mover-esquerda (tetris C2 C2_LARGURA C2_ALTURA TZ2 empty TIMEOUT INIT-JOGO 
                                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TZ2_POS_ESQUERDA empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))

(define mover-baixo-tests
  (test-suite
   "mover-baixo tests"
   (check-equal? (mover-baixo (tetris C2 C2_LARGURA C2_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                      INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
                 (tetris C2 C2_LARGURA C2_ALTURA TT1_POS_BAIXO empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))

(define estorou-campo?-tests
  (test-suite
   "estorou-campo?-Tests"
   (check-equal? (estorou-campo? TT1 (tetris C3 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) 
                 (tetris C3 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS #t))
   (check-equal? (estorou-campo? TT1 (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) 
                 (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))


(define game-over-tests
  (test-suite
   "game-over?-tests"
   (check-equal? (game-over (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) 
                 (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))
   (check-equal? (game-over (tetris C3 C1_LARGURA C1_ALTURA (centraliza TT1 C1_LARGURA) 
                                empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) 
                 (tetris C3 C1_LARGURA C1_ALTURA (centraliza TT1 C1_LARGURA)  empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS #t))))

;; Teste de pontuação apenas checa se a pontuação está aumentando
;; Devido a grande quantidade de mudanças na regra de pontuação 
;; O importante é que o sistema aumente e não quanto ou como
(define jogo (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                                INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) 
(define pontuacao-tests
  (test-suite
   "pontuacao-tests"
   (check-true (> (tetris-pontuacao (pontuacao jogo 1)) (tetris-pontuacao jogo))) 
   (check-true (= (tetris-pontuacao (pontuacao jogo 0)) (tetris-pontuacao jogo))))) 


(define fixa-se-colidiu-tests
  (test-suite 
   "fixa-se-colidiu-tests"
   (check-equal?  (tetris-campo (fixa-se-colidiu TT1_POS_BAIXO (tetris C1 C1_LARGURA C1_ALTURA TT1 (stream-tetraminos) TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)))
                 (tetris-campo (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 (stream-tetraminos) TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)))
   (check-equal?  (tetris-campo (fixa-se-colidiu TT1_POS_BAIXO (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)))
                  (tetris-campo (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                         INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)))))


(define colidiu?-tests
  (test-suite 
   "fixa-se-colidiu-tests"
   (check-equal? (colidiu? TT1_POS_BAIXO (tetris C2 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) #f)
  (check-equal? (colidiu? TT1_POS_BAIXO (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU)) #t))) 

(define mover-direto-para-baixo-tests
  (test-suite 
   "fixa-se-colidiu-tests"
   (check-equal? (tetris-campo (fixa (mover-direto-para-baixo (tetris C2 C2_LARGURA C2_ALTURA TT1 (stream-tetraminos) TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))  
                 (tetris-campo (mover-direto-para-baixo (tetris C2_TT1 C2_LARGURA C2_ALTURA TT1 (stream-tetraminos) TIMEOUT INIT-JOGO 
                               INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))))))


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
                 add-linhas-vazias-no-topo-tests
                 limpa-tests
                 set-tetris-timeout-tests
                 calc-novo-timeout-tests
                 desenhar-campo-tests
                 desenhar-linha-tests
                 rotacionar-tests
                 mover-direita-tests
                 mover-esquerda-tests
                 mover-baixo-tests
                 pontuacao-tests
                 estorou-campo?-tests
                 game-over-tests
                 fixa-se-colidiu-tests
                 colidiu?-tests
                 mover-direto-para-baixo-tests)
