#lang racket

;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         lop-livres?
         fixa
         limpa
         trata-tecla
         trata-tick
         desenha
         set-tetris-timeout
         calc-new-timeout)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (jogar-tetris (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO)))

;; Função recursiva que vai ficar atualizando toda vez o jogo
(define (jogar-tetris jogo) jogo)

;; Jogo String -> Jogo
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up"
;; e "down".

;; fazer teste
(define (trata-tecla jogo tecla)
  (cond [(key=? tecla "right") (mover-direita jogo)]
        [(key=? tecla "left") (mover-esquerda jogo)]
        [(key=? tecla "up") (rotacionar jogo)]
        [(key=? tecla "down") (mover-baixo jogo)]
        [else jogo]))

;; Move um tetramino em relação a coluna em uma unidade
;; fazer teste
(define (mover direcao jogo)
  (define tetra (tetris-tetra jogo))
  (define pos (tetramino-pos tetra))
  (define nova-pos (struct-copy posn pos (col (direcao (posn-col pos)))))
  (define tetramino-nova-pos (struct-copy tetramino tetra (pos nova-pos)))
  (struct-copy tetris jogo (tetra tetramino-nova-pos)))

;; fazer teste
(define (mover-direita jogo)
  (mover add1 jogo))

;; fazer teste
(define (mover-esquerda jogo)
  (mover sub1 jogo))

(define (mover-baixo jogo) jogo)

(define (rotacionar jogo) jogo)

;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois printf "d"que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.
(define (set-tetris-timeout jogo newValue) 
  (cond
   [(empty? jogo) empty]
   [else (struct-copy tetris jogo (timeout newValue))])) 

(define (calc-new-timeout timeout) 
  (if (>= timeout TIMEOUT-PADRAO)
      0
      (add1 timeout)))

;; TODO! 
;; Quando ele se encaixar quem vai checar isso
;; é o trata-tick ou mover-abaixo?
(define (trata-tick jogo)
  (printf "t")
  (define timeout (tetris-timeout jogo))
  (define newTimeout (calc-new-timeout timeout))
  (define jogoWithNewTimeout (set-tetris-timeout jogo newTimeout))
  (if (= newTimeout 0)
      (mover-baixo jogoWithNewTimeout)
      jogoWithNewTimeout))

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.

;; fazer testes
(define (desenha jogo)
  (desenhar-campo (tetris-campo jogo) 
                  (tetris-largura jogo) 
                  (tetris-altura jogo)))

;; fazer testes
(define (desenhar-campo campo largura altura)
  (cond [(empty? campo) (rectangle 0 0 "solid" "black")]
        [else (above (desenhar-linha (first campo) largura altura)
                     (desenhar-campo (rest campo) largura altura))]))

;; fazer testes
(define (desenhar-linha linha largura altura)
  (cond [(empty? linha) (rectangle 0 0 "solid" "black")]
        [else (beside (rectangle largura altura "solid" 
                                 (list-ref CORES (first linha)))
                      (desenhar-linha (rest linha) largura altura))]))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definina em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (pos 1 1) (posn 2 0) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (pos 1 1) (pos 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.
(define (tetramino->lista-pos t)
  empty)

;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.
(define (lop-validas? lp largura altura) false)

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.
(define (lop-livres? lp campo) false)

;; Jogo -> Jogo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.
(define (fixa jogo) jogo)

;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.
(define (limpa jogo) jogo)

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
(define (stream-tetraminos)
  (criar-lista-tetraminos 10))

(define (criar-lista-tetraminos n)
  (cond [(zero? n) empty-stream]
        [else (stream-cons (list-ref TETRAMINOS (random 7))
                           (criar-lista-tetraminos (sub1 n)))]))