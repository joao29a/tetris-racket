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
         calc-new-timeout
         desenhar-campo
         desenhar-linha
         addEmptysLinesNoTopo
         fullLine?
         rotacionar
         mover-direita
         mover-esquerda
         mover-baixo
         estaJogando?)

(define (estaJogando? jogo)
  (< (tetris-jogando? jogo) 0))
;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO
               INIT-JOGO INIT-PONTUACAO INIT-LEVEL INIT-LINHAS NOT-ACABOU))

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
  (if (estaJogando? jogo) 
      (cond [(key=? tecla "right") (mover-direita jogo)]
            [(key=? tecla "left") (mover-esquerda jogo)]
            [(key=? tecla "up") (rotacionar jogo)]
            [(key=? tecla "down") (mover-baixo jogo)]
            [(key=? tecla " ") (mover-baixo (mover-direto-para-baixo jogo))]
            [(key=? tecla "r") (make-tetris-padrao)]
            [else jogo])
      jogo))

;; Move um tetramino em relação a coluna em uma unidade
(define (mover-horizontal direcao jogo)
  (define tetra (tetris-tetra jogo))
  (define pos (tetramino-pos tetra))
  (define nova-pos (struct-copy posn pos (col (direcao (posn-col pos)))))
  (define tetramino-nova-pos (struct-copy tetramino tetra (pos nova-pos)))
  (nao-mexe-se-colidiu tetramino-nova-pos jogo))

(define (mover-direita jogo)
  (mover-horizontal add1 jogo))

(define (mover-esquerda jogo)
  (mover-horizontal sub1 jogo))

(define (mover-baixo jogo)
  (cond
    [(empty? jogo) empty]
    [else (define tetra (tetris-tetra jogo))
          (define pos (tetramino-pos tetra))
          (define novoTetra (struct-copy tetramino tetra
                                         [pos (posn (add1 (posn-lin pos))
                                                    (posn-col pos))]))
          (fixa-se-colidiu novoTetra jogo)]))

(define (mover-direto-para-baixo jogo)
  (cond
    [(empty? jogo) empty]
    [else 
     (define tetra (tetris-tetra jogo))
     (define jogoMovido (mover-baixo jogo))
     (if (equal? (tetris-campo jogo) (tetris-campo jogoMovido))
         (mover-direto-para-baixo jogoMovido)
         jogo)]))

(define (rotacionar jogo)
  (define tetra (tetris-tetra jogo))
  (define len (length (tetramino-tipo tetra)))
  (define rot (tetramino-rot tetra))
  (define (novo-tetra-rot x)
    (struct-copy tetramino tetra (rot x)))
  (nao-mexe-se-colidiu (if (= rot (sub1 len))
               (novo-tetra-rot 0)
               (novo-tetra-rot (add1 rot)))
           jogo))

(define (colidiu? tetra jogo) 
  (define blocosOcupados (tetramino->lista-pos tetra))
  (not (and (lop-validas? blocosOcupados LARGURA-PADRAO ALTURA-PADRAO)
           (lop-livres? blocosOcupados (tetris-campo jogo)))))

(define (nao-mexe-se-colidiu tetra jogo) 
  (if (colidiu? tetra jogo)
      jogo
      (struct-copy tetris jogo [tetra tetra])))

(define (fixa-se-colidiu tetra jogo)
  (if (colidiu? tetra jogo)
      (proximoTetra (fixa jogo))
      (struct-copy tetris jogo [tetra tetra])))

;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois printf "d"que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.
(define (set-tetris-timeout jogo newValue) 
  (cond
   [(empty? jogo) empty]
   [else (define pontos (tetris-pontuacao jogo))
         (define level  (tetris-level jogo))
         (define novoLevel (if (> pontos (* AUMENTO-LEVEL level)) 
                           (add1 level)
                           level))
         (define novoPontos (if (> pontos (* AUMENTO-LEVEL level)) 
                           0
                           pontos))
         (struct-copy tetris jogo (timeout newValue) 
                      (level novoLevel) 
                      (pontuacao novoPontos))])) 

(define (calc-new-timeout timeout level) 
  (if (>= timeout (quotient TIMEOUT-PADRAO level))
      0
      (add1 timeout)))

;; TODO! 
;; Quando ele se encaixar quem vai checar isso
;; é o trata-tick ou mover-abaixo?
(define (trata-tick jogo)
  (cond [(tetris-fim jogo) jogo]
        [else
         (define jogo-limpo (limpa (game-over jogo)))
         (define timeout (tetris-timeout jogo-limpo))
         (define level (tetris-level jogo))
         (define newTimeout (calc-new-timeout timeout level))
         (define jogoWithNewTimeout (set-tetris-timeout jogo-limpo newTimeout))
         (cond
           [(estaJogando? jogo)
            (if (= newTimeout 0)
                (mover-baixo jogoWithNewTimeout)
                jogoWithNewTimeout)]
           [else (struct-copy tetris jogo (jogando? (sub1 (tetris-jogando? jogo))))])]))

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.

;; fazer testes
(define (desenhar-tetra tetra) 
  (define (lop-desenha lop cor)
   (cond
     [(empty? lop) BLANK]
     [else 
      (define pos (first lop))
      (overlay/xy 
       (overlay BLOCO-BORDA
                (rectangle Q-LARGURA Q-ALTURA "solid" 
                           (list-ref CORES cor)))
       (- (* (posn-col pos) Q-LARGURA)) (- (* (posn-lin pos) Q-ALTURA))
       (lop-desenha (rest lop) cor))]))
  (cond  
    [(empty? tetra) BLANK]
    [else
           (define cor (tetramino-cor tetra))
           (lop-desenha (tetramino->lista-pos tetra) cor)]))

(define (desenha-placar pergunta resposta size color)
  (beside (text pergunta size color)
          (text (number->string resposta) size color)))

(define (desenhar-textos jogo)
  (overlay/align "left" "top" 
   (above (overlay 
           (desenhar-tetra (stream-first (tetris-proximos jogo)))
           EMPTY-RECTANGLE)
          (desenha-placar "Pontuação: " (tetris-pontuacao jogo) FONT-SIZE FONT-COLOR)
          (desenha-placar "Level: " (tetris-level jogo) FONT-SIZE FONT-COLOR)
          (desenha-placar "Linha: " (tetris-linhas jogo) FONT-SIZE FONT-COLOR))
   (desenha-tela "Chocolate")))

(define (desenha-tela cor)
  (rectangle (+ (* Q-LARGURA LARGURA-PADRAO) EMPTY-RECTANGLE-SIZE) 
                      (* Q-ALTURA ALTURA-PADRAO) "solid" cor))

(define (desenha-game-over jogo)
  (overlay (above (text "Fim de Jogo." 50 "red")
                  (text "Pressione r para reiniciar." 25 "Pale Green")
                  (desenha-placar "Pontuação: " (tetris-pontuacao jogo) 25 "white")
                  (desenha-placar "Level: " (tetris-level jogo) 25 "white")
                  (desenha-placar "Linhas: " (tetris-linhas jogo) 25 "white"))
           (desenha-tela "black")))


(define (tela-inicial n)
  (overlay (above (text (number->string (quotient n 30)) 100 "white")
                  (text "Criado por" 30 "Pale Green")
                  (text "João A. Jesus Jr." 25 "RoyalBlue")
                  (text "Vanderson M. do Rosario" 25 "OrangeRed"))
           (desenha-tela "black")))

(define (desenha jogo)
  (if (and (estaJogando? jogo) (not (tetris-fim jogo)))
      (overlay/align
       "left" "top"
       (desenhar-tetra (tetris-tetra jogo))
       (desenhar-tetra (struct-copy tetramino (tetris-tetra (mover-direto-para-baixo jogo)) [cor 8]))
       (beside/align "top" (desenhar-campo (tetris-campo jogo) 
                                           Q-LARGURA 
                                           Q-ALTURA)
                     (desenhar-textos jogo)))
       (if (tetris-fim jogo) 
           (desenha-game-over jogo)
           (tela-inicial (tetris-jogando? jogo)))))

(define (desenhar-campo campo largura altura)
  (cond [(empty? campo) BLANK]
        [else (above (desenhar-linha (first campo) largura altura)
                     (desenhar-campo (rest campo) largura altura))]))


(define (desenhar-linha linha largura altura)
  (cond [(empty? linha) BLANK]
        [else 
         (let ([bloco (rectangle largura altura "solid" (list-ref CORES (first linha)))])
           (beside (if (> (first linha) 0)
                          (overlay BLOCO-BORDA
                                   bloco)
                          bloco)
                   (desenhar-linha (rest linha) largura altura)))]))

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
  (define tipo (tetramino-tipo t))
  (define tipo-rotacao (list-ref tipo (tetramino-rot t)))
  (contar-posn-lin tipo-rotacao (tetramino-pos t) 0 0))

;; Percorre todas as linhas, verificando coluna por coluna.
(define (contar-posn-lin tipo-rotacao offset linha coluna)
  (cond [(empty? tipo-rotacao) empty]
        [else (append (contar-posn-col (first tipo-rotacao) offset linha coluna)
                    (contar-posn-lin (rest tipo-rotacao) offset (add1 linha) coluna))]))

;; Pega uma linha e percorre as colunas, se encontrar 1, cria uma estrutura posn
(define (contar-posn-col lst-linha offset linha coluna)
  (cond [(empty? lst-linha) empty]
        [else
         (let ([incrementar-coluna (contar-posn-col (rest lst-linha) offset linha (add1 coluna))])
           (if (= (first lst-linha) 1) 
             (cons (posn (+ (posn-lin offset) linha) (+ (posn-col offset) coluna))
               incrementar-coluna)
             incrementar-coluna))]))
               
;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.
(define (lop-validas? lp largura altura)
  (define (valido? pos) 
    (define lin (posn-lin pos))
    (define col (posn-col pos))
    (and 
     (and (< lin altura) (>= lin 0))
     (and (< col largura) (>= col 0))))
 (cond 
    [(empty? lp) #t]
    [else (foldl (λ (x y) (and (valido? x) y)) #t lp)]))

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas?.
(define (lop-livres? lp campo)
  (define (livre? pos)
    (define lin (posn-lin pos))
    (define col (posn-col pos))
    (= (list-ref (list-ref campo lin) col) 0))
  (cond 
    [(empty? campo) #f]
    [else (foldl (λ (x y) (and (livre? x) y)) #t lp)]))

;; Jogo -> Jogo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.
(define (adicionarTetraminoNoCampo campo list-posn cor)
  (cond [(empty? list-posn) campo]
        [else
         (define lin (posn-lin (first list-posn)))
         (define col (posn-col (first list-posn)))
         (define first-campo (take campo lin))
         (define rest-campo (drop campo lin))
         (define first-line (take (first rest-campo) col))
         (define rest-line (drop (first rest-campo) col))
         (define novo-campo (append first-campo
                                    (cons (append first-line
                                                    (cons cor (rest rest-line)))
                                            (rest rest-campo))))
         (adicionarTetraminoNoCampo novo-campo (rest list-posn) cor)]))


(define (fixa jogo)
  (cond 
    [(empty? jogo) empty]
    [else
     (define campo (tetris-campo jogo))
     (define cor (tetramino-cor (tetris-tetra jogo)))
     (define list-posn (tetramino->lista-pos (tetris-tetra jogo)))
     (struct-copy tetris jogo (campo (adicionarTetraminoNoCampo campo list-posn cor)))]))

;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.
(define (emptyLine n) (build-list n (λ (x) 0)))

(define (fullLine? linha) 
  (cond 
    [(empty? linha) #t]
    [else (and (if (= (first linha) 0)
              #f
              #t) 
               (fullLine? (rest linha)))]))

(define (addEmptysLinesNoTopo x campo len) 
  (cond
    [(= x 0) campo]
    [else 
     (addEmptysLinesNoTopo (sub1 x) 
                                   (append (list (emptyLine len)) campo) len)]))

(define (limpa jogo) 
  (cond 
    [(empty? jogo) empty]
    [else 
      (define campo (tetris-campo jogo))
      (define campoSemLinhasCheias (filter-not fullLine? campo))
      (define numLinhasCheias (- (length campo) 
                                 (length campoSemLinhasCheias)))
      (define len (length (first campo)))
      (define (novo-jogo jogo) 
        (struct-copy tetris jogo [campo (addEmptysLinesNoTopo numLinhasCheias campoSemLinhasCheias len)]))
      (if (not (zero? numLinhasCheias))
          (novo-jogo (struct-copy tetris jogo (pontuacao 
                                               (+ (+ (* PONTOS numLinhasCheias) (tetris-level jogo))
                                                  (tetris-pontuacao jogo)))
                                  (linhas (+ (tetris-linhas jogo) numLinhasCheias))))
          (novo-jogo jogo))]))

(define (game-over jogo)
  (define tetra (tetris-tetra jogo))
  (define tetra-centro (centraliza tetra (tetris-largura jogo)))
  (if (equal? tetra-centro tetra)
      (estorou-campo? tetra jogo)
      jogo))

(define (estorou-campo? tetra jogo)
  (cond
    [(colidiu? tetra jogo)
      (struct-copy tetris jogo (fim #t))]
    [else jogo]))
  
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

(define (proximoTetra jogo) 
  (define proximos (tetris-proximos jogo))
  (struct-copy tetris jogo 
               [tetra (centraliza (stream-first proximos) LARGURA-PADRAO)]
               [proximos (stream-append (stream-rest proximos) (criar-lista-tetraminos 1))]))
