# reduction.rktの独創性

## TODO

実は，Wadler の Expression problem への解の一つになっているのでは？
struct(データ)の修正・拡張も，コードの修正・拡張も additive (モジュラー)に
行える．

アスペクト指向，文脈指向などなどとの比較も必要？

## 動機

redexは特にnon-deterministic operational semanticsを書く道具として
すぐれている．応用例：AAM．
しかし，次のような問題があり，practicalな実装をつくるベースライブラリ
としては向いていない．

1. redexは遅い．せっかくモデルを定義して形式化したのに，ちょっとサイズの大きな
  ベンチマークを試そうとしたらすごく時間がかかる．．．
2. redexで書くと拡張性がない

そこで，本物のツールをつくるのに使えるredexライクな軽量reduction engine
を提案．

1. 高速
2. 拡張性

がウリ．1.はパターンの柔軟性と性能は本質的なトレードオフ．
提案では柔軟性は犠牲にし，標準のmatchで我慢することで遅くなるのを回避．
柔軟性がある程度そこなわれても，通常のセマンティクスであれば表現力は
十分であることをAIで例示．

:::warning
π計算をもう一つの例にすると，柔軟性を犠牲にしたことのコストも議論できる．
あるいは，標準matchと適当なマクロでπ計算もなんとか読みやすいように記述
する方法をあみだせる？
:::

## 拡張性

まず，extended-reduction-relation の問題点．言語の再定義の問題．
super言語を参照しつづける．おそらく「関数定義」として閉じている．

:::warning
TODO: 論文書くには，extended-reduction-relationがどう実装されているかを
いちおう確認しておくべき？redex/paramと違って，振舞いから予測するだけでも
いい？
:::

redex/param は，おそらく，動的変数をつかってある程度拡張性を確保している．
しかし，それでも限界がある．(machine/以下のモデル巻に重複定義が残っている．)
また，とくに相互再帰な定義に使えない．

:::danger
TODO: 論文書くには，redex/paramの実装をコードを読んで確認しないと．
:::


提案手法は，マクロによるclauseの展開，renamingで実装しているので，
「親定義で閉じることがなく」，プログラマがイメージしているとおりの直観的な
拡張が可能．
**必ずしも何でもかんでもレキシカルスコープの方が分かりやすいとは限らない
好例であると言える．かつ，redex/paramでできないことからも分かるとおり，
レキシカルスコープの回避策としての動的束縛変数にもやれることの限界がある
という好例でもある．**

:::danger
redex/paramができないことの根源的な原因は，define-languageの有無にある
のではないかという懸念．たとえば，必要と思ってないけど，我々のアプローチ
に define-language的なものを足すことは問題ないか？
:::


また，相互再帰についても，relation自身をparameterize，かつ，サンクによる遅延
という(よく知られてはいるが)コードへ展開されるマクロで可能にしている．

## parameterizedなrelationの実装について

interp/reduction-v1.0.rkt：
親と子の parameter が一致していないといけないバージョンの実装．
まず，これで基本的な実装手法を説明．

さらに，親子のparameterが不一致な場合に拡張．
* supre-relの指定を，ただの名前から，呼び出しへ変更．
* rename-transformer機能で親を含めたすべての先祖の節を一斉に置換
* 自然なこととして，子で新規追加した節はrenameの対象ではない

子の節はrenameしないことの例：interp/tmp6.rkt
```racket
(define (double x) (+ x x))
(define (square x) (*  x x))

(define-parameterized-reduction-relation ~~>
  [f]

  [(cons 'single x) (cons 'single (f x))     rule-single]
  [(cons 'double x) (cons 'double (double (f x))) rule-double])

(define base-eval ((reducer-of ~~>) add1))

(base-eval (cons 'single 100))
(base-eval (cons 'double 100))

(define-parameterized-extended-reduction-relation ==>
  [f g] (~~> square)

  ; このfはsquareにrenameされない．
  [(cons 'single x) (cons 'single (f x))         rule-single]
  [(cons 'triple x) (cons 'triple (g (g (g x)))) rule-triple])

(define ext-eval ((reducer-of ==>) add1 sub1))

(ext-eval (cons 'single 100))
(ext-eval (cons 'double 100))
(ext-eval (cons 'triple 100))
```

## 要検討：親のパラメータと同じようにreducerの引数もrename-transformerでは？

でinstantiateする実装をつくって，同じことができるか検討．
特に，相互再帰なreduction ruleでも大丈夫か？

1. 大丈夫なら，一番最後だけlambda抽象にしておくことのメリットを他に探す？「名前」じゃなく任意の「値」でparameter化したい例．
1. 同名の定義ではなく別名の定義に置き換えたい時．
  たとえば，`:=(1)` を `<-`に置き換える等．

## 要検討：parameterizationとextensionの使い分け

レキシカルスコープではないextensionを使えば，子定義で別定義を参照するために
parameterizeしておく必要はない．parameterizeは「同じreduction定義」を違う
関数（同名である必要もなし）の組み合わせで使えるようにしておくためのもの．

:::warning
あと，相互再帰もparameterを介してじゃないとできない？？
:::

逆に言うと，extensionした子定義で色々なものを「同名の」別の定義に置き換えたいなら，
単に子定義のスコープで見えている定義をコントロールすれば良い．
(言われてみればあたり前の機能だが，コードがかなりコンパクトにできる．)

実際，数学やセマンティクス定義などで「....」とextensionを省略する場面では
人間はおそらく暗黙で子定義のスコープに持ってきて考えなおしている．

:::info
上の一文（紙の上で書くとき）が，レキシカルスコープではないマクロに
基づいたデザインにしようという発想の基になっている，と主張．
:::

## 健全ではないマクロ展開の実装

健全なマクロシステムのRacketで実現するのは実はちょっと大変．
`reduction-v1.1.rkt` の `make-reducer-body` で一見良さそうだが，
実は，super-paramのrename-transformerへの束縛に失敗している．
これは，(car scs)に適切なスコープを強制的に付与しても，その後の
マクロ展開で，また別のスコープをつけられてしまう可能性があるから．
実際，`tmp8.rkt`で `:=` を `<-` に置き換えようとしたが，
`make-match-body`を適用することで，また元のスコープのついた`:=`に
戻されている．

## unit によるモジュール化との連携

名前のつけかえは提案したとおり．その名前の実体が何であるかをすげかえるには，
Racketの unit ライブラリと組み合わせる．

ただし，前節までの実装方法(reduction-v1.0.rkt)では，マクロ
定義(define-syntaxes)をユニットメンバーにできないため，修正が必要．

アイディアの肝：「reduction = signature」と捉えることで，メタ情報も
含めてreductionをパッケージできる．

制限？：unitと組み合わせるなら，reduction自体をimportしたくなる(下の例)．
だが，signatureはfirst class valueではないため，このデザインではムリ．

reductionという，コンパイル時に必要な情報を含んだものを実行時の値の
授受に用いるというのがそもそもムリ？  
→ ってことは，signatureの中に define-syntaxesでなら埋め込める？  
→ signature自体staticに指定するものだから，unitを invoke
する時点まで実体を遅延させることは不可能．

さしあたり，そんな風に書きたくなる場面がないので後回し．

:::danger
と，ここまで書いた方法だけでは，define-syntaxをexport/importできない
というそもそもの問題の解決に至っていない．なぜなら，signatureも同様に
first classではなく，export/importできないから．
:::

```racket
(define-signature -->/store ...)

(define-unit unit@
  (import context ...)
  (export)

  (define-signature ==>/store))
```
だと，`==>/store`が外部に見えない．
でも，`==>/store`の定義(`reducer`本体)からは`unit@`が`import`した`context`を見たい．

reduction（==>/store）を内部で定義する unit は，
以下のような特別なマクロで定義する．

```racket
(define-reduction-within-imports ==>/store
  (-->/store ...) (context ...) clause ...)

(define-signature ==>/store
  ((open context) ...
   (define-values ...)
   (define-syntaxes ...)
  ))
```

### 結論

他のunitのbindingsをreduction中で使用する名前の実体とするため，
`tmp/tmp3.rkt`のようにunit定義の内部で`define-reduction`をしたいが，
`unit`内部で展開コードの`define-signature`をしても無意味．

紆余曲折の末，unitと組み合わせられるバージョンが完成（reduction-v1.2.rkt）．
簡単な使い方は`tmp/tmp4.rkt`参照．

それに伴い，`tmp3.rkt`のようにunitの内部ではなく define-reductionの
superとして親定義を参照するためには，reduction構造体そのもの(-->)ではなく，
それに対応する signature (-->^)を require しなければいけない．
分かりにくいので，require/provideの derived form を用意し，
```racket
(provide (reduction-out --> ...))
(require (reduction-in "tmp2.rkt" --> ...)
```
と書けるようにする．


## version 1.3

前節のver. 1.2 ではなく，こちらをいきなり解説
以下は手動コンパイルの例．

```racket
#lang racket

(define-signature red^ (reducer))

(begin-for-syntax
 (struct reduction-desc
   (unit-id
    maybe-sig-id ;; #:doが書かれている場合だけdefineされている変数をexport
    params
    suepr-desc
    super-args
    within-signatures clause-map) #:transparent))

;;;;;;;; 1 ;;;;;;;;;;

;; (define-reduction (-->/+ <+>)
;;   [(cons a b) (<+> a b) 'add])
(define-syntax -->/+ (reduction-desc
                      #'-->/+@ #f
                      #'(<+>)
                      #f #'() #'() '[(cons a b) (<+> a b)]))
(define-unit -->/+@ (import) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ (<+>) (λ (s)
                               (match s
                                 [(cons a b) (<+> a b)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer1 (reducer-of -->/+))
(define reducer1 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (() -->/+@    ;; (syntax-local-value #'-->/+)
                             )))))
((reducer1 +) (cons 3 4))


;;;;;;;; 2 ;;;;;;;;;;

(define-signature X^ (X))

;; (define-reduction (--->/+ <+>) #:within-signatures [X^]
;;   [(cons a b) (<+> a b X) 'add])
(define-syntax --->/+ (reduction-desc
                       #'--->/+@ #f
                       #'(<+>)
                       #f #'() #'(X^) '[(cons a b) (<+> a b X)]))
(define-unit --->/+@ (import X^) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer )
       (λ (stx) #'(λ (<+>) (λ (s)
                               (match s
                                 [(cons a b) (<+> a b X)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

(define-unit X@ (import) (export X^)
  (define X 100))

;; (define reducer2 (reducer-of --->/+ #:within-units [X@]))
(define reducer2 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() --->/+@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer2 +) (cons 3 4))


;;;;;;;; 3 ;;;;;;;;;;

;; (define-reduction ==> #:super (-->/+ *))
(define-syntax ==> (reduction-desc
                    #'==>@ #f
                    #'()
                    #'-->/+ #'(*) #'() '[]))
(define-unit ==>@ (import) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ () (λ (s)
                            (match s  ; (syntax-local-value #'-->/+)
                              [(cons a b) (* a b)]) 
                            ))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer3 (reducer-of ==>))
(define reducer3 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (() ==>@    ;; (syntax-local-value #'==>)
                             )))))
((reducer3) (cons 3 4))


;;;;;;;; 4 ;;;;;;;;;;

;; (define-reduction ===> #:super (--->/+ *) #:within-signatures [X^])
(define-syntax ===> (reduction-desc
                     #'===>@ #f
                     #'()
                     #'--->/+ #'(*) #'(X^) '[]))
(define-unit ===>@ (import X^) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ () (λ (s)
                            (match s ; (syntax-local-value #'--->/+)
                              [(cons a b) (* a b X)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer4 (reducer-of ===> #:within-units [X@]))
(define reducer4 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() ===>@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer4) (cons 3 4))



;;;;;;;; 5 ;;;;;;;;;;
;;;; #:do [...]

;; (define-reduction (~~>/+ <+>) #:within-signatures [X^]
;;   #:do [(define Y 300)
;;         (define (dbgX msg) (println msg) X)]
;;   [(cons a b) (<+> a b (dbgX 'HOGEE) Y) 'add])
(define-syntax ~~>/+ (reduction-desc
                      #'~~>/+@ #'~~>/+^
                      #'(<+>)
                      #f #'() #'(X^) '[(cons a b) (<+> a b X)]))
(define-signature ~~>/+^ (Y dbgX))
(define-unit ~~>/+@ (import X^) (export red^ ~~>/+^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer )
       (λ (stx) #'(λ (<+>) (λ (s)
                               (match s
                                 [(cons a b) (<+> a b (dbgX 'HOGEE) Y)])))))))
  (define-unit M@ (import) (export M^))

  (define Y 300)
  (define (dbgX msg) (println msg) X)

  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer5 (reducer-of ~~>/+ #:within-units [X@]))
(define reducer5 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() ~~>/+@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer5 +) (cons 3 4))

;;;;;;;; 6 ;;;;;;;;;;
;;;; #:do [...] を含む reducton を，その定義を含めて継承

;; (define-reduction ~~~> #:super (~~>/+ *) #:within-signatures [X^])
(define-syntax ~~~> (reduction-desc
                     #'~~~>@ #f
                     #'()
                     #'~~>/+ #'(*) #'(X^) '[]))
(define-unit ~~~>@ (import X^ ~~>/+^) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ () (λ (s)
                            (match s ; (syntax-local-value #'~~>/+)
                              [(cons a b) (* a b (dbgX 'HOGEE) Y)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer6 (reducer-of ~~~> #:within-units [X@]))
(define reducer6 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (([s : ~~>/+^]) ~~>/+@ x)
                         (() ~~~>@    ;; (syntax-local-value #'~~~>)
                             s x)))))
((reducer6) (cons 3 4))
```

## define-language

ASTの定義として

1. 述語
2. match
3. extends可能
4. ==first classである==

が重要．1〜3 は標準のstructでなんとかなるが，4はclassじゃないと無理．
4はunit(モジュール機能)を駆使してAST定義を置き換えるのに必須．
(unitにおいてstructはsignatureメンバ，つまり，staticに固定．)
逆に，classは1〜2ができない(1はis/aのthin wrapperで十分)．

そこで，手書きのstructではなく，class定義を生成する
オリジナルのdefine-languageを準備．

## nondet monad との柔軟な組み合わせが可能

例として，expand の ex-app

```racket
  ;; application
  [(ζ (Stxξ (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘ κ0 Σ)
   #:when (id? stx_fun)
   #:with name :=<1> (resolve stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• 𝓁_new)
       Σ_1)
   ex-app]
```
ふつうなら
```racket
   #:with   at :=    (lookup-ξ ξ name)
```
を :=<1> にして set バージョンでは <- に切り替えるだけで良いが，
今回の場合「集合として空かどうか」で動作の切り替えが必要．
非決定的実行が組込まれている Redex ではこれは不可能．


## 評価

「健全性(レキシカルスコープ)を捨て去ることによるデメリットはないのか？」
Redex公式Tutorial, Redex book，AAM，DCPL等々，色々なreduction ruleを
持ってきて，提案ツールでも問題なく記述・動作できていることを検証．
