[導入]

一見なんでもないRacketコードを見せる．
そして
「これを解析したい，
でも実はマクロが含まれていて，究極的には local-expand や
bind-syntaxesを含んでいる．これを素直に抽象解釈しようと
するなら，scope-setモデルの抽象化が必要不可欠．」

[抽象解釈化に必要なこと]
1[済] ヒープの集合化
  - 2.のalloc-nameの有限化によって，namがドメインなξだけでなく，
    nam を含む Var がドメインである env も集合化の必要あり．

  set-baseにするだけ次のallocの有限化をしなくても stuck が生じることに注意．
  stuckの原因は，bind-syntaxesがstoreへのassignmentであることによりstore中の
  値の多重化が起きること．
  set-box!も同様であるが，そちらは結果が多重化するだけで済む．一方，
  bind-syntaxesはそのdefsを使用したlocal-expand中の変数解決がstuckしてしまう．
  --> 解決方法
  1. set/full/expander.rktの ==> の referenceの場合に unbound をエラーとせず，
  単に探索候補から取り除く．
  2. set/full/expander.rktの expand で stuck 状態をエラーとせず，単に結果から
  取り除く．

  TODO: 実装をシンプルに保つため，warningだけは出すことでお茶を濁す．
        理論的には，解析結果に「Top(よく分からない)」を追加すべき．

2[済] allocの有限化
  > どのドメインが有限？あるいは有限化が必要？
  > その有限性は syntax object が有限であるという仮定に依存しているか？

  - alloc-nameの有限化によって：
  ```racket
  (let ([z 1])
    ((let-syntax ([x (lambda (stx) #'z)])
       (lambda (z) (x))) 2))
  ```
  の z の env が混ざる((Var nam)のnamが同じになるため)．

3[済] resolveをscopeの有限化に合わせる．biggest-subsetとlookup-binding．
  biggestが存在しない場合ambiguous --> biggestの候補全部をnondetに実行．

  修正前：
  === conc/set/core/main.rkt ====
  sbss: (#<set: #(struct:object:StoBind% #<set: lam38778166> x:9)
                   #(struct:object:StoBind% #<set: lam38778165> x:2)>)
  scpsss: ((#<set: lam38778165> #<set: lam38778166>))
  scps_biggests: (#<set: lam38778166>)
  nam_biggests: (x:9)
  ref: #(struct:object:Stx% #(struct:object:Sym% x) #<set: lam38778166>)

  === abs/core.rkt ====
  sbss: (#<set: #(struct:object:StoBind% #<set: lam> x:0)
                   #(struct:object:StoBind% #<set: lam> x:2)>)
  scpsss: ((#<set: lam> #<set: lam>))
  scps_biggests: (#<set:>)
  nam_biggests: ()
  ref: #(struct:object:Stx% #(struct:object:Sym% x) #<set: lam>)

  さらに，parse時のresolveにより不整合なASTが生成される．それらの大半は
  evalがstuckするので，evaluateの書き換えによる結果からの除去で対処．


4. 値の有限化
  個々のドメインの抽象化を実装する前にまずは有限ではないせいで解析が停止
  しないmotivating exampleをつくる．その方が論文書きやすい．
  - 整数
  - 真偽値
  - 構文オブジェクト
    + とりあえず構文の形は気にせず有限サイズ
    + Stxコンストラクタへのフック
    + parser が AST への変換で「不法な」Stxを「top リテラル」へ写像
    + スコープセットはPROのとおり

5. evalとexpandの引数(3.で有限済み)による結果のメモ化
   - fullにおいて eval と expand が相互再帰．これで無限ループになるような
     例を書けるか？

6. (Var nam)のnamを(Stx Id ctx)にしてenvはIdのeq?によるハッシュにする．
  2.の env の有限化で2つのzが混ざるのは (Var nam) の nam が同じなため．
  それを lexicalに違う出現は別扱いするようにできる．
  さらに，3.の精度を向上させるため，scopeもsymbolそのものではなく，
  ソースコード中のStxオブジェクト単位で同一視する．
  
  ただし，これらの改良は 5. までの停止性を保証してから．この改善では
  ある程度のサイズのソースコードに対し有限上の衝突を「回避」する方法であり，
  いずれにしろ衝突への根本的対処が必要である．

7. exampleの拡充．
  - Macros that Work Together (model-together) の例を example に追加
