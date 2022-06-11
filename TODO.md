TODO

  (1) define, define-syntax
      とりあえず，上から順のtoplevel primitiveである
        define-values, define-syntaxes
      を実装．define, define-syntaxをマクロ定義してテストをしやすくする．
  (1.5) その後，内部定義(let () (define ...))や (let () (define-syntax ...)
      を local-expand, definition-contextをつかった正式な実装にして
      (ということはlet-values+syntaxesをprimitiveに追加)，それらの
      テスト対象マクロとする．

  (2) 構文オブジェクトの抽象化(abstraction function)
    * コード部分
      - あまりおおざっぱだとevalやexpandが回らなくなりそう
      - 方針1: シンボルとリストは正確なまま．それ以外は近似．
          コードの大きさ(ペア数)がある程度を超えたら解析時エラーで落とす．
      - 方針2: コアフォームを「ある程度」認識できる程度にフォームの先頭部分の
          いくらかを正確に覚えておく．それだけの知識で「ある程度」ざっくりと
          動作できるagnosticなevalとexpandも抱き合わせで提案．
    * スコープ部分
      - use/def相当の情報をidにつけるにはどんなデータ構造が良い？
        それとも，単にスコープセットのセットにしてしまってぼやけてしまうことを
        問題提起しておわりにするか．．．

  (3) 抽象インナプリタ化(AAMでやる)
      まずはRedexに頼らずにworklist方式でnondeterministicな実行を可能にしておく．
      できればyieldを呼び出したときだけスイッチする方が効率良い？

  (4) toy benchmark
  loopするメタレベル関数コード&loopする展開先コード&ループで無限にフェーズが大きくなる
  letrec, letrec-syntaxes? --> とりあえず μ を入れておく



[論文に載せること]
 * local-expand以降は，evalとexpandの相互再帰．
    それを有限回に抑える方法は？ということを
      local-expandを考慮すべきことや抽象化の工夫点として論文に書く．
      core, phasesでは expand から eval を呼び出すだけなので個々を
      有限にすれば十分ということも書く．
      --> coreでは無理だけどphasesでは相互再帰できそうな気がしてきた．
          let-syntaxのrhsをexpandする中でまたlet-syntaxすればいい？
      - 方針1: あらゆるeval, expand呼び出しで同じ探索空間を共有
      - 方針2: ADIの方法を流用．(call x 探索空間)でキャッシュ．
        呼出しも有限なのでキャッシュサイズも有限．方針1よりは精度が良さそう．
 * define, define-syntax
      とりあえず，上から順のtoplevel primitiveである
        define-values, define-syntaxes
      を実装．define, define-syntaxをマクロ定義してテストをしやすくする．
 *その後，内部定義(let () (define ...))や (let () (define-syntax ...)
      を local-expand, definition-contextをつかった正式な実装にして
      (ということはlet-values+syntaxesをprimitiveに追加)，それらの
      テスト対象マクロとする．


