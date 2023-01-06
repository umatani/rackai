# Racket-based Framework for Abstract Interpretation

## 準備

1. 通常の方法で[Racket](https://racket-lang.org)をインストール
1. 依存パッケージ`redex-parameter`をインストール
    ```shell-session
    $ raco pkg install redex-parameter
    ```

## 使い方

* `test/main.rkt`を読み込み`(main)`を実行することにより，全テストプログラムを
  全解釈器を使って実行
* `test/eval2.rkt`：6節の性能測定
    - lwで実行
        ```racket
        eval2.rkt > (run-examples lw-eval-no-cache scopeset-examples #:repeat 1)
        ```
    - rxで実行
        ```racket
        eval2.rkt > (run-examples rx-eval scopeset-examples #:repeat 1)
        ```
    - rx-no-cacheで実行
        ```racket
        eval2.rkt > (run-examples rx-eval-no-cache scopeset-examples #:repeat 1)
        ```

## ディレクトリ構成

```shell
.                           # 共通(model以外)
├── model                   ## 実行モデル
│   ├── scopeset            ### スコープ集合モデル(先行研究)
│   ├── scopeset-smallstep  ### スコープ集合モデル(small-step)
│   └── together            ### 履歴モデル(先行研究)
├── conc                    ## 具象解釈(共通)
│   ├── core                ### core言語
│   ├── phases              ### phases言語
│   └── full                ### full言語
├── sig                     ## シグネチャ(モジュールインタフェース)群
├── mult                    ## ヒープメモリ多重化(共通)
│   ├── core                ### core言語
│   ├── phases              ### phases言語
│   └── full                ### full言語
├── abs                     ## 抽象解釈(共通)
│   ├── core.rkt            ### core言語
│   ├── phases.rkt          ### full言語
│   ├── full.rkt            ### full言語
│   └── naive               ### naive抽象解釈
└── test                    ## テストコード
```
