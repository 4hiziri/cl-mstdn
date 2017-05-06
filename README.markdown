# Cl-Mstdn

## Usage
```lisp
(defparameter client-token (request-client-token "インスタンス名")) ;; クライアントごとに一回でオーケー
(defparameter access-token (register-client "インスタンス名" client-token "たぶん登録したメアド" "パスワード")) ;; このトークンは以後インスタンスサーバへのアクセスで使う、保存しておく(保存するメソッドはない)
```

## Installation

## Author

* takagi seiya (meirvg@gmail.com)

## Copyright

Copyright (c) 2017 takagi seiya (meirvg@gmail.com)
