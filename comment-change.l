#|

[概要]
comment をトグル(comment-toggle) または コメント文字を削除(comment-out-off)する

[copyright]
(c) TKI

[インストール]
(load-library "comment-change.l")
;追加mode設定方法（必要に応じて)
(set-comment-toggle-init
 (list
  ;一行コメント、  複数コメント(前後)のように設定してください
  '("Lisp Interaction"  ";" "#|" "|#")
  ))

[License]
all free
(改造・再利用、商用OK,リンクや著作権記載不要）

素材そのものを、販売するのはNG
他者への攻撃や、私へのイメージの悪くなる社会的利用NG
素材そのものを他者へ転送すること・複製することは私のサイトが有る間はNG

[仕様詳細]
 (comment-toggle)
selection
  single
    複数コメント定義あり（セレクションの場所が範囲）
      カーソル位置にコメントあり：削除  ・・・使いづらい
      カーソル位置にコメントなし：セレクション自体に設定
  multi（セレクションの行の行頭から、 行末までが範囲）
    複数コメント定義なし（セレクションの場所が範囲）
      行頭位置にコメントあり：各行のコメント削除  --(A)
      行頭位置にコメントなし：各行にコメント
    複数コメント定義あり（セレクションの場所が範囲）
      行頭位置にコメントあり：コメント削除  ・・・使いづらい
      行頭位置にコメントなし：コメント設定
※セレクションの範囲を大きくそれない範囲に制限

non-selection
    単一コメント定義なし
      行頭位置にコメントあり：行頭/行末のコメント削除
      行頭位置にコメントなし：行頭/行末にコメント(カラム数は行頭の空白を除く)
    単一コメント定義あり（セレクションの場所が範囲）
      行頭位置にコメントあり：行頭のコメント削除 ・・・(B)
      行頭位置にコメントなし：行頭にコメント(カラム数は行頭の空白を除く)

※行中にコメントするケースは手動で対応するほうが速いので無視。

(comment-out-off)
selection
  single
    複数コメント定義あり（セレクションの場所が範囲）
      カーソル位置にコメントあり：削除  ・・・(C)
  multi（セレクションの行の行頭から、 行末までが範囲）
    複数コメント定義なし（セレクションの場所が範囲）
      行頭位置にコメントあり：各行のコメント削除 ・・・上記(A)と同様
    複数コメント定義あり（セレクションの場所が範囲）
      行頭位置にコメントあり：コメント削除  ・・・(C)と同様

non-selection
    単一コメント定義なし
      行頭位置にコメントあり：行頭/行末のコメント削除--- (C)と同様
    単一コメント定義あり（セレクションの場所が範囲）
      行頭位置にコメントあり：行頭のコメント削除   --- (B)と同様

|#

(setq *comment-toggle-ht* (make-hash-table :test #'equalp))
(defun set-comment-toggle-init (list)
  (dolist (al list)
	(setf (gethash (car al) *comment-toggle-ht*) (cdr al))))

(set-comment-toggle-init
 (list
  '("Lisp Interaction"  ";" "#|" "|#")
  '("Lisp"  ";" "#|" "|#")
  '("C"  "//" "" "")
  '("C++"  "//" "/*" "*/")
  '("C#"  "//" "/*" "*/")
  '("Java"  "//" "/*" "*/")
  '("HTML"  "" "<!--" "-->")
  '("HTML+"  "" "<!--" "-->")
  '("CSS"  "" "<!--" "-->")
  '("XML"  "" "<!--" "-->")
  '("javascript"  "//" "/*" "*/")
  '("Perl"  "#" "=begin" "=end")
  '("py"  "#" "" "")
  '("PHP"  "//" "/*" "*/")
  '("Ruby"  "#" "=begin" "=end")
  ))

(defun comment-toggle ()
  (interactive)
  (save-excursion
	(save-restriction
	  (let ((cl (gethash mode-name *comment-toggle-ht*)))
		(if (pre-selection-p)
			(comment-multi-toggle cl)
		  (comment-oneline-toggle cl))))))

(defun comment-out-off ()
  (interactive)
  (save-excursion
	(save-restriction
	  (let ((cl (gethash mode-name *comment-toggle-ht*)))
		(if (pre-selection-p)
			;selection
			(selection-start-end (start end)
			  (when (> start end)
				(let ((te start))(setq start end)(setq end te)))
			  (if (equal "" (cadr cl))
				  (let (bp)
					(setq bp t)
					(while bp
					  (if (scan-buffer (concat "^[ \t]*" (car cl) ) :regexp t :tail t)
						  (delete-backward-char (length (car cl))))
					  (setq bp (not (eq lastl (current-line-number))))
					  (next-line)
					  (goto-bol)))
				;(C)
				(comment-out-off-inner cl start end)))
		  ;non-selection
		  (if (equal "" (car cl))
			  (comment-out-off-inner cl (point)(point))
			(if (scan-buffer (concat "^[ \t]*" (car cl) ) :regexp t :tail t)
				(delete-backward-char (length  (car cl))))))))))

(defun comment-multi-toggle (cl)
  ;範囲選択：複数行
  (selection-start-end (start end)
	(when (> start end)
	  (let ((te start))
		(setq start end)
		(setq end te)))
	(narrow-to-region
	 (progn (goto-char start)(goto-bol) (point))
	 (progn (goto-char end)(goto-eol) (point)))

	(let ((lastl -1)fl cf (fcom (regexp-quote(cadr cl)))(ecom (regexp-quote (caddr cl))))
	  (goto-char start)(setq fl (current-line-number))
	  (goto-char end)
	  (if (and (eq fl (current-line-number))(not (equal "" (cadr cl))))
		  ;single select line
		  (progn
			(goto-char start)
			(if (setq cf(scan-buffer (concat "" fcom) :regexp t))
				(delete-char (length (cadr cl)))
			  (insert (cadr cl)))
			(goto-char end)
			(if cf
				(progn
				  (goto-char (- end (length (caddr cl))))
				  (scan-buffer (concat "" ecom) :regexp t :tail t)
				  (delete-backward-char (length (caddr cl))))
			  (progn (forward-char (length (cadr cl)))
				(insert (caddr cl)))))
		(progn
		  (goto-char (point-min))
		  (if (equal "" (cadr cl))
			  ;複数行コメント定義無し:各行の先頭でチェック
			  (let (bp)
				(setq cf (scan-buffer (concat "^[ \t]*" (car cl) ) :regexp t :tail t)
					  bp t)
				(while bp
				  (if cf
					  (delete-backward-char (length (car cl)))
					;(progn (skip-syntax-spec-forward " ")
					(insert (car cl))
					;)
					)
				  (setq bp (not (eq lastl (current-line-number))))
				  (next-line)
				  (goto-bol)
				  (scan-buffer (concat "^[ \t]*" (regexp-quote(car cl))) :regexp t :tail t)))
			;複数行コメント定義あり
			(progn
			  (if (setq cf (scan-buffer (concat "^[ \t]*" fcom) :regexp t :tail t))
				  (delete-backward-char (length (cadr cl)))
				(insert (cadr cl)))
			  (goto-char (point-max))

			  (if cf
				  (progn
					(goto-char (- end (length (caddr cl))))
					(scan-buffer (concat "" ecom) :regexp t )
					(delete-char (length (caddr cl))))
				(insert (caddr cl))))))))))

(defun comment-oneline-toggle(cl)
  ;選択なし:単一行
  (narrow-to-region
   (progn (goto-bol) (point))
   (progn (goto-eol) (point)))

  (goto-bol)
  (if (equal "" (car cl))
	  ;単一コメント定義なし
	  (if (scan-buffer (concat "^[ \t]*" (cadr cl) ) :regexp t :tail t)
		  (progn
			(delete-backward-char (length  (cadr cl)))
			(scan-buffer (caddr cl))
			(delete-char (length (caddr cl))))
		(progn
		  (insert (cadr cl))
		  (goto-eol)
		  (insert (caddr cl))))
	;単一コメント定義あり
	(if (scan-buffer (concat "^[ \t]*" (car cl) ) :regexp t :tail t)
		(delete-backward-char (length  (car cl)))
	  (progn (skip-syntax-spec-forward " ")(insert (car cl))))))

(defun comment-out-off-inner(cl start end)
  (let ((sbef -999888777) (sfore 999888777) (ebef -999888777)(efore 999888777)
		ns  ne)
	(goto-char start)
	(if (scan-buffer (regexp-quote (cadr cl)) :regexp t :reverse t)
	 (setq sbef (point)))
	(goto-char start)
	(if (scan-buffer (regexp-quote (cadr cl)) :regexp t :right-bound end)
		(setq sfore (point)))
	(goto-char end)
	(if (scan-buffer (regexp-quote (caddr cl)) :regexp t :reverse t :left-bound start)
		(setq ebef (point)))
	(goto-char end)
	(if (scan-buffer (regexp-quote (caddr cl)) :regexp t )
		(setq efore (point)))

	;近い方を採用
	(if (> (- start sbef) (- sfore start))
		(setq ns sfore)
	  (setq ns sbef))
	(if (> (- end ebef) (- efore end))
		(setq ne efore)
	  (setq ne ebef))

	(goto-char ne)
	(delete-char (length (caddr cl)))

	(goto-char ns)
	(delete-char (length (cadr cl)))))

;ツールバー
(defun comment-change-tool-bar ()
  (create-tool-bar
   'comment-change-tool-bar
   (merge-pathnames "toolbar.bmp" (etc-path))
   '(
	 ("プレビュー" 32 comment-toggle)
	 ("プレビュー" 34 comment-out-off)
    )))
(define-command-bar 'comment-change-tool-bar "コメント")

