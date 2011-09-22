;; Copyleft(c)2010 HackerFellowship. All lefts reserved.
  
;; B.tag <bb.qnyd@gmail.com>
;; NalaGinrut <NalaGinrut@gmail.com>
  
;; May Lord Bless!Happy Hacking!
  
;; This program is free software;you can redistribute it and /or modify
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundataion;either version 2 of the License,or (at 
;; your option) any later version.
  
;; You should have read the GNU General Public License before start "RTFSC".
  
;; If not,see <http://www.gnu.org/licenses/>
 
(define check-key
  (lambda (lists)
    #t))

;;要确保全局变量的唯一性 
;;需要确保表的数据能被特殊函数读取和写入
;;此处是建立一个type类型和p-key-table的字段存放表。
(define type-table '())
(define p-key-table '())

(define make-type-table
  (lambda (type-table-list p-key-list)
    (cond ((eqv? type-table-list '())
	   (error make-type-table "Invalid type-table." type-table-list))
	  ((eqv? p-key-list '())
	   (error make-type-table "Invalid p-key-list." p-key-list))
	  (else
	   (if (check-key type-table-list)
	       (set! type-table type-table-list))
	   (if (check-key p-key-list)
	       (set! p-key-table p-key-list))))
    type-table-list))
	 

;;将新的字段列表插入type-table中
(define insert-type-table-key
  (lambda (table-key-list)
    (cons table-key-list type-table)))

(define db_file_exist
  (lambda (file-name)
    #t)

(define make-table
  (lambda (table-name)
    #t))


(define make-type-check
  (lambda (type)
    (let ((t string-downcase))
      (cond 
       ((string=? t "int") integer?)
       ((string=? t "uint") positive?)
       ((string=? t "string") string?)
       (else
	(error make-type-check "Invalid type!" type))))))
       
(define make-type-record
  (lambda (name type size)
    (list name (make-type-check type) size))) 


(define insert-type-to-table 
  (lambda (type-table keyname keytype keysize)
    (cons 
     (make-type-record keyname keytype keysize)
     type-table)))

(define create-table
  (lambda (name p-key)
    (cond 
     ((not (string? name))
      (error create-table "Invalid name!" name))
     ((not (string? p-key)) 
      (error create-table "Invalid p-key!" p-key))
     (else      
      (list (list name length p-key)
	    (list '())
	    (list '()))))))

(define table-type-handle '(("id" integer? 11) ("name" string? 20) ("age" integer? 2)))



(define db '((("id" 1) ("name" "bao") ("mail" "bao@gmail.com") ("age" "18"))
	     (("id" 2) ("name" "pw") ("mail" "pw@gmail.com") ("age" "18"))
	     (("id" 3) ("name" "gang") ("mail" "gang@gmail.com") ("age" "19"))))

(define search
  (lambda (db f p v)
    (let ((results '()))
      (for-each
       (lambda (x)
	 (let ((item (assoc-ref x f)))
	   (if (p (car item) v) (set! results (cons x results))
	       )))
       db)
      results)))

(define search_values
  (lambda (o f)
    (let ((results '()))
      (for-each
       (lambda (x)
	 (let ((var (assoc-ref x f)))
	   (set! results (cons var results)))
	 )
       o)
      results)))

;(map 
; (lambda (x)
;   (format #t "name:~a~%" (car x))) 
; (search_values (search db "id" <= 2) "name"))

;(for-each 
; (lambda (x) 
;   (set! array (cons (car x) array))) 
; (search_values (search db "id" <= 2) "name"))


;(define FIELD 1)
;(define TABLE 2)
;(define ITEM 3)
;(define PRE 4)
;(define VALUE 5)

;(define lang (string-split 
;	      (regexp-substitute #f 
;				 (string-match sql "select name from HFG where age = 18")
;				 'pre 1 "#" 2 "#" 3 "#" 4 "#" 5 'post) #\&))
;(define sql "select ([a-zA-Z0-9]*) from ([a-zA-Z0-9]*) where ([a-zA-Z0-9]*) ([=<>]*) ([a-z0-9A-Z]*)")


(define-syntax mselect 
  (syntax-rules (from where in) 
    ((mselect FIELD from TABLE where ID in (VALUE))
     (search_values (search TABLE ID string=? VALUE) FIELD))
    )
  )

;; check type yes or no exist.
;; if exist,return this type.
;; not exist,return error,inform error where.
(define checking
  (lambda (name)
    (let ((t string-downcase))
      (cond ((string=? (t name) "int") name)
	    ((string=? (t name) "string") name)
	    (else
	     (error checking "Invalid Type!" name))))))


(define make-type-table
  (lambda (field-name field-type field-length)
    (list field-name field-type field-length)))

(define table
  (lambda (table-field field-to-data)
    (list table-field field-to-data)))

(define status-table
  (lambda (table-name p-key power)
    (list table-name p-key power)))

(define type-list '(("name" "string" 20) ("age" "int" 10)))

(define make-all-table
  (lambda (table-name type-list p-key)
    (list
     (let loop ((ll type-list)
		(type-table '()))
       (cond ((null? ll)
	  type-table)
	     ((not (null? (car ll)))
	      (loop (cdr ll)
		    (cons (make-type-table (caar ll) (cadar ll) (caddar ll)) type-table)
		    ))))
     (make-status-table table-name p-key 0)
     
     (let loop ((ll type-list)
		(main-table '()))
       (cond ((null? ll)
	      main-table)
	     ((not (null? (car ll)))
	      (loop (cdr ll)
		    (cons (table (caar ll) "null") main-table)
		    ))))
     )))

