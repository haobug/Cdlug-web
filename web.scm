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



;; 定义一个简单的web服务 Hello World！
(define (hello-world-handler request request-body)
  (values '((content-type . (text/plain)))
	  "Hello World!"))

;; 运行guile内置的web服务器。
(run-server hello-world-handler)

;; 检查request

(use-modules (web server))
(use-modules (web request)
	     (web response)
	     (web uri))

;; 获取request值
(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

;; 测试 split-and-decode-uri-path
(define (list-tostring alist)
  (let ((str ""))  
    (map
     (lambda (lst)
       (string-append str lst)
       (format #t "~a~%" str))
    alist)))

(define (test request body)
  (values (build-response #:code 404)
	  (string-append "Resource: "
			 (request-path-components request))))

;; 定义一个错误信息页面
(define (not-found request) 
  (values (build-response #:code 404) 
	  (string-append "Resource not found: " 
			 (uri-path (request-uri request)))))

;;定义一个简单的web服务 request请求
(define (hello-hacker-handler request body)
  (if (equal? (request-path-components request)
	      '("hacker"))
      (values '((content-type . (text/plain)))
	      "Hello Hacker!")
      (not-found request)))

