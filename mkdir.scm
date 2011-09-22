;; Copyleft(c)2010 HackerFellowship. All lefts reserved.
  
;; B.tag <bb.qnyd@gmail.com>
  
;; May Lord Bless!Happy Hacking!
  
;; This program is free software;you can redistribute it and /or modify
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundataion;either version 2 of the License,or (at 
;; your option) any later version.
  
;; You should have read the GNU General Public License before start "RTFSC".
  
;; If not,see <http://www.gnu.org/licenses/>


;;(define _DATA_PATH "./")
;; create db directory.
(define mkdirs
  (lambda (file-path)
      (if (file-exists? file-path)
	  (error mkdirs "File exists." file-path))
      (if 
       (and (mkdir file-path) (chmod file-path 0755))
       #t)))
;; write data in table file.
(define write-data
  (lambda (file-names string-data)
    (let ((fp (open-file file-names "w+")))
      (write string-data fp)
      (close fp))))

;;modify table file or db directory names.
(define alter-file-or-dir-name
  (lambda (old-names new-names)
    (if (not (file-exists? new-names))
	(rename-file old-names new-names))))

;; procedure is-db-or-is-table
;; parameter files mode
;; files -> db directory or -> table file
;; mode category db or table
(define is-db-or-is-table
  (lambda (files mode)
    (cond ((= mode 1)
	   (if (file-exists? files)
	       #t))
	  ((= mode 0)
	   (if (file-is-directory? files)
	       #t)))))

(define execute
  (lambda (ation lists)
    (if (list? lists)
	(cond ((eqv? ation "create database")
	       (create-db (assoc-ref lists "db")))
	      ))))

(define bin-search
  (lambda (lists param)
    (let ((m (+ (div (length lists) 2) 1)))
      (cond ((string>? (list-ref lists m) param)
	     (bin-search (list-head lists m) param))
	    ((string<? (list-ref lists m) param)
	     (bin-search (list-tail lists m) param))
	    ((string=? (list-ref lists m) param)
	     (list-ref lists m))
	    (else
	     #f)))))

(define (fun ss) 
  (map (lambda (s) 
	 (if (list? s) 
	     (fun s)) 
	     (string-delete #\/ s))
       ss))


