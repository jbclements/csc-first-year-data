#lang racket/base

(require db
         memoize
         racket/contract)

;; this file provides the function that makes a connection to the
;; grading database, and also provides a parameter that allows a program
;; to specify its own connection-maker

(provide
 (contract-out
  ;; return a new db connection using the current connection-maker
  [make-connection (-> connection?)]
  ;; the parameter that contains the function to be used when making a connection
  [connection-maker (parameter/c
                     (-> connection?))]
  ;; a replacement connection-maker for use directly on desmond
  [local-connect (-> connection?)]
  [database-name (parameter/c string?)]))

;; the user to use for making the connection
(define USER "scheduler")
;; terrible low-security password
(define PASSWORD "aoeuidht")

;; connect to a database using tcp
(define (tcp-connect)
  (postgresql-connect
   #:user USER
   #:password PASSWORD
   #:port 13432
   #:database (database-name)))

;; connect to the local database
(define (local-connect)
  (postgresql-connect #:user USER
                      #:password PASSWORD
                      #:port 5432
                      #:database (database-name)))

;; the parameter that controls how connections are made.
(define connection-maker
  (make-parameter tcp-connect))

(define database-name (make-parameter "cssegrades"))

;; make a connection by calling the current value of the connection-maker parameter
(define/memo (make-connection)
  ((connection-maker)))
