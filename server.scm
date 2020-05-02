(use-modules (ice-9 match)
             (rip headstone)             
             (web request)
             (web response)
             (web server)
             (web uri))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (bad-request code)
  (values (build-response #:code code)
          "bad request"))

(define (query-entry-to-pair entry)
  (match entry
    (() (cons #f #f))
    ((a) (cons a #f))
    ((a b) (cons a b))
    ((a b . rst) (cons a (string-join (cons b rst) "=")))))

(define (query-to-alist query)
  (let* ((pairs (string-split query #\&))
         (alist (map (lambda (x) (string-split x #\=)) pairs)))
    (map query-entry-to-pair alist)))

(define (handle-headstone request)
  (let* ((query-str (or (uri-query (request-uri request)) ""))
         (query (query-to-alist query-str))
         (name (assoc-ref query "name"))
         (gold (random 1000000))
         (death (assoc-ref query "death"))
         (now (localtime (current-time)))
         (year (+ (tm:year now) 1900)))
    (cond
     ((not (and name death))
      (bad-request 400))
     ((> (string-length name) headstone-width)
      (bad-request 400))
     (else
      (values
       '((content-type . (text/plain)))
       (format-headstone name gold death year))))))

(define (handler request body)
  (let* ((method (request-method request))
         (path (request-path-components request))
         (now (localtime (current-time)))
         (now-str (strftime "%FT%T" now)))
    (format #t "[~a] ~a ~a\n" now-str method path)
    (match (cons method (list path))
      ((GET ()) (handle-headstone request))
      (_ (not-found request)))))

(set! *random-state* (random-state-from-platform))

(run-server handler 'http '(#:addr 0 #:port 8080))

