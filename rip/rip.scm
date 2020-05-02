(define-module (rip headstone)
  #:export (format-headstone headstone-width)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define headstone
  '("                       ----------"
    "                      /          \\"
    "                     /    REST    \\"
    "                    /      IN      \\"
    "                   /     PEACE      \\"
    "                  /                  \\"
    "                  |                  |" ;; Name of player
    "                  |                  |" ;; Amount of $
    "                  |                  |" ;; Type of death
    "                  |                  |" ;; .
    "                  |                  |" ;; .
    "                  |                  |" ;; .
    "                  |       1001       |" ;; Real year of death
    "                 *|     *  *  *      | *"
    "        _________)/\\\\_//(\\/(/\\)/\\//\\/|_)_______"))


(define headstone-header
  '("                       ----------\n"
    "                      /          \\\n"
    "                     /    REST    \\\n"
    "                    /      IN      \\\n"
    "                   /     PEACE      \\\n"
    "                  /                  \\\n"))


(define headstone-middle-prefix "                  | ")
(define headstone-middle-suffix " |\n")

(define headstone-footer
  '("                 *|     *  *  *      | *\n"
    "        _________)/\\\\_//(\\/(/\\)/\\//\\/|_)_______\n"))

(define headstone-offset 16)
(define headstone-width 16)

(define stone-line-cent 8)

(define (center text)
  (let* ((text-length (string-length text))
         (text-center (floor (/ (+ text-length 1) 2)))
         (text-start (- stone-line-cent text-center))
         (left-padding (make-string text-start #\space))
         (right-padding (make-string
                         (- headstone-width
                            (+ text-start text-length)) #\space)))
    (string-append left-padding text right-padding)))

(define (format-name name)
  (string-append
   headstone-middle-prefix
   (center name)
   headstone-middle-suffix))

(define (format-gold gold)
  (let ((gold-line (format #f "~d Au" gold)))
    (string-append
     headstone-middle-prefix
     (center gold-line)
     headstone-middle-suffix)))

(define (split-word word)
  (let ((word-length (string-length word)))
    (if (> word-length headstone-width)
        (cons (substring word 0 headstone-width)
              (split-word (substring word headstone-width word-length)))
        (list word))))

(define (split-text text)
  (let ((words (string-split text #\space)))
    (fold-right
     (lambda (xs prev)
       (fold-right cons prev xs))
     '()
     (map split-word words))))

(define (handle-word word prev)
  (let ((lst (car prev))
        (count (cdr prev))
        (word-len (string-length word)))
    (if (null? lst)
        (cons (list (list word)) word-len)
        (let ((line (car lst))
              (rest (cdr lst)))           
          (if (>= (+ word-len count) headstone-width)
              (cons (cons (list word) (cons (reverse line) rest))
                    word-len)
              (cons (cons (cons word line) rest)
                    (+ word-len count 1)))))))

(define (make-death-lines text)
  (let* ((words (split-text text))
         (lines (car (fold handle-word '(() . 0) words)))
         (last-line (reverse (car lines)))
         (fixed-lines (cons last-line (cdr lines))))      
    (map (lambda (x)
           (string-join x " "))
         (reverse fixed-lines))))

(define (ensure-three-elems lst)
  (match lst
    ((x) (list x "" ""))
    ((x y) (list x y ""))
    (_ lst)))

(define (format-death text)
  (let* ((lines
          (ensure-three-elems
           (make-death-lines text))))
    (map (lambda (x)
           (string-append
            headstone-middle-prefix
            (center x)
            headstone-middle-suffix))
         lines)))

(define (format-year year)
  (let ((year-line (format #f "~d" year)))
    (string-append
     headstone-middle-prefix
     (center year-line)
     headstone-middle-suffix)))

(define (format-headstone name gold death year)
  (let ((name-line (format-name name))
        (gold-line (format-gold gold))
        (death-lines (format-death death))
        (year-line (format-year year)))
    (apply string-append
           `(,@headstone-header
             ,name-line
             ,gold-line
             ,@death-lines
             ,year-line
             ,@headstone-footer))))


