(import (chicken format) (chicken process-context) (chicken string) (chicken time posix) (srfi 13) http-client openssl uri-common medea phricken)

(client-software (cons '("Star Gopher" "0.1" #f) (client-software)))

(define *api-key*
  (get-environment-variable "STAR_GOPHER_API_KEY"))
(assert *api-key* "Please define the STAR_GOPHER_API_KEY environment variable")

(define (current-timezone)
  (time->string (seconds->local-time) "%z"))

;; URLs

(define *api-base-uri*
  (uri-reference "https://data.explore.star.fr/api/records/1.0/search/"))

(define (realtime-traffic-uri line-id direction stop-id)
  (assert (= (string-length line-id) 4))
  (assert (member direction '("0" "1")))
  (update-uri *api-base-uri*
              query: `((dataset . tco-bus-circulation-passages-tr)
                       (sort . -depart)
                       (facet . idligne)
                       (facet . nomcourtligne)
                       (facet . sens)
                       (facet . destination)
                       (facet . precision)
                       (apikey . ,*api-key*)
                       (timezone . ,(current-timezone))
                       (refine.idligne . ,line-id)
                       (refine.sens . ,direction)
                       (refine.idarret . ,stop-id))))


;; Actual work

(define (fetch uri)
  (with-input-from-request uri #f read-json))

(define (format-realtime-traffic-data records)
  ;; TODO relative time
  (define (cleanup-date str)
    (cadr (string-split str "T+")))
  (define (fmt-record rec)
    (sprintf "~a -> ~a: ~a (~a)"
             (alist-ref 'nomcourtligne rec)
             (alist-ref 'destination rec)
             (if (string=? (alist-ref 'precision rec)
                           "Temps réel")
                 (cleanup-date (alist-ref 'depart rec))
                 #\*)
             (cleanup-date (alist-ref 'departtheorique rec))))
  (map (lambda (r) (fmt-record (alist-ref 'fields r))) records))


;; Gopher frontend

(define (root-handler req)
  (send-entries
    `((i "Star Gopher")
      (i "v0.1")
      (i)
      (0 "C5 Lycée Brequigny @ Rochester" "/1259/5/0")
      (0 "C5 Lycée Brequigny @ Sainte Anne" "/1014/5/0")
      (0 "C5 Patton @ Sainte Anne" "/1026/5/1")
      (0 "C5 Patton @ Pressoir" "/2257/5/1")
      )))

(define (realtime-traffic-handler req)
  (define (sanitize-input stop-id line-id direction)
    (list (string-pad line-id 4 #\0) direction stop-id))
  (let* ((args (apply sanitize-input (request-matches req)))
         (uri (apply realtime-traffic-uri args))
         (json (fetch uri))
         (records (vector->list (alist-ref 'records json))))
    (for-each
      send-line
      (format-realtime-traffic-data records))
    #t))

(handlers (list (match-selector "" root-handler)
                (match-selector
                  '(: "/" ($ (+ num)) "/" ($ (+ num)) "/" ($ (+ num)))
                  realtime-traffic-handler)))

(port 7071)
(sgm-port 70)
(cond-expand
      ((or chicken-script compiling) (start-server!))
      (else))
