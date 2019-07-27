(import
  (chicken format)
  (chicken io)
  (chicken process)
  (chicken process-context)
  (chicken string)
  (chicken time posix)
  (srfi 1)
  (srfi 13)
  (srfi 71)
  http-client
  openssl
  uri-common
  medea
  phricken
  sql-de-lite)


(define *version*
  (call-with-input-pipe
    "git describe --always --tags --dirty"
    read-line))

(client-software (cons (list "Star Gopher" *version* #f) (client-software)))

(define *api-key*
  (get-environment-variable "STAR_GOPHER_API_KEY"))
(assert *api-key* "Please define the STAR_GOPHER_API_KEY environment variable")

(define (current-timezone)
  (time->string (seconds->local-time) "%z"))

(include "database.scm")

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
                       (facet . nomarret)
                       (apikey . ,*api-key*)
                       (timezone . ,(current-timezone))
                       (refine.idligne . ,line-id)
                       (refine.sens . ,direction)
                       (refine.idarret . ,stop-id))))


;; Date and duration madness

;; Help, we need a good time & duration API

;; API Dates are represented as strings of this format: yyyy-mm-ddTHH:MM:SS+ZZ:ZZ"

(define (api-date->time str)
  (string->time str "%Y-%m-%dT%H:%M:%S%z"))

(define (time- d1 d2)
  (- (local-time->seconds d1)
     (local-time->seconds d2)))

;; Duration in seconds -> duration in hours, minutes, seconds
(define (seconds->duration secs)
  (let* ((big-minutes seconds (quotient&remainder secs 60))
         (hours minutes (quotient&remainder big-minutes 60)))
    (list hours minutes)))

(define (duration->string dur)
  (if (zero? (car dur))
      (if (zero? (cadr dur))
          "<1 min"
          (sprintf "~A min" (cadr dur)))
      (sprintf "~A hr ~A min" (car dur) (cadr dur))))


;; Actual work

(define (realtime-traffic-for line-id direction stop-id)
  (let* ((uri (realtime-traffic-uri line-id direction stop-id))
         (json (with-input-from-request uri #f read-json))
         (records (vector->list (alist-ref 'records json)))
         (first-fields (alist-ref 'fields (car records))))
    (cons*
      (sprintf "Ligne ~A direction ~A"
               (alist-ref 'nomcourtligne first-fields)
               (alist-ref 'destination first-fields))
      (sprintf "Arrêt: ~A"
               (alist-ref 'nomarret first-fields))
      (sprintf "Données datant du ~A"
               (time->string (api-date->time
                               (alist-ref 'record_timestamp (car records)))
                             "%d/%m/%Y %H:%M:%S"))
      ""
      (format-realtime-traffic-data records))))

(define (format-realtime-traffic-data records)
  (define (fmt-record record-fields record-timestamp)
    (let* ((departure-time (api-date->time (alist-ref 'depart record-fields)))
           (record-time (api-date->time record-timestamp))
           (diff (time- departure-time record-time))
           (duration (seconds->duration diff)))
    (sprintf "~a ~a (~a)"
             (if (string=? (alist-ref 'precision record-fields)
                           "Temps réel")
                 #\space
                 #\*)
             (duration->string duration)
             (time->string (api-date->time (alist-ref 'departtheorique record-fields))
                           "%H:%M:%S"))))
  (map (lambda (r) (fmt-record (alist-ref 'fields r)
                               (alist-ref 'record_timestamp r)))
    records))


;; Gopher frontend

(define (root-handler req)
  (send-entries
    `((0 "À propos de ce service" "/about")
      (1 "Liste des lignes" "/lines")
      (7 "Recherche par arrêt" "/search/stop")
      (0 "C5 Lycée Brequigny @ Rochester" "/1259/5/0")
      (0 "C5 Lycée Brequigny @ Sainte Anne" "/1014/5/0")
      (0 "C5 Patton @ Sainte Anne" "/1026/5/1")
      (0 "C5 Patton @ Pressoir" "/2257/5/1")
      )))

(define (about-handler req)
  (for-each
    send-line
    `("Star Gopher"
      "-----------"
      ""
      "Service d’horaires en temps réel de la ville de Rennes, servi sur"
      "le réseau Gopher."
      ""
      "Écrit par Kooda <kooda@upyum.com>"
      ""
      ,(conc "Version: " *version*)
      "Sources: git://upyum.com/star-gopher"))
  (send-lastline))

(define (realtime-traffic-handler req)
  (define (sanitize-input stop-id line-id direction)
    (list (string-pad line-id 4 #\0) direction stop-id))
  (let ((args (apply sanitize-input (request-matches req))))
    (for-each send-line (apply realtime-traffic-for args))
    (send-lastline)
    #t))

(define (lines-handler req)
  (define (line-link id name description)
    (make-entry 1
                (sprintf "~A: ~A" name description)
                (sprintf "/line/~A" id)))
  (for-each (lambda (l) (send-entry (apply line-link l))) (list-lines))
  (send-lastline)
  #t)

(define (line-handler req)
  (send-line "Not implemented yet")
  (send-lastline)
  #t)

(define (search-stop-handler req)
  (define (line-link stop-name line-name destination-name stop-id line-id direction)
    (make-entry 0
                (sprintf "Arrêt ~A: ligne ~A direction ~A"
                         stop-name line-name destination-name)
                (sprintf "/~A/~A/~A"
                         stop-id line-id direction)))
  (let* ((search-string (car (request-extra req)))
         (search-results (search-stops search-string)))
    (for-each
      (lambda (result)
        (send-entry (apply line-link result)))
      search-results)
    (send-lastline)))

(handlers (list (match-selector "" root-handler)
                (match-selector "/" root-handler)
                (match-selector "/about" about-handler)
                (match-selector "/lines" lines-handler)
                (match-selector '(: "/line/" ($ (+ num)))
                  line-handler)
                (match-selector "/search/stop" search-stop-handler)
                (match-selector
                  '(: "/" ($ (+ num)) "/" ($ (+ num)) "/" ($ (+ num)))
                  realtime-traffic-handler)))

(port 7071)
(sgm-port 70)
(cond-expand
      ((or chicken-script compiling) (start-server!))
      (else))
