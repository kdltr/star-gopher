(import
  (chicken format)
  (chicken io)
  (chicken process)
  (chicken process-context)
  (chicken string)
  (chicken time posix)
  (srfi 1)
  (srfi 13)
  (srfi 18)
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

;; Date and duration madness

;; Help, we need a good time & duration API

;; API Dates are represented as strings of this format: yyyy-mm-ddTHH:MM:SS+ZZ:ZZ"

(define (api-date->time str)
  (string->time (string-translate str ":")
                "%Y-%m-%dT%H%M%S"))

(define (time- d1 d2)
  (- (utc-time->seconds d1)
     (utc-time->seconds d2)))

;; Duration in seconds -> duration in hours, minutes, seconds
(define (seconds->duration secs)
  (let* ((big-minutes seconds (quotient&remainder secs 60))
         (hours minutes (quotient&remainder big-minutes 60)))
    (list hours minutes)))

(define (duration->string dur)
  (if (<= (car dur) 0)
      (if (<= (cadr dur) 0)
          "<1 min"
          (sprintf "~A min" (cadr dur)))
      (sprintf "~A hr ~A min" (car dur) (cadr dur))))




;; Gopher frontend

(define (root-handler req)
  (send-entries
    `((0 "À propos de ce service" "/about")
      (1 "Liste des lignes" "/lines")
      (7 "Recherche par arrêt" "/search/stop")
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
  (let* ((triplet (request-matches req))
         (now-time (seconds->local-time))
         (line-name line-direction stop-name
           (apply values (apply triplet-informations triplet))))
    (for-each
      send-line
      (list (sprintf "Ligne ~A direction ~A"
                     line-name line-direction)
            (sprintf "Arrêt: ~A"
                     stop-name)
            (sprintf "Date sur le serveur: ~A"
                     (time->string now-time "%d/%m/%Y %H:%M:%S"))
            "* : horaire théorique"
            ""))
    (for-each send-line (traffic-line now-time triplet))
    (send-lastline)
    #t))

(define (traffic-line now-time triplet)
  (map
    (lambda (l)
      (let* ((date precision (apply values l))
             (diff (time- (api-date->time date) now-time))
             (duration (seconds->duration diff)))
        (sprintf "~a ~a (~a)"
                 (if (string=? precision "Temps réel") #\space #\*)
                 (duration->string duration)
                 (time->string (api-date->time date) "%H:%M:%S"))))
    (apply realtime-traffic triplet)))

(define (lines-handler req)
  (define (line-link id name description)
    (make-entry 1
                (sprintf "~A: ~A" name description)
                (sprintf "/routes/~A" id)))
  (for-each (lambda (l) (send-entry (apply line-link l))) (list-lines))
  (send-lastline)
  #t)

(define (routes-handler req)
  (define (line-link short-name description route-id)
    (make-entry 1
                (sprintf "Ligne ~A : ~A" short-name description)
                (sprintf "/stops/~A" route-id)))
  (let ((line-id (car (request-matches req))))
    (for-each
      (lambda (l)
        (send-entry (apply line-link l)))
      (list-routes-for-line line-id)))
  (send-lastline)
  #t)

(define (stops-handler req)
  (define (line-link name stop-id line-id direction)
    (make-entry 1
                name
                (sprintf "/~A/~A/~A" stop-id line-id direction)))
  (let* ((route-id (car (request-matches req)))
         (route-infos (route-informations route-id))
         (line-and-dir (cddr route-infos)))
    (send-entry
      (make-info-entry "Ligne " (first route-infos) " : " (second route-infos)))
    (for-each
      (lambda (l)
        (send-entry (apply line-link (append l line-and-dir))))
      (list-stops-for-route route-id)))
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
                (match-selector '(: "/routes/" ($ (+ num)))
                  routes-handler)
                (match-selector '(: "/stops/" ($ (+ any)))
                  stops-handler)
                (match-selector "/search/stop" search-stop-handler)
                (match-selector
                  '(: "/" ($ (+ num)) "/" ($ (+ num)) "/" ($ (+ num)))
                  realtime-traffic-handler)))

(port 7071)
(sgm-port 70)


(define (start-star-gopher!)
  (when (null? (list-lines))
    (update-all-datasets!))
  (thread-start! (make-thread dataset-updater))
  ((logger) 'info #f "Starting server")
  (start-server!))

(define epoch (time->seconds (current-time)))

(define (dataset-updater #!optional (n 1))
  (thread-sleep! (+ epoch (* n 6 60 60)))
  (update-all-datasets!)
  (dataset-updater (add1 n)))

(define (update-all-datasets!)
  ((logger) 'info #f "Updating datasets")
  (update-lines-table!)
  (update-stops-table!)
  (update-routes-table!)
  ((logger) 'info #f "Finished updating datasets"))

(cond-expand
      ((or chicken-script compiling) (start-star-gopher!))
      (else))
