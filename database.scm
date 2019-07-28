(define (read-csv input-port)
  (read-list input-port
             (lambda (p)
               (let ((line (read-line p)))
                 (if (eof-object? line)
                     line
                     (string-split line ";" #t))))))

(define *download-base-uri*
  (uri-reference "https://data.explore.star.fr/explore/dataset/"))

(define (download-dataset-uri dataset)
  (update-uri *download-base-uri*
              path: (append (uri-path *download-base-uri*)
                            (list dataset "download"))
              query: `((format . csv)
                       (timezone . ,(current-timezone))
                       (use_labels_for_header . false)
                       (apikey . ,*api-key*))))

(define *db-file* "star.sqlite")

(define-values
  (update-lines-table! update-stops-table! update-routes-table!)
  (let ((make-table-updater
          (lambda (number-of-columns dataset-name table-name)
            (let ((uri (download-dataset-uri dataset-name))
                  (delete-statement-code (sprintf "DELETE FROM ~A" table-name))
                  (insert-statement-code
                    (sprintf
                      "INSERT INTO ~A VALUES (~A)"
                      table-name
                      (string-intersperse
                        (make-list number-of-columns "?")
                        ","))))
              (lambda ()
                (let ((data (call-with-input-request uri #f read-csv)))
                  (call-with-database
                    *db-file*
                    (lambda (db)
                      (let ((delete-statement (sql db delete-statement-code))
                            (insert-statement (sql db insert-statement-code)))
                        (with-transaction
                          db
                          (lambda ()
                            (exec delete-statement)
                            (for-each
                              (lambda (row) (apply exec insert-statement row))
                              (cdr data)))))))))))))
    (values
      (make-table-updater 9 "tco-bus-topologie-lignes-td" "lignes")
      (make-table-updater 9 "tco-bus-topologie-dessertes-td" "dessertes")
      (make-table-updater 17 "tco-bus-topologie-parcours-td" "parcours"))))


;; QUERIES

(define (list-lines)
  (call-with-database *db-file*
    (lambda (db)
      (query fetch-rows (sql db "SELECT id, nomcourt, nomlong FROM lignes ORDER BY id;")))))

;; TODO filtrer les arrêts terminus
(define lines-at-stop-query-code
#<<END
SELECT
dessertes.nomarret,
parcours.nomcourtligne,
parcours.nomarretarrivee,
dessertes.idarret,
parcours.idligne,
parcours.sens
FROM dessertes INNER JOIN parcours ON dessertes.idparcours = parcours.id
WHERE dessertes.nomarret LIKE ?
GROUP BY dessertes.idarret, parcours.idligne, parcours.sens
ORDER BY dessertes.idligne
END
)

(define (search-stops str)
  (call-with-database *db-file*
    (lambda (db)
      (query fetch-rows
             (sql db lines-at-stop-query-code)
             (string-append "%" str "%")))))
