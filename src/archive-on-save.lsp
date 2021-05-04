; Archive on save
;
; Description:
;   Augments _QSAVE functionality to create an archived version of the file prior to saving.
;   Tested on Civil 3D 2020.
;
; Behavior:
;   On save, searches for the most recently archived version of the current drawing, saved as ./Archive/VOID_<file-name>_YYYY-MM-DD_hh-mm-ss.<file-extension>, relevative to the directory the file is saved in. If the most recent archive, as determined by filename, not file properties, is older than +archive-threshhold+ days, the file is archived into the ./Archive/ folder with a filename containing the time of archival, not the file's last modification property. If the file is newly created, defined as having been edited less than +archive-edit-threshhold+ days and created less than +archive-threshhold+ days ago, the file will not be archived.
;
; Configurable values:
;   +archive-threshhold+      : [default = 1 day]   number of days old a file needs to be to get auto-archived (can be fractional)
;   +archive-edit-threshhold+ : [default = 2 hours] number of days old a file must be opened for to get auto-archived (can be fractional)
;
; TODO: Add error handling
; TODO: Convert archive function to command for use outside of saving

; Load extensions
(load (findfile "julian.lsp"))
(vl-load-com)

(command "undefine" "_QSAVE")

; Return date as list of strings '(YYYY MM DD hh mm ss)
(defun get-date-string (/ date-string YYYY MM DD H M S) 
  (setq date-string (rtos (getvar "cdate") 2 6))
  (setq YYYY (substr date-string 1 4)
        MM   (substr date-string 5 2)
        DD   (substr date-string 7 2)
        H    (substr date-string 10 2)
        M    (substr date-string 12 2)
        S    (substr date-string 14 2)
  )
  (strcat YYYY "-" MM "-" DD "_" H "-" M "-" S)
)

; Archive specified file in ./Archive/ relative to parent
(defun arch-dwg (file-dir file-name file-ext / arch-dir arch-prefix new-arch-path) 
  (setq arch-dir (strcat file-dir "Archive\\"))
  ; Create Archive directory if needed
  (if (not (vl-file-directory-p arch-dir)) 
    (vl-mkdir arch-dir)
  )
  (setq arch-prefix (strcat "VOID_" file-name "_"))
  (setq new-arch-path (strcat arch-dir arch-prefix (get-date-string) "." file-ext))
  (vl-file-copy (strcat file-dir file-name "." file-ext) new-arch-path)
)

; Find date of most recent "version 1" named archive of specified file
; The "version 1" naming convension is defined as "VOID-*-YYYY-MM-DD.*" with an optional "(1)" after the days
; More specifcally, with regex: VOID-.*-\d{4}\-\d{2}\-\d{2}[(1)]*\.[^.]*$
(defun get-last-v1-arch-date (file-dir file-name file-ext / arch-dir arch-prefix 
                              arch-list most-recent-arch most-recent-arch-date
                             ) 
  (setq arch-dir (strcat file-dir "Archive\\"))
  (setq arch-prefix (strcat "VOID-" file-name "-"))
  ; Create a list of all v1 archives for the specified file
  (setq arch-list (vl-directory-files arch-dir 
                                      (strcat arch-prefix "????-??-??." file-ext)
                                      1
                  )
  )
  ; If list was empty, no archives exist
  (if arch-list 
    (progn 
      ; Find most recent archive in list
      ; Variants with a "(1)" prefix can be ignored because those were previously only created when a non-"(1)" variant existed for a date
      (setq most-recent-arch (car (vl-sort arch-list '>)))
      (setq most-recent-arch-date (substr most-recent-arch 
                                          (- (strlen most-recent-arch) 
                                             (+ 10 (strlen file-ext))
                                          )
                                          10
                                  )
      )
      ; Convert date string "YYYY-MM-DD" to format 20210417.1037 = 2020/04/17 10:37AM
      ; Assumes midnight as time since not specified in this naming convention
      (setq most-recent-arch-date (strcat (substr most-recent-arch-date 1 4) 
                                          (substr most-recent-arch-date 6 2)
                                          (substr most-recent-arch-date 9 2)
                                  )
      )
    )
    (setq most-recent-arch-date nil)
  )
)

; Find date of most recent "version 2" named archive of specified file
; The "version 2" naming convension is defined as "VOID_*_YYYY-MM-DD_hh_mm_ss.*"
; More specifcally, with regex: VOID_.*_\d{4}\-\d{2}\-\d{2}_\d{2}-\d{2}-\d{2}\.[^.]*$
(defun get-last-v2-arch-date (file-dir file-name file-ext / arch-dir arch-prefix 
                              arch-list most-recent-arch most-recent-arch-date
                             ) 
  (setq arch-dir (strcat file-dir "Archive\\"))
  (setq arch-prefix (strcat "VOID_" file-name "_"))
  ; Create a list of all v1 archives for the specified file
  (setq arch-list (vl-directory-files arch-dir 
                                      (strcat arch-prefix "????-??-??_??-??-??." file-ext)
                                      1
                  )
  )
  ; If list was empty, no archives exist
  (if arch-list 
    (progn 
      ; Find most recent archive in list
      (setq most-recent-arch (car (vl-sort arch-list '>)))
      (setq most-recent-arch-date (substr most-recent-arch 
                                          (- (strlen most-recent-arch) 
                                             (+ 19 (strlen file-ext))
                                          )
                                          19
                                  )
      )
      ; Convert date string "YYYY-MM-DD_hh_mm_ss" to format 20210417.103702 = 2020/04/17 10:37:02AM
      (setq most-recent-arch-date (strcat (substr most-recent-arch-date 1 4) 
                                          (substr most-recent-arch-date 6 2)
                                          (substr most-recent-arch-date 9 2)
                                          "."
                                          (substr most-recent-arch-date 12 2)
                                          (substr most-recent-arch-date 15 2)
                                          (substr most-recent-arch-date 18 2)
                                  )
      )
    )
    (setq most-recent-arch-date nil)
  )
)

; Find date of most recent archive of specified file
; Returns date in format: 20210417.1037 = 2020/04/17 10:37AM
(defun get-last-arch-date (file-dir file-name file-ext / last-v1-arch-date 
                           last-v2-arch-date last-arch-date
                          ) 
  (setq last-v1-arch-date (get-last-v1-arch-date file-dir file-name file-ext))
  (setq last-v2-arch-date (get-last-v2-arch-date file-dir file-name file-ext))

  ; Return most recent of the two
  (if (> last-v1-arch-date last-v2-arch-date) 
    (setq last-arch-date last-v1-arch-date)
    (setq last-arch-date last-v2-arch-date)
  )
  ; Check if archive was found
  (if (not last-arch-date) 
    (setq last-arch-date nil)
    (setq last-arch-date (distof last-arch-date))
  )
)

; Finds difference between two dates in days
; Expected date format: 20210417.1037 = 2020/04/17 10:37AM
(defun day-diff (date1 date2) 
  (- (dtoj date1) (dtoj date2))
)

(defun get-days-since-arch (file-dir file-name file-ext / last-arch-date) 
  (setq last-arch-date (get-last-arch-date file-dir file-name file-ext))
  (if (not last-arch-date) 
    (setq last-arch-date 19700101)
    (day-diff 
      (getvar 'cdate)
      last-arch-date
    )
  )
)

; Rounds number to the nearest multiple "mult"
; Round Multiple  -  Lee Mac
; Adapted from http://www.lee-mac.com/round.html
(defun round-mult (num mult) 
  (* mult (atoi (rtos (/ num (float mult)) 2 0)))
)
; Round number to specified decimal places
; Round To  -  Lee Mac
; Adapted from http://www.lee-mac.com/round.html
(defun round-num (num decimals) 
  (round-mult num (expt 10.0 (- decimals)))
)

(defun C:QSAVE (/ cmdecho-initial +archive-threshhold+ +fifty-years+ 
                +archive-edit-threshhold+ file-dir file-name-full file-name-full-len 
                start-of-ext file-name file-ext days-since-arch
               ) 
  (setq cmdecho-initial (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (initdia 1)
  (setq +archive-threshhold+ 1) ; number of days old a file needs to be to get auto-archived (can be fractional)
  (setq +archive-edit-threshhold+ 0.0833) ; number of days old a file must be opened for to get auto-archived (can be fractional)
  (setq +fifty-years+ 18250) ; number of days in 50 years. Treats >50 year old archive as non-existent

  ; Check if drawing is saved anywhere
  (if (= (getvar "dwgtitled") 1) 
    (progn 
      (setq file-dir (getvar "dwgprefix")) ; H:\\12345-Project\\
      (setq file-name-full (getvar "dwgname")) ; 12345-MSTR.dwg
      ; Separate filename from extension
      (setq file-name-full-len (strlen file-name-full))
      (setq start-of-ext (vl-string-position (ascii ".") file-name-full nil t))
      (setq file-name (substr file-name-full 1 start-of-ext)) ; 12345-MSTR
      (setq file-ext (substr file-name-full (+ start-of-ext 2))) ; dwg

      (setq days-since-arch (get-days-since-arch file-dir file-name file-ext))
      ; Print easily readable days-since-arch
      (cond 
        ((> days-since-arch +fifty-years+)
         (princ "\nDrawing has never been archived.")
        )
        ((> days-since-arch 365)
         (princ 
           (strcat "\nDrawing last archived " 
                   (rtos (round-num days-since-arch 1) 2)
                   " year(s) ago."
           )
         )
        )
        ((> days-since-arch 30)
         (princ 
           (strcat "\nDrawing last archived " 
                   (rtos (round-num (/ days-since-arch 30) 1) 2)
                   " month(s) ago."
           )
         )
        )
        ((> days-since-arch 2)
         (princ 
           (strcat "\nDrawing last archived " 
                   (rtos (round-num days-since-arch 1) 2)
                   " days(s) ago."
           )
         )
        )
        ((> days-since-arch 0.08333)
         (princ 
           (strcat "\nDrawing last archived " 
                   (rtos (round-num (* 24 days-since-arch) 1) 2)
                   " hours(s) ago."
           )
         )
        )
        ((> days-since-arch 0.000694)
         (princ 
           (strcat "\nDrawing last archived " 
                   (rtos (round-num (* 1440 days-since-arch) 1) 2)
                   " minutes(s) ago."
           )
         )
        )
        (t
         (princ (strcat "\nDrawing last archived less than a minute ago."))
        )
      )

      (if (> days-since-arch +archive-threshhold+) 
        (progn 
          (if 
            (and (> days-since-arch +fifty-years+) 
                 (< (getvar "TDINDWG") +archive-edit-threshhold+)
                 (> +archive-threshhold+ 
                    (day-diff (getvar "date") (getvar "tdcreate"))
                 )
            )
            (princ "\nDrawing is too new to warrant automatic archival.\n")
            (progn 
              (princ "\nAutomatically archiving drawing prior to saving.\n")
              (arch-dwg file-dir file-name file-ext)
              ; TODO: Add error handling
            )
          )
        )
        (princ "\nA recent archive exists for this drawing.\n")
      )
    )
    (princ "\nThis drawing is new and cannot be archived prior to saving.\n")
  )
  (command ".QSAVE")
  (setvar "cmdecho" cmdecho-initial)
  (princ)
)
(princ)
