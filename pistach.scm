(use-modules (elefan auth)          (elefan timelines)
             (gnome-2)              (ice-9 textual-ports)
             (gnome gobject)        (oop   goops)
             (gnome gtk)           ((srfi  srfi-1)        #:select (fold))
             (gnome gw      pango)  (srfi  srfi-26))

(define (add-to-list treeView str)
  (define model (get-model treeView))

  (set-value model (gtk-list-store-append model) 0 str))

(define HOME_DIR  (let ([user (getlogin)])
                    (setpwent)

                    (let find-home ([aUser (getpwent)])
                      (cond
                       [(not aUser)                                             #f]
                       [(string=? user (passwd:name aUser))     (endpwent)

                                                                (passwd:dir aUser)]
                       [else                                (find-home (getpwent))]))))
(define APPS_FILE (string-append/shared HOME_DIR "/.config/pistach/apps"))
(define APPS      (let ([config (string-append/shared HOME_DIR "/.config")])
                    (if (and
                          (file-exists? config)
                          (eq? (stat:type (stat config)) 'directory))
                        (let ([dir (string-append/shared config "/pistach")])
                          (if (and
                                (file-exists? dir)
                                (eq? (stat:type (stat dir)) 'directory))
                              (let ([rc (string-append/shared dir "/apps")])
                                (if (file-exists? rc)
                                    (fold
                                      (lambda (elem result)
                                        (if (string=? (car elem) "domain")
                                            (cons (cons (cadr elem) '()) result)
                                          (let ([last (car result)])
                                            (cons
                                              (cons
                                                (car last)  ; domain
                                                (cons
                                                  (cons
                                                    (string->symbol (car elem))
                                                    (cadr elem))
                                                  (cdr last)))
                                              (cdr result)))))
                                      '()
                                      (map
                                        (lambda (str)
                                          (map string-trim-both (string-split str #\=)))
                                        (filter
                                          (lambda (str)
                                            (> (length (string-split str #\=)) 1))
                                          (string-split
                                            (call-with-input-file rc get-string-all)
                                            #\newline))))
                                  (begin
                                    (call-with-output-file rc (cut put-string <> ""))

                                    '())))
                            (begin
                              (mkdir dir)

                              '())))
                      '())))

(define window            (gtk-window-new 'toplevel))

;; Login structure
(define loginAlign        (gtk-alignment-new 0.5 0.5 0 0))
(define loginTable        (gtk-table-new 4 3 #t))
(define usernameLabel     (gtk-label-new "E-mail"))
(define usernameTextbox   (make <gtk-entry>))
(define instanceLabel     (gtk-label-new "Instance Domain"))
(define instanceTextbox   (make <gtk-entry>))
(define passwordLabel     (gtk-label-new "Password"))
(define passwordTextbox   (make <gtk-entry> #:visibility #f))
(define loginButton       (gtk-button-new-with-label "Login"))
(define (loginProcess widget)
  (catch #t
    (lambda ()
      (let* ([domain   (get-text instanceTextbox)]
             [ possApp    (assoc-ref APPS domain)]
             [finalApp (if possApp
                           (masto-app-instantiate
                             (string-append/shared "https://" domain)
                             #:id     (assoc-ref possApp 'id)
                             #:secret (assoc-ref possApp 'secret)
                             #:key    (assoc-ref possApp 'key)
                             #:scopes '("read" "write" "follow" "push"))
                         (let ([mApp (masto-app-instantiate
                                       (string-append/shared "https://" domain)
                                       #:scopes '("read" "write" "follow" "push"))])
                           (call-with-output-file
                               APPS_FILE
                             (cut put-string <> (string-append/shared
                                                  "domain="   domain
                                                  "\nid="     (masto-app-id     mApp)
                                                  "\nsecret=" (masto-app-secret mApp)
                                                  "\nkey="    (masto-app-key    mApp))))

                           mApp))])
        (masto-app-set-token-via-user-cred!
          finalApp
          (string-downcase (get-text usernameTextbox))
          (get-text passwordTextbox))

        (create-main finalApp)))
    (lambda _
      (define dialog (%gtk-message-dialog-new
                       window
                       (make <gtk-dialog-flags> #:value 'destroy-with-parent)
                       (make <gtk-message-type> #:value 'error)
                       (make <gtk-buttons-type> #:value 'ok)
                       "E-mail/Password and/or instance domain is incorrect."))

      (set-title dialog "Credentials Error")
      (run     dialog)
      (destroy dialog))))

;; Main structure
(define hPaned            (gtk-hpaned-new))
(define directMessages    (gtk-tree-view-new))
(define timelinesInfoHbox (gtk-hbox-new #t 0))
(define vPaned            (gtk-vpaned-new))
(define          homeFeed (gtk-scrolled-window-new))
(define notificationsFeed (gtk-scrolled-window-new))
(define         localFeed (gtk-scrolled-window-new))
(define     federatedFeed (gtk-scrolled-window-new))
(define timelines         (gtk-notebook-new))
(define vBox              (gtk-vbox-new #f 0))
(define tootBoxFrame      (gtk-scrolled-window-new))
(define tootBox           (gtk-text-view-new-with-buffer))
(define tootActionsHbox   (gtk-hbox-new #f 0))
(define attachmentButton  (gtk-button-new-with-label "Attach File"))
(define align             (gtk-alignment-new 1 0 0 1))
(define tootCharCount     (gtk-label-new "500"))
(define     boldAttrList  (let ([attrs (pango-attr-list-new)])
                            (pango-attr-list-insert attrs (pango-attr-weight-new 'bold))

                            attrs))
(define boldItalAttrList  (let ([attrs (pango-attr-list-new)])
                            (pango-attr-list-insert attrs (pango-attr-weight-new 'bold))
                            (pango-attr-list-insert attrs (pango-attr-style-new  'italic))

                            attrs))
(define placeholder       (gtk-label-new "Placeholder"))

(define (generate-status status)
  (define account     (masto-status-account status))
  (define  statusHbox (gtk-hbox-new #f 0))
  (define accountImg  (gtk-label-new "IMG"))
  (define  statusVbox (gtk-vbox-new #f 0))
  (define  statusName (gtk-label-new (masto-account-display-name account)))
  (define  statusText (gtk-label-new (masto-status-content       status)))

  (set-attributes statusName boldAttrList)
  (set-line-wrap  statusText #t)

  (pack-start statusVbox statusName #f #f 0)
  (pack-start statusVbox statusText #f #f 0)

  (pack-start statusHbox accountImg #f #f 0)
  (pack-start statusHbox statusVbox #t #t 0)

  statusHbox)

(define (create-main app)
  (let ([renderer (gtk-cell-renderer-text-new)]
        [column   (gtk-tree-view-column-new)])
    (set-title     column "Direct Messages")
    (set-alignment column 0.5)
    (pack-start    column renderer #f)
    (add-attribute column renderer "text" 0)

    (append-column directMessages column)
    (set-model     directMessages (gtk-list-store-new (list <gchararray>))))


  (map (lambda (feed)
         (set-policy feed
           (make <gtk-policy-type> #:value 'never)
           (make <gtk-policy-type> #:value 'automatic))) (list
                                                            homeFeed notificationsFeed
                                                           localFeed     federatedFeed))

  (set-tab-pos timelines (make <gtk-position-type> #:value 'top))
  (append-page timelines          homeFeed (gtk-label-new "Home"))
  (append-page timelines notificationsFeed (gtk-label-new "Notifications"))
  (append-page timelines         localFeed (gtk-label-new "Local"))
  (append-page timelines     federatedFeed (gtk-label-new "Federated"))


  (set-wrap-mode tootBox (make <gtk-wrap-mode> #:value 'word))
  (add tootBoxFrame tootBox)
  (set-policy      tootBoxFrame (make <gtk-policy-type> #:value 'automatic)
                                (make <gtk-policy-type> #:value 'automatic))
  (set-shadow-type tootBoxFrame (make <gtk-shadow-type> #:value 'etched-in))


  (set-image attachmentButton (gtk-image-new-from-stock
                                (gtk-stock-id 'add)
                                (make <gtk-icon-size> #:value 'button)))

  (add align tootCharCount)

  (pack-start tootActionsHbox attachmentButton #f #f  0)
  (pack-start tootActionsHbox align            #t #t 15)


  (pack-start vBox tootBoxFrame    #t #t 0)
  (pack-start vBox tootActionsHbox #f #f 3)


  (pack1 vPaned timelines #t #t)
  (pack2 vPaned vBox      #f #t)


  (pack-start timelinesInfoHbox vPaned      #t #t 0)
  (pack-start timelinesInfoHbox placeholder #t #t 0)


  (add1 hPaned directMessages)
  (add2 hPaned timelinesInfoHbox)


  (connect (get-buffer tootBox) 'changed (lambda (b)
                                           (let* ([chars (get-char-count b)]
                                                  [attrs (if (> chars 500)
                                                             boldItalAttrList
                                                           (pango-attr-list-new))])
                                             (set-attributes tootCharCount attrs)

                                             (set-markup
                                               tootCharCount
                                               (number->string (- 500 chars))))))


  (add-with-viewport homeFeed (generate-status (car (masto-timelines-home app #:limit 1))))


  (remove   window loginAlign)
  (add      window hPaned)
  (show-all window))



(let ([FILLorSHRINK (make <gtk-attach-options> #:value '(fill shrink))])
  (map (cut set-attributes <> boldAttrList) (list usernameLabel instanceLabel passwordLabel))

  (attach loginTable usernameLabel   0 1 0 1 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable usernameTextbox 0 1 1 2 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable instanceLabel   1 2 0 1 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable instanceTextbox 1 2 1 2 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable passwordLabel   2 3 0 1 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable passwordTextbox 2 3 1 2 FILLorSHRINK FILLorSHRINK 3 0)
  (attach loginTable loginButton     1 2 3 4 FILLorSHRINK FILLorSHRINK 0 0))

(add loginAlign loginTable)
(add window     loginAlign)



(set-title        window "Mastodon")
(set-position     window 'center)
(set-border-width window 7)
(set-default-size window 370 270)



(connect usernameTextbox 'activate loginProcess)
(connect instanceTextbox 'activate loginProcess)
(connect passwordTextbox 'activate loginProcess)
(connect loginButton     'clicked  loginProcess)
(connect window          'destroy  (lambda (w) (gtk-main-quit)))


(show-all window)
(gtk-main)
