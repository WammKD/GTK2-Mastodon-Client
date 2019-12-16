(use-modules (gnome-2) (oop goops) (gnome gobject) (gnome gtk))

(define (add-to-list treeView str)
  (define model (get-model treeView))

  (set-value model (gtk-list-store-append model) 0 str))

(define window            (gtk-window-new 'toplevel))
(define hPaned            (gtk-hpaned-new))
(define directMessages    (gtk-tree-view-new))
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
(define tootCharCount     (gtk-label-new "0"))



(set-title        window "Mastodon")
(set-position     window 'center)
;; (set-border-width window 5)
(set-default-size window 370 270)


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
         (make <gtk-policy-type> #:value 'automatic)
         (make <gtk-policy-type> #:value 'automatic))) (list
                                                          homeFeed notificationsFeed
                                                         localFeed     federatedFeed))

(set-tab-pos timelines (make <gtk-position-type> #:value 'top))
(append-page timelines          homeFeed (gtk-label-new "Home"))
(append-page timelines notificationsFeed (gtk-label-new "Notifications"))
(append-page timelines         localFeed (gtk-label-new "Local"))
(append-page timelines     federatedFeed (gtk-label-new "Federated"))


(add tootBoxFrame tootBox)
(set-policy      tootBoxFrame (make <gtk-policy-type> #:value 'automatic)
                              (make <gtk-policy-type> #:value 'automatic))
(set-shadow-type tootBoxFrame (make <gtk-shadow-type> #:value 'etched-in))


(pack1 vPaned timelines    #t #t)
(pack2 vPaned tootBoxFrame #f #t)
(set-image attachmentButton (gtk-image-new-from-stock
                              (gtk-stock-id 'add)
                              (make <gtk-icon-size> #:value 'button)))

(add align tootCharCount)

(pack-start tootActionsHbox attachmentButton #f #f  0)
(pack-start tootActionsHbox align            #t #t 15)


(pack-start vBox tootBoxFrame    #t #t 0)
(pack-start vBox tootActionsHbox #f #f 3)




(add1 hPaned directMessages)
(add2 hPaned vPaned)


(add window hPaned)


(connect window 'destroy (lambda (w) (gtk-main-quit)))


(show-all window)
(gtk-main)
