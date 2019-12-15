(use-modules (gnome-2) (oop goops) (gnome gobject) (gnome gtk))

(define (add-to-list treeView str)
  (define model (get-model treeView))

  (set-value model (gtk-list-store-append model) 0 str))

(define window            (gtk-window-new 'toplevel))
(define hPaned            (gtk-hpaned-new))
(define directMessages    (gtk-tree-view-new))
(define          homeFeed (gtk-scrolled-window-new))
(define notificationsFeed (gtk-scrolled-window-new))
(define         localFeed (gtk-scrolled-window-new))
(define     federatedFeed (gtk-scrolled-window-new))
(define timelines         (gtk-notebook-new))
(define tootBoxFrame      (gtk-scrolled-window-new))
(define tootBox           (gtk-text-view-new-with-buffer))



(set-title        window "Mastodon")
(set-position     window 'center)
;; (set-border-width window 5)
(set-default-size window 370 270)


(let ([renderer (gtk-cell-renderer-text-new)]
      [column   (gtk-tree-view-column-new)])
  (set-title     column "Direct Messages")
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


(add1 hPaned directMessages)
(add2 hPaned timelines)

(add window hPaned)


(connect window 'destroy (lambda (w) (gtk-main-quit)))


(show-all window)
(gtk-main)