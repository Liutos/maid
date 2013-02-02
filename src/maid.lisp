(in-package :maid)

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
                             :radix 16
                             :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (acc lst)
             (declare (optimize (speed 3)))
             (if lst
                 (case (car lst)
                   ;; (#\% (cons (http-char (cadr lst) (caddr lst))
                   ;;            (f (cdddr lst))))
                   ;; (#\+ (cons #\Space (f (cdr lst))))
                   ;; (otherwise (cons (car lst) (f (cdr lst))))
                   (#\% (f (cons (http-char (cadr lst) (caddr lst)) acc)
                           (cdddr lst)))
                   (#\+ (f (cons #\Space acc) (cdr lst)))
                   (otherwise (f (cons (car lst) acc) (cdr lst))))
                 acc)))
    (coerce (nreverse (f '() (coerce s 'list))) 'string)))

;; (defun parse-params (s)
;;   (let* ((i1 (position #\= s))
;;          (i2 (position #\& s)))
;;     (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
;;                           (decode-param (subseq s (1+ i1) i2)))
;;                     (and i2 (parse-params (subseq s (1+ i2))))))
;;           ((equal s "") nil)
;;           (t s))))

(defun parse-params (s)
  (labels ((aux (acc s)
             (declare (optimize (speed 3)))
             (let ((i1 (position #\= s))
                   (i2 (position #\& s)))
               (cond (i1 (aux (cons (cons (intern (string-upcase (subseq s 0 i1)))
                                          (decode-param (subseq s (1+ i1) i2)))
                                    acc)
                              (and i2 (subseq s (1+ i2)))))
                     ((equal s "") nil)
                     (t acc)))))
    (nreverse (aux '() s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

;; (defun get-header (stream)
;;   (let* ((s (read-line stream))
;;          (h (let ((i (position #\: s)))
;;               (when i
;;                 (cons (intern (string-upcase (subseq s 0 i)))
;;                       (subseq s (+ i 2)))))))
;;     (when h
;;       (cons h (get-header stream)))))

(defun get-header (stream)
  (labels ((aux (acc stream)
             (declare (optimize (speed 3)))
             (let* ((s (read-line stream))
                    (h (let ((i (position #\: s)))
                         (when i
                           (cons (intern (string-upcase (subseq s 0 i)))
                                 (subseq s (+ i 2)))))))
               (if h
                   (aux (cons h acc) stream)
                   acc))))
    (nreverse (aux '() stream))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun socket-server (port)
  (let ((server
         (iolib:make-socket :connect :passive
                     :address-family :internet
                     :type :stream
                     :external-format '(:utf-8 :eol-style :crlf)
                     :ipv6 nil)))
    (iolib:bind-address server iolib:+ipv4-unspecified+
                        :port port :reuse-address t)
    (iolib:listen-on server :backlog 5)
    server))

(defun socket-server-close (server)
  (close server))

(defun socket-accept (socket)
  (iolib:accept-connection socket :wait t))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
         (loop
            (with-open-stream (stream (socket-accept socket))
              (let* ((url (parse-url (read-line stream)))
                     (path (car url))
                     (header (get-header stream))
                     (params (append (cdr url)
                                     (get-content-params stream header)))
                     (*standard-output* stream))
                (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry...I don't know that page.")))

(defun test-server ()
  (serve #'hello-request-handler))
