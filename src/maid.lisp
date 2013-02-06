(in-package :maid)

(proclaim '(optimize (speed 3)))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
                             :radix 16
                             :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (acc lst)
             (if lst
                 (case (car lst)
                   ;; Original codes in not tail-recursive form.
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

;;; Original function PARSE-PARAMS in not tail-recursive form.
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
             (let ((i1 (position #\= s))
                   (i2 (position #\& s)))
               (cond (i1 (aux (cons (cons (intern (string-upcase (subseq s 0 i1)))
                                          (decode-param (subseq s (1+ i1) i2)))
                                    acc)
                              (and i2 (subseq s (1+ i2)))))
                     ((equal s "") nil)
                     (t acc)))))
    (nreverse (aux '() s))))

(defun parse-method (s)
  (intern (string-upcase (subseq s 0 (position #\Space s)))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

;;; Original function GET-HEADER in not tail-recursive form.
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
             (let* ((s (the string (read-line stream)))
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
         (make-socket :connect :passive
                      :address-family :internet
                      :type :stream
                      :external-format '(:utf-8 :eol-style :crlf)
                      :ipv6 nil)))
    (bind-address server +ipv4-unspecified+
                  :port port :reuse-address t)
    (listen-on server :backlog 5)
    server))

(defun socket-server-close (server)
  (close server))

(defun socket-accept (socket)
  (accept-connection socket :wait t))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
         (progn
            (with-open-stream (stream (socket-accept socket))
              (let* ((line (read-line stream))
                     (url (parse-url line))
                     (method (parse-method line))
                     (path (car url))
                     (header (get-header stream))
                     (params (append (cdr url)
                                     (get-content-params stream header)))
                     ;; (*standard-output* stream)
                     )
                (write-header stream)
                ;; (funcall request-handler path header params)
                (princ (funcall request-handler method path header params) stream))))
      (socket-server-close socket))))

(defun write-header (stream)
  (format stream "HTTP/1.1 200 OK~%")
  (format stream "Date: ")
  (format-rfc1123-timestring stream (now))
  (format stream "~%")
  (format stream "Connection: close~%")
  (format stream "Server: Maid/0.0.2~%")
  (format stream "Content-Type: text/html~%")
  (format stream "~%"))

(defun hello-request-handler (method path header params)
  (declare (ignore header method))
  (with-output-to-string (*standard-output*)
    ;; (write-header *standard-output*)
    ;; (format t "HTTP/1.1 200 OK~%")
  ;; (format t "Date: ")
  ;; (format-rfc1123-timestring *standard-output* (now))
  ;; (format t "~%")
  ;; (format t "Connection: close~%")
  ;; (format t "Server: Maid/0.0.1~%")
  ;; (format t "Content-Type: text/html~%~%")
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry...I don't know that page."))))

(defun test-server ()
  (serve #'hello-request-handler))
