(in-package #:pai-v3.kernel)

;;; Base64url (RFC 4648 §5, unpadded) and a minimal CBOR decoder (RFC 8949)
;;; covering what WebAuthn uses: unsigned and negative integers, byte and
;;; text strings, arrays, maps and simple values. Definite lengths only.

(defparameter +b64url+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(defun base64url-encode (octets)
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for n = (min 3 (- (length octets) i))
          for v = (loop for j below 3
                        sum (ash (if (< j n) (aref octets (+ i j)) 0) (* 8 (- 2 j))))
          do (loop for j below (1+ n)
                   do (write-char (char +b64url+ (ldb (byte 6 (* 6 (- 3 j))) v)) out)))))

(defun base64url-decode (string)
  (let ((clean (string-right-trim "=" (substitute #\- #\+ (substitute #\_ #\/ string))))
        (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length clean) by 4
          for n = (min 4 (- (length clean) i))
          for v = (loop for j below 4
                        sum (ash (if (< j n)
                                     (or (position (char clean (+ i j)) +b64url+)
                                         (error "Invalid base64url character"))
                                     0)
                                 (* 6 (- 3 j))))
          do (loop for j below (1- n)
                   do (vector-push-extend (ldb (byte 8 (* 8 (- 2 j))) v) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun cbor-decode (octets &key (start 0))
  "Decode one CBOR item from OCTETS at START. Returns the item and the
position after it. Maps become alists ((key . value) ...) to keep integer
keys; byte strings stay octet vectors; text becomes strings."
  (let ((pos start))
    (labels ((u8 () (prog1 (aref octets pos) (incf pos)))
             (uint (bytes) (loop repeat bytes for v = (u8) then (logior (ash v 8) (u8))
                                 finally (return v)))
             (argument (info)
               (cond ((< info 24) info)
                     ((= info 24) (uint 1)) ((= info 25) (uint 2))
                     ((= info 26) (uint 4)) ((= info 27) (uint 8))
                     (t (error "Unsupported CBOR length ~d" info))))
             (item ()
               (let* ((initial (u8))
                      (major (ash initial -5))
                      (info (logand initial 31)))
                 (case major
                   (0 (argument info))
                   (1 (- -1 (argument info)))
                   (2 (let ((n (argument info)))
                        (prog1 (subseq octets pos (+ pos n)) (incf pos n))))
                   (3 (let ((n (argument info)))
                        (prog1 (babel:octets-to-string octets :start pos :end (+ pos n)
                                                              :encoding :utf-8)
                          (incf pos n))))
                   (4 (loop repeat (argument info) collect (item)))
                   (5 (loop repeat (argument info) collect (cons (item) (item))))
                   (7 (case info (20 :false) (21 t) (22 :null)
                        (t (error "Unsupported CBOR simple value ~d" info))))
                   (t (error "Unsupported CBOR major type ~d" major))))))
      (values (item) pos))))
