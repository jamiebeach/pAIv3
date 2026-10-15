(in-package #:pai-v3.mind)

;;; Token estimates are stored with rows at write time (spec §5.2) and
;;; recalibrated from provider-reported usage (spec §14). Characters per
;;; token is the single calibration parameter; v2's bytes-as-tokens estimate
;;; overstated requests about 4.8x.

(defvar *chars-per-token* 3.6)

(defun estimate-tokens (text)
  (if (zerop (length text))
      0
      (max 1 (ceiling (length text) *chars-per-token*))))
