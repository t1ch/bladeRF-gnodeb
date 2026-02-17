;;; ==================================================================
;;; cl-bladerf-gnodeb.lisp - Unified FAPI Test Suite v11.0
;;;
;;; SCF-222 compliant: ALL messages use the same common header, and
;;; message lengths are derived from the header's msg_length field.
;;;
;;; v11.0 changes vs v10.0:
;;;   - Full DL_TTI.req body builder with PDU-level structure
;;;     (SCF-222 Table 3.4.2-1: SFN/slot, nPDUs, PDU headers,
;;;      PDSCH body fields: bwpSize, bwpStart, scs, cp, pduIndex, rnti)
;;;   - Full TX_DATA.req body builder with TLV-based TB payload
;;;     (SCF-222 Table 3.4.6-1: SFN/slot, controlLength/nPDUs,
;;;      pduLength, pduIndex/cwIndex, numTLV, tag, length, value)
;;;   - DL_TTI.response (0x8A) verification with nPDUs/nPdsch fields
;;;   - Dedicated test phases for DL_TTI and TX_DATA message families
;;;   - Retained all v10.0 config and generic slot tests
;;;
;;; ── SCF-222 §3.1.1 Wire Format ─────────────────────────────────
;;;
;;; TX  (host → FPGA):
;;;   Dword 0: [num_msg(8) | handle(16) | phy_id(8)]
;;;   Dword 1: [msg_type_id(16) | msg_length(16)]
;;;   Dwords 2..2+N-1: body dwords  (N = ceil(msg_length / 4))
;;;   Dword 2+N:     0x00000000  (padding)
;;;   Dword 2+N+1:   0x0000000D  (sentinel, EOP)
;;;
;;; RX  (FPGA → host):
;;;   Same layout; the parser extracts N from msg_length in dword 1.
;;;
;;; ── DL_TTI.request body (SCF-222 Table 3.4.2-1) ────────────────
;;;
;;;   Body DW0: [SFN(16) | Slot(16)]
;;;   Body DW1: [nPDUs(16) | nDlTypes(8) | pad(8)]
;;;   Body DW2: [numPDUsOfEachType — skipped by PoC FPGA]
;;;   Body DW3: [numGroups(16) | pad(16) — skipped by PoC FPGA]
;;;   Per PDU:
;;;     [pduType(16) | pduSize(16)]
;;;     PDSCH body DW0: [bwpSize(16) | bwpStart(16)]
;;;     PDSCH body DW1: [scs(8) | cp(8) | pduIndex(16)]
;;;     PDSCH body DW2: [rnti(16) | pad(16)]
;;;
;;; ── DL_TTI.response body (SCF-222 Table 3.4.2b-1) ─────────────
;;;
;;;   Body DW0: [SFN(16) | Slot(16)]
;;;   Body DW1: [nPDUs(8) | nPdsch(8) | errCode(8) | phyState(8)]
;;;
;;; ── TX_DATA.request body (SCF-222 Table 3.4.6-1) ───────────────
;;;
;;;   Body DW0: [SFN(16) | Slot(16)]
;;;   Body DW1: [controlLength(16) | nPDUs(16)]
;;;   Per PDU:
;;;     [pduLength(32)]
;;;     [pduIndex(16) | cwIndex(8) | pad(8)]
;;;     [numTLV(32)]
;;;     [tag(16) | pad(16)]
;;;     [length(32)]            ← TB size in bytes
;;;     [value dwords...]       ← inline TB payload (tag 0 or 3)
;;;
;;; ==================================================================

(in-package #:cl-bladerf-gnodeb)

;;; ===================================================================
;;; FAPI Message Type IDs (SCF-222 Table 3.2-1) — 16-bit on wire
;;; ===================================================================

;; Configuration messages
(defconstant +fapi-param-request+    #x0000)
(defconstant +fapi-param-response+   #x0001)
(defconstant +fapi-config-request+   #x0002)
(defconstant +fapi-config-response+  #x0003)
(defconstant +fapi-start-request+    #x0004)
(defconstant +fapi-stop-request+     #x0005)
(defconstant +fapi-stop-indication+  #x0006)
(defconstant +fapi-error-indication+ #x0007)
(defconstant +fapi-reset-request+    #x0008)
(defconstant +fapi-reset-indication+ #x0009)
(defconstant +fapi-connectivity-indication+ #x000A)

;; Slot-level messages
(defconstant +fapi-dl-tti-request+   #x0080)
(defconstant +fapi-ul-tti-request+   #x0081)
(defconstant +fapi-slot-indication+  #x0082)
(defconstant +fapi-ul-dci-request+   #x0083)
(defconstant +fapi-tx-data-request+  #x0084)
(defconstant +fapi-rx-data-indication+  #x0085)
(defconstant +fapi-crc-indication+      #x0086)
(defconstant +fapi-uci-indication+      #x0087)
(defconstant +fapi-srs-indication+      #x0088)
(defconstant +fapi-rach-indication+     #x0089)
(defconstant +fapi-dl-tti-response+     #x008A)
(defconstant +fapi-timing-indication+   #x008B)
(defconstant +fapi-ul-metrics-indication+      #x008C)
(defconstant +fapi-rim-rs-indication+          #x008D)  ; also ADVANCE_SLEEP_CONTROL.req (direction-dependent)
(defconstant +fapi-adv-sleep-ctrl-request+     #x008D)  ; shared ID with RIM_RS.ind
(defconstant +fapi-adv-sleep-ctrl-indication+  #x008E)

;;; ===================================================================
;;; DL PDU Type IDs (SCF-222 Table 3.4.2-1)
;;; ===================================================================

(defconstant +dl-pdu-pdcch+   0)
(defconstant +dl-pdu-pdsch+   1)
(defconstant +dl-pdu-csi-rs+  2)
(defconstant +dl-pdu-ssb+     3)

;;; ===================================================================
;;; TX_DATA TLV Tags (SCF-222 Table 3.4.6-2)
;;; ===================================================================

(defconstant +tlv-tag-inline-payload+  0)   ; Tag 0: inline MAC PDU
(defconstant +tlv-tag-pointer+         1)   ; Tag 1: pointer (not supported in PoC)
(defconstant +tlv-tag-inline-aligned+  3)   ; Tag 3: inline aligned payload

;;; ===================================================================
;;; PHY State / Error Codes
;;; ===================================================================

(defconstant +phy-idle+       #x00)
(defconstant +phy-configured+ #x01)
(defconstant +phy-running+    #x02)

(defconstant +msg-ok+             #x00)
(defconstant +msg-invalid-state+  #x01)

(defconstant +eop-sentinel+ #x0000000D)

;;; Stream configuration
(defconstant +stream-timeout-ms+ 3000)

;;; ===================================================================
;;; Name lookups (for diagnostics)
;;; ===================================================================

(defun msg-type-name (id)
  (case id
    ;; Configuration messages
    (#x0000 "PARAM.req")   (#x0001 "PARAM.resp")
    (#x0002 "CONFIG.req")  (#x0003 "CONFIG.resp")
    (#x0004 "START.req")   (#x0005 "STOP.req")
    (#x0006 "STOP.ind")    (#x0007 "ERROR.ind")
    (#x0008 "RESET.req")   (#x0009 "RESET.ind")
    (#x000A "CONNECTIVITY.ind")
    ;; Slot-level messages
    (#x0080 "DL_TTI.req")  (#x0081 "UL_TTI.req")
    (#x0082 "SLOT.ind")    (#x0083 "UL_DCI.req")
    (#x0084 "TX_DATA.req") (#x0085 "RX_DATA.ind")
    (#x0086 "CRC.ind")     (#x0087 "UCI.ind")
    (#x0088 "SRS.ind")     (#x0089 "RACH.ind")
    (#x008A "DL_TTI.resp") (#x008B "TIMING.ind")
    (#x008C "UL_METRICS.ind")
    (#x008D "RIM_RS.ind/ADV_SLEEP.req")  ; shared ID, direction-dependent
    (#x008E "ADV_SLEEP.ind")
    ;; Other
    (#x00FF "NULL/default")
    (t (format nil "UNK(0x~4,'0X)" id))))

(defun phy-state-name (val)
  (case val
    (#x00 "IDLE") (#x01 "CONFIGURED") (#x02 "RUNNING")
    (t (format nil "UNK(0x~2,'0X)" val))))

(defun dl-pdu-type-name (id)
  (case id
    (0 "PDCCH") (1 "PDSCH") (2 "CSI-RS") (3 "SSB")
    (t (format nil "UNK(~D)" id))))

;;; ===================================================================
;;; Utility: ceiling division for body-length → dword-count
;;; ===================================================================

(defun body-len-to-dwords (byte-len)
  "Convert body length in bytes to number of dwords (ceiling div by 4)."
  (ceiling byte-len 4))

;;; ===================================================================
;;; Device lifecycle: shutdown
;;; ===================================================================

(defun shutdown-bladerf (dev)
  (format t "~%=== Shutting down bladeRF ===~%")
  (flet ((disable (channel label)
           (let ((st (cl-bladerf::bladerf_enable_module dev channel nil)))
             (format t "  disable ~A: ~:[FAIL(~D)~;OK~]~%" label (>= st 0) st))))
    (disable (cl-bladerf::channel-rx 0) "RX")
    (sleep 0.1)
    (disable (cl-bladerf::channel-tx 0) "TX")
    (sleep 0.1))
  (format t "  modules disabled, safe to close~%"))

(defmacro with-safe-bladerf ((dev-var dev-str) &body body)
  `(cl-bladerf::with-bladerf-device (,dev-var ,dev-str)
     (unwind-protect
          (progn ,@body)
       (handler-case
           (shutdown-bladerf ,dev-var)
         (error (c)
           (format t "  WARNING: shutdown error: ~A~%" c))))))

;;; ===================================================================
;;; Device configuration
;;; ===================================================================

(defun configure-bladerf-for-fapi-duplex (dev)
  (let ((status 0))
    (format t "~%=== Configuring bladeRF for FAPI duplex (PACKET_META) ===~%")
    (macrolet ((check (expr label)
                 `(progn
                    (setf status ,expr)
                    (format t "  ~A: ~:[FAIL(~D)~;OK~]~%" ,label (>= status 0) status)
                    (when (< status 0)
                      (return-from configure-bladerf-for-fapi-duplex nil)))))

      (check (cl-bladerf::bladerf_sync_config
              dev :BLADERF_TX_X1 :BLADERF_FORMAT_PACKET_META
              4096 4096 16 +stream-timeout-ms+)
             "sync-config TX")
      (check (cl-bladerf::bladerf_sync_config
              dev :BLADERF_RX_X1 :BLADERF_FORMAT_PACKET_META
              4096 4096 16 +stream-timeout-ms+)
             "sync-config RX")

      (check (cl-bladerf::bladerf_set_frequency dev (cl-bladerf::channel-tx 0) 918000000)
             "freq TX 918MHz")
      (check (cl-bladerf::bladerf_set_frequency dev (cl-bladerf::channel-rx 0) 918000000)
             "freq RX 918MHz")
      (check (cl-bladerf::bladerf_set_sample_rate dev (cl-bladerf::channel-tx 0) 1000000 (cffi:null-pointer))
             "srate TX 1MHz")
      (check (cl-bladerf::bladerf_set_sample_rate dev (cl-bladerf::channel-rx 0) 1000000 (cffi:null-pointer))
             "srate RX 1MHz")
      (check (cl-bladerf::bladerf_set_bandwidth dev (cl-bladerf::channel-tx 0) 2000000 (cffi:null-pointer))
             "bw TX 2MHz")
      (check (cl-bladerf::bladerf_set_bandwidth dev (cl-bladerf::channel-rx 0) 2000000 (cffi:null-pointer))
             "bw RX 2MHz")

      (check (cl-bladerf::bladerf_enable_module dev (cl-bladerf::channel-tx 0) t)
             "enable TX")
      (check (cl-bladerf::bladerf_enable_module dev (cl-bladerf::channel-rx 0) t)
             "enable RX")

      (check (cl-bladerf::bladerf_set_gain_mode dev (cl-bladerf::channel-rx 0) :BLADERF_GAIN_MGC)
             "gain-mode RX MGC")
      (check (cl-bladerf::bladerf_set_gain dev (cl-bladerf::channel-rx 0) 60)
             "gain RX 60dB")
      (check (cl-bladerf::bladerf_set_gain_stage dev (cl-bladerf::channel-tx 0) "dsa" 0)
             "gain TX DSA 0dB"))

    (let ((speed (cl-bladerf::bladerf_device_speed dev)))
      (format t "  USB speed: ~A ~A~%" speed
              (if (eq speed :BLADERF_DEVICE_SPEED_SUPER) "(SuperSpeed - OK)"
                  "(WARNING: not SuperSpeed)")))
    t))

;;; ===================================================================
;;; Common Header Builders
;;; ===================================================================

(defun build-header-dw0 (&key (num-msg 1) (handle 0) (phy-id 0))
  "Build header dword 0: [num_msg(8) | handle(16) | phy_id(8)]"
  (logior (ash (logand num-msg #xFF) 24)
          (ash (logand handle #xFFFF) 8)
          (logand phy-id #xFF)))

(defun build-header-dw1 (msg-type-id msg-length-bytes)
  "Build header dword 1: [msg_type_id(16) | msg_length(16)]"
  (logior (ash (logand msg-type-id #xFFFF) 16)
          (logand msg-length-bytes #xFFFF)))

;;; ===================================================================
;;; TX: Unified variable-length FAPI request builder
;;; ===================================================================

(defun build-fapi-request (msg-type &key (body-dwords nil)
                                         (handle 0) (phy-id 0))
  "Build a FAPI request packet with the SCF-222 common header and
a variable-length body.  BODY-DWORDS is a list of 32-bit values
forming the message body.  msg_length in the header is set to
(* 4 (length body-dwords)).

Returns (values foreign-pointer dword-count).

Wire layout:
  Dword 0: header dw0  [num_msg(8) | handle(16) | phy_id(8)]
  Dword 1: header dw1  [msg_type_id(16) | msg_length(16)]
  Dwords 2..2+N-1: body dwords
  Dword 2+N:       0x00000000  (padding)
  Dword 2+N+1:     0x0000000D  (sentinel, EOP)"
  (let* ((n-body (length body-dwords))
         (msg-length-bytes (* 4 n-body))
         ;; Total dwords: 2 (header) + N (body) + 1 (padding) + 1 (sentinel)
         (n-total (+ 2 n-body 1 1))
         (buf (cffi:foreign-alloc :uint32 :count n-total :initial-element 0)))
    ;; Header
    (setf (cffi:mem-aref buf :uint32 0)
          (build-header-dw0 :handle handle :phy-id phy-id))
    (setf (cffi:mem-aref buf :uint32 1)
          (build-header-dw1 msg-type msg-length-bytes))
    ;; Body dwords
    (loop for dw in body-dwords
          for i from 2
          do (setf (cffi:mem-aref buf :uint32 i) dw))
    ;; Padding is already 0 from initial-element
    ;; Sentinel
    (setf (cffi:mem-aref buf :uint32 (1- n-total)) +eop-sentinel+)
    ;; Diagnostic
    (format t "  [tx-build] ~D dwords (body=~D bytes), msg=0x~4,'0X (~A)"
            n-total msg-length-bytes msg-type (msg-type-name msg-type))
    (when body-dwords
      (format t ", body:")
      (dolist (dw body-dwords)
        (format t " 0x~8,'0X" dw)))
    (format t "~%")
    (values buf n-total)))

;;; ===================================================================
;;; TX: Convenience builders for specific message types
;;; ===================================================================

(defun build-config-request (msg-type &key (handle 0) (phy-id 0))
  "Build a configuration (P5) request — body is empty."
  (build-fapi-request msg-type :body-dwords nil
                               :handle handle :phy-id phy-id))

(defun build-slot-request (msg-type sfn slot &key (handle 0) (phy-id 0))
  "Build a slot-level request — body is [sfn(16) | slot(16)]."
  (let ((body-dw (logior (ash (logand sfn #xFFFF) 16)
                         (logand slot #xFFFF))))
    (build-fapi-request msg-type :body-dwords (list body-dw)
                                 :handle handle :phy-id phy-id)))

;;; ===================================================================
;;; TX: DL_TTI.request body builder (SCF-222 Table 3.4.2-1)
;;; ===================================================================
;;;
;;; Builds a full DL_TTI.request body with:
;;;   Body DW0: [SFN(16) | Slot(16)]
;;;   Body DW1: [nPDUs(16) | nDlTypes(8) | pad(8)]
;;;   Body DW2: numPDUsOfEachType (PoC: skipped, set to 0)
;;;   Body DW3: [numGroups(16) | pad(16)] (PoC: skipped, set to 0)
;;;   Per PDSCH PDU:
;;;     [pduType(16) | pduSize(16)]            — PDU header
;;;     [bwpSize(16) | bwpStart(16)]           — PDSCH body DW0
;;;     [scs(8) | cp(8) | pduIndex(16)]        — PDSCH body DW1
;;;     [rnti(16) | pad(16)]                   — PDSCH body DW2

(defstruct dl-tti-pdsch-pdu
  "Parameters for a single PDSCH PDU within DL_TTI.request."
  (bwp-size   52  :type (unsigned-byte 16))
  (bwp-start  0   :type (unsigned-byte 16))
  (scs        1   :type (unsigned-byte 8))     ; 0=15kHz 1=30kHz 2=60kHz 3=120kHz
  (cp         0   :type (unsigned-byte 8))     ; 0=normal 1=extended
  (pdu-index  0   :type (unsigned-byte 16))
  (rnti       #x1234 :type (unsigned-byte 16)))

(defun build-dl-tti-body (sfn slot pdsch-pdus)
  "Build the DL_TTI.request body dword list.
PDSCH-PDUS is a list of dl-tti-pdsch-pdu structs.
Returns a list of body dwords."
  (let* ((n-pdus (length pdsch-pdus))
         (body (list)))
    ;; Body DW0: [SFN(16) | Slot(16)]
    (push (logior (ash (logand sfn #xFFFF) 16)
                  (logand slot #xFFFF))
          body)
    ;; Body DW1: [nPDUs(16) | nDlTypes(8) | pad(8)]
    ;; nDlTypes = 1 (only PDSCH for PoC)
    (push (logior (ash (logand n-pdus #xFFFF) 16)
                  (ash 1 8)    ; nDlTypes = 1
                  0)           ; pad
          body)
    ;; Body DW2: numPDUsOfEachType (PoC: single entry = nPDUs for PDSCH)
    (push (logand n-pdus #xFFFF) body)
    ;; Body DW3: [numGroups(16) | pad(16)]  (PoC: 0 groups)
    (push 0 body)

    ;; Per PDU
    (dolist (pdu pdsch-pdus)
      ;; PDU header: [pduType(16) | pduSize(16)]
      ;; pduSize = 4 (header) + 12 (3 body dwords × 4 bytes) = 16
      (let ((pdu-size 16))
        (push (logior (ash +dl-pdu-pdsch+ 16)
                      (logand pdu-size #xFFFF))
              body))
      ;; PDSCH body DW0: [bwpSize(16) | bwpStart(16)]
      (push (logior (ash (logand (dl-tti-pdsch-pdu-bwp-size pdu) #xFFFF) 16)
                    (logand (dl-tti-pdsch-pdu-bwp-start pdu) #xFFFF))
            body)
      ;; PDSCH body DW1: [scs(8) | cp(8) | pduIndex(16)]
      (push (logior (ash (logand (dl-tti-pdsch-pdu-scs pdu) #xFF) 24)
                    (ash (logand (dl-tti-pdsch-pdu-cp pdu) #xFF) 16)
                    (logand (dl-tti-pdsch-pdu-pdu-index pdu) #xFFFF))
            body)
      ;; PDSCH body DW2: [rnti(16) | pad(16)]
      (push (ash (logand (dl-tti-pdsch-pdu-rnti pdu) #xFFFF) 16)
            body))

    (nreverse body)))

(defun send-dl-tti-request (dev sfn slot pdsch-pdus &key (handle 0) (phy-id 0))
  "Build and send a DL_TTI.request with full PDU structure.
PDSCH-PDUS is a list of dl-tti-pdsch-pdu structs."
  (let ((body-dwords (build-dl-tti-body sfn slot pdsch-pdus)))
    (format t "  [dl-tti] sfn=~D slot=~D nPDUs=~D body=~D dwords~%"
            sfn slot (length pdsch-pdus) (length body-dwords))
    (send-fapi-request dev +fapi-dl-tti-request+
                       :body-dwords body-dwords
                       :handle handle :phy-id phy-id)))

;;; ===================================================================
;;; TX: TX_DATA.request body builder (SCF-222 Table 3.4.6-1)
;;; ===================================================================
;;;
;;; Builds a full TX_DATA.request body with:
;;;   Body DW0: [SFN(16) | Slot(16)]
;;;   Body DW1: [controlLength(16) | nPDUs(16)]
;;;   Per PDU:
;;;     [pduLength(32)]
;;;     [pduIndex(16) | cwIndex(8) | pad(8)]
;;;     [numTLV(32)]
;;;     [tag(16) | pad(16)]
;;;     [length(32)]              — TB size in bytes
;;;     [value dwords...]         — inline TB payload

(defstruct tx-data-pdu
  "Parameters for a single PDU within TX_DATA.request."
  (pdu-index  0    :type (unsigned-byte 16))
  (cw-index   0    :type (unsigned-byte 8))
  (tlv-tag    0    :type (unsigned-byte 16))    ; 0=inline, 3=inline-aligned
  (tb-data    nil  :type list))                 ; list of 32-bit TB payload dwords

(defun build-tx-data-body (sfn slot tx-pdus)
  "Build the TX_DATA.request body dword list.
TX-PDUS is a list of tx-data-pdu structs.
Returns a list of body dwords."
  (let* ((n-pdus (length tx-pdus))
         (body (list)))
    ;; Body DW0: [SFN(16) | Slot(16)]
    (push (logior (ash (logand sfn #xFFFF) 16)
                  (logand slot #xFFFF))
          body)
    ;; Body DW1: [controlLength(16) | nPDUs(16)]
    ;; controlLength = 4 (just this header dword, PoC simplification)
    (push (logior (ash 4 16)
                  (logand n-pdus #xFFFF))
          body)

    ;; Per PDU
    (dolist (pdu tx-pdus)
      (let* ((tb-dwords (tx-data-pdu-tb-data pdu))
             (tb-len-bytes (* 4 (length tb-dwords)))
             ;; pduLength = pduIndex/cw(4) + numTLV(4) + tag(4) + length(4) + TB payload
             (pdu-body-bytes (+ 4 4 4 4 tb-len-bytes))
             ;; Total pduLength includes the pduLength field itself? No.
             ;; Per FPGA parser: pduLength(32) is consumed, then remainder = pduLength - 4 + body
             ;; Actually from the FPGA code, pduLength is the full length, and the parser does:
             ;;   pduRemainDw = (pduLenBytes + 3) / 4 - 1
             ;; So pduLength = total bytes of the PDU payload after the pduLength field
             ;; Let's use pduLength = pdu-body-bytes
             (pdu-length pdu-body-bytes))
        ;; [pduLength(32)]
        (push pdu-length body)
        ;; [pduIndex(16) | cwIndex(8) | pad(8)]
        (push (logior (ash (logand (tx-data-pdu-pdu-index pdu) #xFFFF) 16)
                      (ash (logand (tx-data-pdu-cw-index pdu) #xFF) 8)
                      0)
              body)
        ;; [numTLV(32)]  — 1 TLV per PDU in PoC
        (push 1 body)
        ;; [tag(16) | pad(16)]
        (push (ash (logand (tx-data-pdu-tlv-tag pdu) #xFFFF) 16) body)
        ;; [length(32)]  — TB size in bytes
        (push tb-len-bytes body)
        ;; [value dwords...]
        (dolist (dw tb-dwords)
          (push dw body))))

    (nreverse body)))

(defun send-tx-data-request (dev sfn slot tx-pdus &key (handle 0) (phy-id 0))
  "Build and send a TX_DATA.request with full TLV structure.
TX-PDUS is a list of tx-data-pdu structs."
  (let* ((body-dwords (build-tx-data-body sfn slot tx-pdus))
         (total-tb-dwords (reduce #'+ tx-pdus
                                  :key (lambda (p) (length (tx-data-pdu-tb-data p)))
                                  :initial-value 0)))
    (format t "  [tx-data] sfn=~D slot=~D nPDUs=~D tb-dwords=~D body=~D dwords~%"
            sfn slot (length tx-pdus) total-tb-dwords (length body-dwords))
    (send-fapi-request dev +fapi-tx-data-request+
                       :body-dwords body-dwords
                       :handle handle :phy-id phy-id)))

;;; ===================================================================
;;; TX: Sending (unified)
;;; ===================================================================

(defun zero-metadata (meta)
  (dotimes (i (cffi:foreign-type-size '(:struct cl-bladerf::bladerf_metadata)))
    (setf (cffi:mem-aref (cffi:inc-pointer meta 0) :uint8 i) 0)))

(defun send-fapi-packet (dev buf n-dwords)
  "Send a pre-built FAPI packet buffer of N-DWORDS dwords."
  (cffi:with-foreign-object (meta '(:struct cl-bladerf::bladerf_metadata))
    (zero-metadata meta)
    (setf (cffi:foreign-slot-value meta '(:struct cl-bladerf::bladerf_metadata)
                                    'cl-bladerf::flags)
          cl-bladerf::BLADERF_META_FLAG_TX_NOW)
    (let ((st (cl-bladerf::bladerf_sync_tx dev buf n-dwords meta 2000)))
      (cond
        ((>= st 0)
         (format t "  [tx] OK (~D dwords)~%" n-dwords)
         t)
        (t
         (format t "  [tx] FAIL status=~D~%" st)
         nil)))))

(defun send-fapi-request (dev msg-type &key (body-dwords nil)
                                            (handle 0) (phy-id 0))
  "Build and send a FAPI request with optional body dwords."
  (multiple-value-bind (buf n)
      (build-fapi-request msg-type :body-dwords body-dwords
                                   :handle handle :phy-id phy-id)
    (unwind-protect
         (send-fapi-packet dev buf n)
      (cffi:foreign-free buf))))

(defun send-config-request (dev msg-type)
  "Convenience: send a configuration (P5) request (empty body)."
  (send-fapi-request dev msg-type))

(defun send-slot-request (dev msg-type sfn slot)
  "Convenience: send a slot-level request with SFN/slot body."
  (let ((body-dw (logior (ash (logand sfn #xFFFF) 16)
                         (logand slot #xFFFF))))
    (send-fapi-request dev msg-type :body-dwords (list body-dw))))

;;; ===================================================================
;;; Unified FAPI Response Struct
;;; ===================================================================
;;;
;;; A single struct covers ALL FAPI responses.  Fields like sfn and
;;; slot are 0 for configuration messages (body length < 8 bytes).
;;; The parser derives everything from msg_length in the header.
;;;
;;; v11.0: Added n-pdus and n-pdsch fields for DL_TTI.response parsing.

(defstruct fapi-response
  (msg-type     #xFFFF :type (unsigned-byte 16))
  (msg-length   0      :type (unsigned-byte 16))  ; body length in bytes
  (err-code     #xFF   :type (unsigned-byte 8))
  (phy-state    #xFF   :type (unsigned-byte 8))
  (handle       0      :type (unsigned-byte 16))
  (phy-id       0      :type (unsigned-byte 8))
  (sfn          0      :type (unsigned-byte 16))
  (slot         0      :type (unsigned-byte 16))
  (n-pdus       0      :type (unsigned-byte 8))    ; DL_TTI.resp: nPDUs echoed
  (n-pdsch      0      :type (unsigned-byte 8))    ; DL_TTI.resp: nPdsch count
  (sentinel     0      :type (unsigned-byte 32))
  (body-dwords  nil    :type list)    ; raw body dwords for inspection
  (valid-p      nil    :type boolean))

;;; ===================================================================
;;; Unified Parsing — msg_length driven, with DL_TTI.resp awareness
;;; ===================================================================
;;;
;;; The parser reads dwords 0-1 (common header), extracts msg_length
;;; from dw1, computes body dword count N = ceil(msg_length/4), reads
;;; N body dwords, then reads padding and sentinel.
;;;
;;; Total expected dwords = 2 (header) + N (body) + 1 (padding) + 1 (sentinel)
;;;                       = N + 4
;;;
;;; For interpretation, the parser uses msg_length and msg_type:
;;;   DL_TTI.response (0x8A), msg_length >= 8:
;;;     body dw0 = [sfn(16) | slot(16)]
;;;     body dw1 = [nPDUs(8) | nPdsch(8) | errCode(8) | phyState(8)]
;;;   Other slot-level, msg_length >= 8:
;;;     body dw0 = [sfn(16) | slot(16)]
;;;     body dw1 = [errCode(8) | phyState(8) | reserved(16)]
;;;   msg_length >= 4:
;;;     body dw0 = [errCode(8) | phyState(8) | reserved(16)]
;;;   msg_length == 0:
;;;     header-only

(defun parse-fapi-response (rx-buffer actual-count)
  "Parse any FAPI response using msg_length from the header to determine
the body layout.  Returns a fapi-response struct."
  ;; Need at least 2 dwords for the header
  (when (< actual-count 2)
    (format t "  [rx-parse] short packet: ~D dwords (need >= 2 for header)~%"
            actual-count)
    (return-from parse-fapi-response
      (make-fapi-response :valid-p nil)))

  (let* ((dw0 (cffi:mem-aref rx-buffer :uint32 0))
         (dw1 (cffi:mem-aref rx-buffer :uint32 1))
         ;; Common header fields
         (handle     (ldb (byte 16 8)  dw0))
         (phy-id     (ldb (byte 8  0)  dw0))
         (msg-type   (ldb (byte 16 16) dw1))
         (msg-length (ldb (byte 16 0)  dw1))  ; body length in bytes
         ;; Compute body dword count
         (n-body     (body-len-to-dwords msg-length))
         ;; Expected total dwords: header(2) + body(N) + padding(1) + sentinel(1)
         (expected   (+ 2 n-body 1 1)))

    (when (< actual-count expected)
      (format t "  [rx-parse] short packet: ~D dwords (need ~D for msg_length=~D)~%"
              actual-count expected msg-length)
      (return-from parse-fapi-response
        (make-fapi-response :valid-p nil)))

    ;; Read body dwords
    (let* ((body-dws (loop for i from 0 below n-body
                           collect (cffi:mem-aref rx-buffer :uint32 (+ 2 i))))
           ;; Sentinel is the last dword
           (sentinel-idx (1- expected))
           (sentinel (cffi:mem-aref rx-buffer :uint32 sentinel-idx))
           ;; Interpret body based on msg_length and msg_type
           (sfn       0)
           (slot-val  0)
           (err-code  0)
           (phy-state 0)
           (n-pdus    0)
           (n-pdsch   0)
           (is-dl-tti-resp (= msg-type +fapi-dl-tti-response+)))

      ;; msg_length >= 8: slot-level or DL_TTI response
      (when (>= msg-length 8)
        (let ((bdw0 (nth 0 body-dws))
              (bdw1 (nth 1 body-dws)))
          (setf sfn       (ldb (byte 16 16) bdw0))
          (setf slot-val  (ldb (byte 16 0)  bdw0))
          (if is-dl-tti-resp
              ;; DL_TTI.response: [nPDUs(8) | nPdsch(8) | errCode(8) | phyState(8)]
              (progn
                (setf n-pdus    (ldb (byte 8 24) bdw1))
                (setf n-pdsch   (ldb (byte 8 16) bdw1))
                (setf err-code  (ldb (byte 8 8)  bdw1))
                (setf phy-state (ldb (byte 8 0)  bdw1)))
              ;; Standard slot response: [errCode(8) | phyState(8) | reserved(16)]
              (progn
                (setf err-code  (ldb (byte 8 24) bdw1))
                (setf phy-state (ldb (byte 8 16) bdw1))))))

      ;; msg_length >= 4 and < 8: config response
      (when (and (>= msg-length 4) (< msg-length 8))
        (let ((bdw0 (nth 0 body-dws)))
          (setf err-code  (ldb (byte 8 24) bdw0))
          (setf phy-state (ldb (byte 8 16) bdw0))))

      (make-fapi-response :msg-type    msg-type
                          :msg-length  msg-length
                          :err-code    err-code
                          :phy-state   phy-state
                          :handle      handle
                          :phy-id      phy-id
                          :sfn         sfn
                          :slot        slot-val
                          :n-pdus      n-pdus
                          :n-pdsch     n-pdsch
                          :sentinel    sentinel
                          :body-dwords body-dws
                          :valid-p     t))))

;;; ===================================================================
;;; Printing
;;; ===================================================================

(defun print-fapi-response (resp)
  "Print any FAPI response.  Includes sfn/slot when body has them.
For DL_TTI.response, also prints nPDUs and nPdsch."
  (let ((ml (fapi-response-msg-length resp))
        (mt (fapi-response-msg-type resp)))
    (format t "  msg=0x~4,'0X (~A)  len=~D  err=0x~2,'0X"
            mt (msg-type-name mt)
            ml (fapi-response-err-code resp))
    (when (>= ml 8)
      (format t "  sfn=~D  slot=~D"
              (fapi-response-sfn resp)
              (fapi-response-slot resp)))
    (when (= mt +fapi-dl-tti-response+)
      (format t "  nPDUs=~D  nPdsch=~D"
              (fapi-response-n-pdus resp)
              (fapi-response-n-pdsch resp)))
    (format t "  phy=0x~2,'0X (~A)  sentinel=0x~8,'0X~A~%"
            (fapi-response-phy-state resp) (phy-state-name (fapi-response-phy-state resp))
            (fapi-response-sentinel resp)
            (if (= (fapi-response-sentinel resp) +eop-sentinel+) "" " *** BAD SENTINEL ***"))))

;;; ===================================================================
;;; Unified RX: receive one FAPI response
;;; ===================================================================

(defconstant +rx-buf-dwords+ 1024)

(defun receive-fapi-response (dev &key (timeout-ms 500) (verbose nil))
  "Receive one FAPI response packet (variable length, auto-detected
from msg_length in the common header)."
  (let ((buf (cffi:foreign-alloc :uint32 :count +rx-buf-dwords+))
        (result nil))
    (unwind-protect
         (cffi:with-foreign-object (meta '(:struct cl-bladerf::bladerf_metadata))
           (zero-metadata meta)
           (setf (cffi:foreign-slot-value meta '(:struct cl-bladerf::bladerf_metadata)
                                          'cl-bladerf::flags)
                 cl-bladerf::BLADERF_META_FLAG_RX_NOW)
           (let ((st (cl-bladerf::bladerf_sync_rx dev buf +rx-buf-dwords+ meta timeout-ms)))
             (cond
               ((< st 0)
                (when verbose (format t "  [rx] bladerf_sync_rx error: ~D~%" st)))
               (t
                (let ((n (cffi:foreign-slot-value meta '(:struct cl-bladerf::bladerf_metadata)
                                                  'cl-bladerf::actual_count)))
                  (when verbose
                    (format t "  [rx] actual_count=~D" n)
                    (when (> n 0)
                      (format t ", raw:")
                      (dotimes (i (min n 12))
                        (format t " 0x~8,'0X" (cffi:mem-aref buf :uint32 i))))
                    (format t "~%"))
                  (when (> n 0)
                    (setf result (parse-fapi-response buf n))))))))
      (cffi:foreign-free buf))
    result))

;;; ===================================================================
;;; Unified await: wait for a specific response (bounded retries)
;;; ===================================================================

(defun await-fapi-response (dev expected-msg-type
                            &key (max-attempts 10) (timeout-ms 500) (verbose nil))
  "Wait for a FAPI response with the given msg_type_id.
Works for any FAPI response — the parser auto-detects the body
layout from msg_length."
  (dotimes (attempt max-attempts)
    (let ((resp (receive-fapi-response dev :timeout-ms timeout-ms :verbose verbose)))
      (cond
        ((null resp)
         (when verbose (format t "  [await ~D/~D] no packet, retrying...~%"
                               (1+ attempt) max-attempts))
         (sleep 0.01))
        ((= (fapi-response-msg-type resp) expected-msg-type)
         (return-from await-fapi-response resp))
        ((fapi-response-valid-p resp)
         (format t "  [await ~D/~D] UNEXPECTED: got 0x~4,'0X (~A), wanted 0x~4,'0X (~A)~%"
                 (1+ attempt) max-attempts
                 (fapi-response-msg-type resp) (msg-type-name (fapi-response-msg-type resp))
                 expected-msg-type (msg-type-name expected-msg-type))
         (return-from await-fapi-response resp))
        (t (sleep 0.005)))))
  (format t "  [await] exhausted ~D attempts for 0x~4,'0X (~A)~%"
          max-attempts expected-msg-type (msg-type-name expected-msg-type))
  nil)

;;; ===================================================================
;;; Unified send-and-verify: core test primitive
;;; ===================================================================
;;;
;;; A single function handles all message types.
;;; When :sfn and :slot are provided, it sends a slot-level request
;;; (body = sfn/slot dword) and checks sfn/slot in the response.
;;; Otherwise it sends a config request (empty body).
;;;
;;; Verification uses msg_length from the response to determine
;;; which fields to check.

(defun send-and-verify (dev msg-type expected-resp-type expected-err expected-phy
                        &key sfn slot (verbose nil))
  "Unified send-and-verify for all FAPI message types.
For configuration requests: call with just msg-type and expected fields.
For slot-level requests: also supply :sfn and :slot.
The response format is auto-detected from msg_length."
  (let ((has-slot-body (and sfn slot)))
    ;; --- TX ---
    (if has-slot-body
        (progn
          (format t "  TX: 0x~4,'0X (~A) sfn=~D slot=~D~%"
                  msg-type (msg-type-name msg-type) sfn slot)
          (unless (send-slot-request dev msg-type sfn slot)
            (format t "  *** TX FAILED ***~%")
            (return-from send-and-verify nil)))
        (progn
          (format t "  TX: 0x~4,'0X (~A)~%" msg-type (msg-type-name msg-type))
          (unless (send-config-request dev msg-type)
            (format t "  *** TX FAILED ***~%")
            (return-from send-and-verify nil))))
    (sleep 0.02)

    ;; --- RX ---
    (format t "  RX: expecting 0x~4,'0X (~A)...~%"
            expected-resp-type (msg-type-name expected-resp-type))
    (let ((resp (await-fapi-response dev expected-resp-type
                                     :max-attempts 10
                                     :timeout-ms 500
                                     :verbose verbose)))
      (cond
        ((null resp)
         (format t "  ===> FAIL: no response received~%")
         nil)
        (t
         (format t "  GOT: ") (print-fapi-response resp)
         (let* (;; Always check msg-type, err-code, phy-state, sentinel
                (pass (and (= (fapi-response-msg-type resp)  expected-resp-type)
                           (= (fapi-response-err-code resp)  expected-err)
                           (= (fapi-response-phy-state resp) expected-phy)
                           (= (fapi-response-sentinel resp)  +eop-sentinel+)))
                ;; Check sfn/slot if we have expectations AND the response
                ;; body is long enough to contain them (msg_length >= 8)
                (pass (if (and pass has-slot-body
                               (>= (fapi-response-msg-length resp) 8))
                          (and (= (fapi-response-sfn resp) sfn)
                               (= (fapi-response-slot resp) slot))
                          pass)))
           (if pass
               (format t "  ===> PASS~%")
               (if has-slot-body
                   (format t "  ===> FAIL: expected err=0x~2,'0X phy=0x~2,'0X(~A)~
 sfn=~D slot=~D sentinel=0x~8,'0X~%"
                           expected-err expected-phy (phy-state-name expected-phy)
                           sfn slot +eop-sentinel+)
                   (format t "  ===> FAIL: expected err=0x~2,'0X phy=0x~2,'0X(~A) sentinel=0x~8,'0X~%"
                           expected-err expected-phy (phy-state-name expected-phy) +eop-sentinel+)))
           pass))))))

;;; ===================================================================
;;; DL_TTI send-and-verify: extended test primitive
;;; ===================================================================
;;;
;;; Sends a full DL_TTI.request with PDU structure and verifies the
;;; DL_TTI.response (0x8A) including nPDUs/nPdsch echo fields.

(defun send-dl-tti-and-verify (dev sfn slot pdsch-pdus
                               &key (verbose nil))
  "Send DL_TTI.request with PDSCH PDU structure.  Verify DL_TTI.response
echoes SFN/slot and nPDUs/nPdsch counts."
  (let ((n-pdus (length pdsch-pdus)))
    ;; --- TX ---
    (format t "  TX: DL_TTI.req sfn=~D slot=~D nPdsch=~D~%" sfn slot n-pdus)
    (unless (send-dl-tti-request dev sfn slot pdsch-pdus)
      (format t "  *** TX FAILED ***~%")
      (return-from send-dl-tti-and-verify nil))
    (sleep 0.02)

    ;; --- RX ---
    (format t "  RX: expecting 0x~4,'0X (~A)...~%"
            +fapi-dl-tti-response+ (msg-type-name +fapi-dl-tti-response+))
    (let ((resp (await-fapi-response dev +fapi-dl-tti-response+
                                     :max-attempts 15
                                     :timeout-ms 500
                                     :verbose verbose)))
      (cond
        ((null resp)
         (format t "  ===> FAIL: no response received~%")
         nil)
        (t
         (format t "  GOT: ") (print-fapi-response resp)
         (let* ((pass (and (= (fapi-response-msg-type resp)  +fapi-dl-tti-response+)
                           (= (fapi-response-err-code resp)  +msg-ok+)
                           (= (fapi-response-phy-state resp) +phy-running+)
                           (= (fapi-response-sentinel resp)  +eop-sentinel+)
                           ;; Verify SFN/slot echo
                           (= (fapi-response-sfn resp) sfn)
                           (= (fapi-response-slot resp) slot)
                           ;; Verify PDU counts
                           (= (fapi-response-n-pdus resp) n-pdus)
                           (= (fapi-response-n-pdsch resp) n-pdus))))
           (if pass
               (format t "  ===> PASS~%")
               (format t "  ===> FAIL: expected sfn=~D slot=~D nPDUs=~D nPdsch=~D ~
err=0x~2,'0X phy=RUNNING sentinel=0x~8,'0X~%"
                       sfn slot n-pdus n-pdus +msg-ok+ +eop-sentinel+))
           pass))))))

;;; ===================================================================
;;; TX_DATA send-and-verify: extended test primitive
;;; ===================================================================
;;;
;;; Sends a full TX_DATA.request with TLV payload and verifies the
;;; SLOT.indication (0x82) response echoing SFN/slot.

(defun send-tx-data-and-verify (dev sfn slot tx-pdus
                                &key (verbose nil))
  "Send TX_DATA.request with TLV-structured TB payload.  Verify
SLOT.indication echoes SFN/slot."
  ;; --- TX ---
  (format t "  TX: TX_DATA.req sfn=~D slot=~D nPDUs=~D~%"
          sfn slot (length tx-pdus))
  (unless (send-tx-data-request dev sfn slot tx-pdus)
    (format t "  *** TX FAILED ***~%")
    (return-from send-tx-data-and-verify nil))
  (sleep 0.02)

  ;; --- RX ---
  (format t "  RX: expecting 0x~4,'0X (~A)...~%"
          +fapi-slot-indication+ (msg-type-name +fapi-slot-indication+))
  (let ((resp (await-fapi-response dev +fapi-slot-indication+
                                   :max-attempts 15
                                   :timeout-ms 500
                                   :verbose verbose)))
    (cond
      ((null resp)
       (format t "  ===> FAIL: no response received~%")
       nil)
      (t
       (format t "  GOT: ") (print-fapi-response resp)
       (let* ((pass (and (= (fapi-response-msg-type resp)  +fapi-slot-indication+)
                         (= (fapi-response-err-code resp)  +msg-ok+)
                         (= (fapi-response-phy-state resp) +phy-running+)
                         (= (fapi-response-sentinel resp)  +eop-sentinel+)
                         ;; Verify SFN/slot echo
                         (= (fapi-response-sfn resp) sfn)
                         (= (fapi-response-slot resp) slot))))
         (if pass
             (format t "  ===> PASS~%")
             (format t "  ===> FAIL: expected sfn=~D slot=~D err=0x~2,'0X ~
phy=RUNNING sentinel=0x~8,'0X~%"
                     sfn slot +msg-ok+ +eop-sentinel+))
         pass)))))

;;; ===================================================================
;;; Flush stale RX packets
;;; ===================================================================

(defun flush-rx (dev &key (max-reads 5) (timeout-ms 100))
  (let ((flushed 0))
    (dotimes (i max-reads)
      (let ((resp (receive-fapi-response dev :timeout-ms timeout-ms)))
        (if resp
            (progn (incf flushed)
                   (format t "  [flush] discarded stale: msg=0x~4,'0X~%"
                           (fapi-response-msg-type resp)))
            (return))))
    (when (> flushed 0)
      (format t "  [flush] discarded ~D stale packets~%" flushed))))

;;; ===================================================================
;;; Test Suite: Configuration (P5) State Machine
;;; ===================================================================

(defun test-fapi-config-state-machine ()
  (with-safe-bladerf (dev "")
    (unless (configure-bladerf-for-fapi-duplex dev)
      (format t "~%*** bladeRF configuration failed ***~%")
      (return-from test-fapi-config-state-machine nil))

    (flush-rx dev)

    (format t "~%~
+==============================================================+~%~
|  FAPI Configuration Message Tests v11.0                      |~%~
|  (SCF-222 unified common header, variable-length bodies)     |~%~
+==============================================================+~%")

    (let ((all-passed t)
          (test-num 0)
          (verbose nil))
      (flet ((run (name msg-type exp-resp exp-err exp-phy)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (unless (send-and-verify dev msg-type exp-resp exp-err exp-phy
                                        :verbose verbose)
                 (setf all-passed nil))))

        ;; --- PHY_IDLE ---
        (run "PARAM.req (IDLE -> IDLE)"
             +fapi-param-request+ +fapi-param-response+ +msg-ok+ +phy-idle+)

        (run "CONFIG.req (IDLE -> CONFIGURED)"
             +fapi-config-request+ +fapi-config-response+ +msg-ok+ +phy-configured+)

        ;; --- PHY_CONFIGURED ---
        (run "PARAM.req (CONFIGURED -> CONFIGURED)"
             +fapi-param-request+ +fapi-param-response+ +msg-ok+ +phy-configured+)

        (run "CONFIG.req re-config (CONFIGURED -> CONFIGURED)"
             +fapi-config-request+ +fapi-config-response+ +msg-ok+ +phy-configured+)

        (run "START.req (CONFIGURED -> RUNNING)"
             +fapi-start-request+ +fapi-error-indication+ +msg-ok+ +phy-running+)

        ;; --- PHY_RUNNING ---
        (run "PARAM.req (RUNNING -> ERROR)"
             +fapi-param-request+ +fapi-error-indication+ +msg-invalid-state+ +phy-running+)

        (run "CONFIG.req (RUNNING -> ERROR)"
             +fapi-config-request+ +fapi-error-indication+ +msg-invalid-state+ +phy-running+)

        (run "START.req (RUNNING -> ERROR)"
             +fapi-start-request+ +fapi-error-indication+ +msg-invalid-state+ +phy-running+)

        (run "STOP.req (RUNNING -> IDLE)"
             +fapi-stop-request+ +fapi-stop-indication+ +msg-ok+ +phy-idle+)

        ;; --- Back to PHY_IDLE ---
        (run "STOP.req (IDLE -> ERROR)"
             +fapi-stop-request+ +fapi-error-indication+ +msg-invalid-state+ +phy-idle+)

        (run "START.req (IDLE -> ERROR)"
             +fapi-start-request+ +fapi-error-indication+ +msg-invalid-state+ +phy-idle+))

      (format t "~%==============================================================~%")
      (format t "  ~D tests: ~:[SOME FAILED~;ALL PASSED~]~%" test-num all-passed)
      (format t "==============================================================~%")
      all-passed)))

;;; ===================================================================
;;; Test Suite: Slot-Level Messages (generic + DL_TTI + TX_DATA)
;;; ===================================================================

(defun test-fapi-slot-messages ()
  (with-safe-bladerf (dev "")
    (unless (configure-bladerf-for-fapi-duplex dev)
      (format t "~%*** bladeRF configuration failed ***~%")
      (return-from test-fapi-slot-messages nil))

    (flush-rx dev)

    (format t "~%~
+==============================================================+~%~
|  FAPI Slot-Level Message Tests v11.0                         |~%~
|  (SCF-222 unified header, DL_TTI/TX_DATA full body tests)    |~%~
+==============================================================+~%")

    (let ((all-passed t)
          (test-num 0)
          (verbose nil))

      (flet ((run-cfg (name msg-type exp-resp exp-err exp-phy)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (unless (send-and-verify dev msg-type exp-resp exp-err exp-phy
                                        :verbose verbose)
                 (setf all-passed nil)))

             (run-slot (name msg-type sfn slot exp-resp exp-err exp-sfn exp-slot)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (unless (send-and-verify dev msg-type exp-resp exp-err +phy-running+
                                        :sfn exp-sfn :slot exp-slot
                                        :verbose verbose)
                 (setf all-passed nil)))

             (run-dl-tti (name sfn slot pdsch-pdus)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (unless (send-dl-tti-and-verify dev sfn slot pdsch-pdus
                                               :verbose verbose)
                 (setf all-passed nil)))

             (run-tx-data (name sfn slot tx-pdus)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (unless (send-tx-data-and-verify dev sfn slot tx-pdus
                                                :verbose verbose)
                 (setf all-passed nil)))

             ;; Slot error tests: send slot-level request but expect ERROR.ind
             (run-slot-error (name msg-type sfn slot exp-err exp-phy)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (format t "  TX: 0x~4,'0X (~A) sfn=~D slot=~D~%"
                       msg-type (msg-type-name msg-type) sfn slot)
               (unless (send-slot-request dev msg-type sfn slot)
                 (format t "  *** TX FAILED ***~%")
                 (setf all-passed nil)
                 (return-from run-slot-error nil))
               (sleep 0.02)
               (format t "  RX: expecting 0x~4,'0X (~A)...~%"
                       +fapi-error-indication+ (msg-type-name +fapi-error-indication+))
               (let ((resp (await-fapi-response dev +fapi-error-indication+
                                                :max-attempts 10
                                                :timeout-ms 500
                                                :verbose verbose)))
                 (cond
                   ((null resp)
                    (format t "  ===> FAIL: no response received~%")
                    (setf all-passed nil))
                   (t
                    (format t "  GOT: ") (print-fapi-response resp)
                    (let ((pass (and (= (fapi-response-msg-type resp) +fapi-error-indication+)
                                     (= (fapi-response-err-code resp) exp-err)
                                     (= (fapi-response-phy-state resp) exp-phy)
                                     (= (fapi-response-sentinel resp) +eop-sentinel+))))
                      (if pass
                          (format t "  ===> PASS~%")
                          (progn
                            (format t "  ===> FAIL: expected err=0x~2,'0X phy=0x~2,'0X(~A) sentinel=0x~8,'0X~%"
                                    exp-err exp-phy (phy-state-name exp-phy) +eop-sentinel+)
                            (setf all-passed nil))))))))

             ;; DL_TTI error test: full body, non-RUNNING state → ERROR.ind
             (run-dl-tti-error (name sfn slot pdsch-pdus exp-err exp-phy)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (format t "  TX: DL_TTI.req sfn=~D slot=~D nPdsch=~D (expecting ERROR)~%"
                       sfn slot (length pdsch-pdus))
               (unless (send-dl-tti-request dev sfn slot pdsch-pdus)
                 (format t "  *** TX FAILED ***~%")
                 (setf all-passed nil)
                 (return-from run-dl-tti-error nil))
               (sleep 0.02)
               (format t "  RX: expecting 0x~4,'0X (~A)...~%"
                       +fapi-error-indication+ (msg-type-name +fapi-error-indication+))
               (let ((resp (await-fapi-response dev +fapi-error-indication+
                                                :max-attempts 10
                                                :timeout-ms 500
                                                :verbose verbose)))
                 (cond
                   ((null resp)
                    (format t "  ===> FAIL: no response received~%")
                    (setf all-passed nil))
                   (t
                    (format t "  GOT: ") (print-fapi-response resp)
                    (let ((pass (and (= (fapi-response-msg-type resp) +fapi-error-indication+)
                                     (= (fapi-response-err-code resp) exp-err)
                                     (= (fapi-response-phy-state resp) exp-phy)
                                     (= (fapi-response-sentinel resp) +eop-sentinel+))))
                      (if pass
                          (format t "  ===> PASS~%")
                          (progn
                            (format t "  ===> FAIL: expected err=0x~2,'0X phy=0x~2,'0X(~A) sentinel=0x~8,'0X~%"
                                    exp-err exp-phy (phy-state-name exp-phy) +eop-sentinel+)
                            (setf all-passed nil))))))))

             ;; TX_DATA error test: full body, non-RUNNING state → ERROR.ind
             (run-tx-data-error (name sfn slot tx-pdus exp-err exp-phy)
               (incf test-num)
               (format t "~%-- Test ~D: ~A --~%" test-num name)
               (format t "  TX: TX_DATA.req sfn=~D slot=~D nPDUs=~D (expecting ERROR)~%"
                       sfn slot (length tx-pdus))
               (unless (send-tx-data-request dev sfn slot tx-pdus)
                 (format t "  *** TX FAILED ***~%")
                 (setf all-passed nil)
                 (return-from run-tx-data-error nil))
               (sleep 0.02)
               (format t "  RX: expecting 0x~4,'0X (~A)...~%"
                       +fapi-error-indication+ (msg-type-name +fapi-error-indication+))
               (let ((resp (await-fapi-response dev +fapi-error-indication+
                                                :max-attempts 10
                                                :timeout-ms 500
                                                :verbose verbose)))
                 (cond
                   ((null resp)
                    (format t "  ===> FAIL: no response received~%")
                    (setf all-passed nil))
                   (t
                    (format t "  GOT: ") (print-fapi-response resp)
                    (let ((pass (and (= (fapi-response-msg-type resp) +fapi-error-indication+)
                                     (= (fapi-response-err-code resp) exp-err)
                                     (= (fapi-response-phy-state resp) exp-phy)
                                     (= (fapi-response-sentinel resp) +eop-sentinel+))))
                      (if pass
                          (format t "  ===> PASS~%")
                          (progn
                            (format t "  ===> FAIL: expected err=0x~2,'0X phy=0x~2,'0X(~A) sentinel=0x~8,'0X~%"
                                    exp-err exp-phy (phy-state-name exp-phy) +eop-sentinel+)
                            (setf all-passed nil)))))))))

        ;; ================================================================
        ;; Phase 1: Bring PHY to RUNNING
        ;; ================================================================
        (format t "~%--- Phase 1: Bring PHY to RUNNING ---~%")
        (run-cfg "CONFIG.req (IDLE -> CONFIGURED)"
                +fapi-config-request+ +fapi-config-response+ +msg-ok+ +phy-configured+)
        (run-cfg "START.req (CONFIGURED -> RUNNING)"
                +fapi-start-request+ +fapi-error-indication+ +msg-ok+ +phy-running+)

        ;; ================================================================
        ;; Phase 2: Generic slot-level messages (unchanged from v10)
        ;; ================================================================
        (format t "~%--- Phase 2: Generic slot-level messages in RUNNING ---~%")
        (run-slot "UL_TTI.req (sfn=100 slot=5)"
                +fapi-ul-tti-request+ 100 5
                +fapi-slot-indication+ +msg-ok+ 100 5)
        (run-slot "UL_DCI.req (sfn=1023 slot=19)"
                +fapi-ul-dci-request+ 1023 19
                +fapi-slot-indication+ +msg-ok+ 1023 19)
        (run-slot "UL_TTI.req (sfn=42 slot=0)"
                +fapi-ul-tti-request+ 42 0
                +fapi-slot-indication+ +msg-ok+ 42 0)

        ;; ================================================================
        ;; Phase 3: DL_TTI.request with full PDU body structure
        ;; ================================================================
        (format t "~%--- Phase 3: DL_TTI.req with full PDSCH PDU body ---~%")

        ;; 3a: Single PDSCH PDU, minimal
        (run-dl-tti "DL_TTI.req 1×PDSCH (sfn=0 slot=0)"
                    0 0
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 0
                           :rnti #x1234)))

        ;; 3b: Single PDSCH PDU, different SFN/slot and RNTI
        (run-dl-tti "DL_TTI.req 1×PDSCH (sfn=512 slot=7)"
                    512 7
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 106 :bwp-start 10
                           :scs 0 :cp 0 :pdu-index 1
                           :rnti #xABCD)))

        ;; 3c: Two PDSCH PDUs in one DL_TTI.req
        (run-dl-tti "DL_TTI.req 2×PDSCH (sfn=100 slot=3)"
                    100 3
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 0
                           :rnti #x1111)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 24 :bwp-start 52
                           :scs 1 :cp 0 :pdu-index 1
                           :rnti #x2222)))

        ;; 3d: Boundary SFN/slot values
        (run-dl-tti "DL_TTI.req 1×PDSCH (sfn=1023 slot=19)"
                    1023 19
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 273 :bwp-start 0
                           :scs 3 :cp 1 :pdu-index 5
                           :rnti #xFFFF)))

        ;; 3e: Zero PDUs (empty DL_TTI.req)
        (run-dl-tti "DL_TTI.req 0 PDUs (sfn=200 slot=10)"
                    200 10
                    (list))

        ;; 3f: Three PDSCH PDUs
        (run-dl-tti "DL_TTI.req 3×PDSCH (sfn=750 slot=15)"
                    750 15
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 0
                           :rnti #x0001)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 1
                           :rnti #x0002)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 2
                           :rnti #x0003)))

        ;; 3g: Four PDSCH PDUs (max per slot in FPGA)
        (run-dl-tti "DL_TTI.req 4×PDSCH (sfn=999 slot=0, max)"
                    999 0
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 0
                           :rnti #xA001)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 106 :bwp-start 0
                           :scs 0 :cp 0 :pdu-index 1
                           :rnti #xA002)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 24 :bwp-start 52
                           :scs 2 :cp 0 :pdu-index 2
                           :rnti #xA003)
                          (make-dl-tti-pdsch-pdu
                           :bwp-size 273 :bwp-start 0
                           :scs 3 :cp 1 :pdu-index 3
                           :rnti #xA004)))

        ;; ================================================================
        ;; Phase 4: TX_DATA.request with full TLV body structure
        ;; ================================================================
        (format t "~%--- Phase 4: TX_DATA.req with full TLV body ---~%")

        ;; 4a: Single PDU, small inline payload (tag=0)
        (run-tx-data "TX_DATA.req 1 PDU, 4-dword TB (sfn=0 slot=0)"
                     0 0
                     (list (make-tx-data-pdu
                            :pdu-index 0 :cw-index 0
                            :tlv-tag +tlv-tag-inline-payload+
                            :tb-data '(#xDEADBEEF #xCAFEBABE #x12345678 #x9ABCDEF0))))

        ;; 4b: Single PDU, different SFN/slot, aligned tag (tag=3)
        (run-tx-data "TX_DATA.req 1 PDU, tag=3 aligned (sfn=300 slot=5)"
                     300 5
                     (list (make-tx-data-pdu
                            :pdu-index 1 :cw-index 0
                            :tlv-tag +tlv-tag-inline-aligned+
                            :tb-data '(#xAAAAAAAA #xBBBBBBBB))))

        ;; 4c: Single PDU, minimal 1-dword TB payload
        (run-tx-data "TX_DATA.req 1 PDU, 1-dword TB (sfn=42 slot=0)"
                     42 0
                     (list (make-tx-data-pdu
                            :pdu-index 0 :cw-index 0
                            :tlv-tag +tlv-tag-inline-payload+
                            :tb-data '(#x00000001))))

        ;; 4d: Single PDU, 8-dword TB payload
        (run-tx-data "TX_DATA.req 1 PDU, 8-dword TB (sfn=600 slot=12)"
                     600 12
                     (list (make-tx-data-pdu
                            :pdu-index 2 :cw-index 0
                            :tlv-tag +tlv-tag-inline-payload+
                            :tb-data (loop for i from 1 to 8 collect
                                           (logior (ash i 24) (ash i 16) (ash i 8) i)))))

        ;; 4e: Boundary SFN/slot
        (run-tx-data "TX_DATA.req 1 PDU (sfn=1023 slot=19)"
                     1023 19
                     (list (make-tx-data-pdu
                            :pdu-index 0 :cw-index 0
                            :tlv-tag +tlv-tag-inline-payload+
                            :tb-data '(#xFFFFFFFF #xFFFFFFFF))))

        ;; ================================================================
        ;; Phase 5: Combined DL_TTI + TX_DATA sequence (realistic pair)
        ;; ================================================================
        (format t "~%--- Phase 5: DL_TTI + TX_DATA paired sequence ---~%")

        ;; A real gNB sends DL_TTI.req first (scheduling), then TX_DATA.req
        ;; (transport block) for the same SFN/slot.  Test this pairing.

        (let ((paired-sfn 400)
              (paired-slot 8)
              (paired-rnti #x5678)
              (paired-pdu-idx 0))

          (run-dl-tti (format nil "DL_TTI.req paired (sfn=~D slot=~D)" paired-sfn paired-slot)
                      paired-sfn paired-slot
                      (list (make-dl-tti-pdsch-pdu
                             :bwp-size 52 :bwp-start 0
                             :scs 1 :cp 0
                             :pdu-index paired-pdu-idx
                             :rnti paired-rnti)))

          (run-tx-data (format nil "TX_DATA.req paired (sfn=~D slot=~D)" paired-sfn paired-slot)
                       paired-sfn paired-slot
                       (list (make-tx-data-pdu
                              :pdu-index paired-pdu-idx :cw-index 0
                              :tlv-tag +tlv-tag-inline-payload+
                              :tb-data '(#x01020304 #x05060708
                                         #x090A0B0C #x0D0E0F10)))))

        ;; Second paired sequence with different parameters
        (let ((paired-sfn 800)
              (paired-slot 14)
              (paired-rnti #x9ABC))

          (run-dl-tti (format nil "DL_TTI.req paired 2×PDSCH (sfn=~D slot=~D)" paired-sfn paired-slot)
                      paired-sfn paired-slot
                      (list (make-dl-tti-pdsch-pdu
                             :bwp-size 106 :bwp-start 0
                             :scs 0 :cp 0 :pdu-index 0
                             :rnti paired-rnti)
                            (make-dl-tti-pdsch-pdu
                             :bwp-size 52 :bwp-start 106
                             :scs 0 :cp 0 :pdu-index 1
                             :rnti #xDEF0)))

          (run-tx-data (format nil "TX_DATA.req paired (sfn=~D slot=~D)" paired-sfn paired-slot)
                       paired-sfn paired-slot
                       (list (make-tx-data-pdu
                              :pdu-index 0 :cw-index 0
                              :tlv-tag +tlv-tag-inline-payload+
                              :tb-data '(#xF0F0F0F0 #x0F0F0F0F)))))

        ;; ================================================================
        ;; Phase 6: Rapid-fire DL_TTI sequence
        ;; ================================================================
        (format t "~%--- Phase 6: Rapid-fire DL_TTI sequence ---~%")
        (loop for slot-idx from 0 below 5
              for sfn = 500
              do (run-dl-tti (format nil "DL_TTI.req rapid-fire (sfn=~D slot=~D)" sfn slot-idx)
                             sfn slot-idx
                             (list (make-dl-tti-pdsch-pdu
                                    :bwp-size 52 :bwp-start 0
                                    :scs 1 :cp 0
                                    :pdu-index slot-idx
                                    :rnti (+ #x3000 slot-idx)))))

        ;; ================================================================
        ;; Phase 7: Rapid-fire TX_DATA sequence
        ;; ================================================================
        (format t "~%--- Phase 7: Rapid-fire TX_DATA sequence ---~%")
        (loop for slot-idx from 0 below 5
              for sfn = 600
              do (run-tx-data (format nil "TX_DATA.req rapid-fire (sfn=~D slot=~D)" sfn slot-idx)
                              sfn slot-idx
                              (list (make-tx-data-pdu
                                     :pdu-index slot-idx :cw-index 0
                                     :tlv-tag +tlv-tag-inline-payload+
                                     :tb-data (list (logior (ash sfn 16) slot-idx))))))

        ;; ================================================================
        ;; Phase 8: STOP, then DL_TTI/TX_DATA in non-RUNNING state
        ;; ================================================================
        (format t "~%--- Phase 8: DL_TTI/TX_DATA in non-RUNNING state ---~%")
        (run-cfg "STOP.req (RUNNING -> IDLE)"
                +fapi-stop-request+ +fapi-stop-indication+ +msg-ok+ +phy-idle+)

        (run-slot-error "DL_TTI.req (IDLE -> ERROR, simple body)"
                       +fapi-dl-tti-request+ 0 0
                       +msg-invalid-state+ +phy-idle+)

        (run-dl-tti-error "DL_TTI.req (IDLE -> ERROR, full body)"
                         0 0
                         (list (make-dl-tti-pdsch-pdu))
                         +msg-invalid-state+ +phy-idle+)

        (run-slot-error "TX_DATA.req (IDLE -> ERROR, simple body)"
                       +fapi-tx-data-request+ 0 0
                       +msg-invalid-state+ +phy-idle+)

        (run-tx-data-error "TX_DATA.req (IDLE -> ERROR, full body)"
                          0 0
                          (list (make-tx-data-pdu
                                 :tb-data '(#xDEAD)))
                          +msg-invalid-state+ +phy-idle+)

        (run-cfg "CONFIG.req (IDLE -> CONFIGURED)"
                +fapi-config-request+ +fapi-config-response+ +msg-ok+ +phy-configured+)

        (run-dl-tti-error "DL_TTI.req (CONFIGURED -> ERROR)"
                         50 3
                         (list (make-dl-tti-pdsch-pdu :pdu-index 0 :rnti #x1111))
                         +msg-invalid-state+ +phy-configured+)

        (run-tx-data-error "TX_DATA.req (CONFIGURED -> ERROR)"
                          200 7
                          (list (make-tx-data-pdu
                                 :pdu-index 0 :cw-index 0
                                 :tlv-tag 0 :tb-data '(#xBEEF)))
                          +msg-invalid-state+ +phy-configured+)

        ;; ================================================================
        ;; Phase 9: Return to RUNNING for cleanup verification
        ;; ================================================================
        (format t "~%--- Phase 9: Cleanup ---~%")
        (run-cfg "START.req (CONFIGURED -> RUNNING)"
                +fapi-start-request+ +fapi-error-indication+ +msg-ok+ +phy-running+)

        ;; Final DL_TTI + TX_DATA to confirm clean state
        (run-dl-tti "DL_TTI.req post-cleanup (sfn=0 slot=0)"
                    0 0
                    (list (make-dl-tti-pdsch-pdu
                           :bwp-size 52 :bwp-start 0
                           :scs 1 :cp 0 :pdu-index 0
                           :rnti #xFACE)))

        (run-tx-data "TX_DATA.req post-cleanup (sfn=0 slot=0)"
                     0 0
                     (list (make-tx-data-pdu
                            :pdu-index 0 :cw-index 0
                            :tlv-tag +tlv-tag-inline-payload+
                            :tb-data '(#xC0FFEE00 #xC0FFEE01))))

        (run-cfg "STOP.req (RUNNING -> IDLE, final)"
                +fapi-stop-request+ +fapi-stop-indication+ +msg-ok+ +phy-idle+))

      (format t "~%==============================================================~%")
      (format t "  ~D tests: ~:[SOME FAILED~;ALL PASSED~]~%" test-num all-passed)
      (format t "==============================================================~%")
      all-passed)))

;;; ===================================================================
;;; Entry point
;;; ===================================================================

(defun run-all-fapi-tests ()
  (format t "~%~
+==============================================================+~%~
|  5G NR FAPI Test Suite v11.0                                 |~%~
|  SCF-222 Unified Header / DL_TTI + TX_DATA Full Body Tests   |~%~
+==============================================================+~%")
  (let ((cfg-ok  (test-fapi-config-state-machine))
        (slot-ok (test-fapi-slot-messages)))
    (format t "~%~
+==============================================================+~%~
|  OVERALL RESULTS                                             |~%~
|  Config: ~:[FAIL~;PASS~]   Slot: ~:[FAIL~;PASS~]~34T|~%~
+==============================================================+~%"
            cfg-ok slot-ok)
    (and cfg-ok slot-ok)))
