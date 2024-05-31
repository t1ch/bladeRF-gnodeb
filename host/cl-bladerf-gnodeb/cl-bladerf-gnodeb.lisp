;;;; cl-bladerf-gnodeb.lisp
(in-package #:cl-bladerf-gnodeb)
(cffi:defcstruct packet-control
  (core-id     :uint8)
  (flags       :uint8)
  (sop         :bool)
  (eop         :bool)
  (data        :uint32)
  (data-valid  :bool))

(defvar packets nil)
(defvar *pdu-array-1*
  (cffi:foreign-alloc '(:array packet-control 3)))

(defvar *pdu-array-2*
  (cffi:foreign-alloc '(:array packet-control 2)))
(cl-bladerf::with-bladerf-device (dev1 "")


;;;; set the fields of the PDUs
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0) '(:struct packet-control)  'core-id) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0 ) '(:struct packet-control) 'flags) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0 ) '(:struct packet-control) 'sop) #b1)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0 ) '(:struct packet-control) 'eop) #b0)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0 ) '(:struct packet-control) 'data) #b01010101010101010101010101010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 0 ) '(:struct packet-control) 'data-valid) #b1)

  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1) '(:struct packet-control)  'core-id) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1 ) '(:struct packet-control) 'flags) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1 ) '(:struct packet-control) 'sop) #b0)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1 ) '(:struct packet-control) 'eop) #b1)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1 ) '(:struct packet-control) 'data) #b01010101010101010101010101010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 1 ) '(:struct packet-control) 'data-valid) #b1)

  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2) '(:struct packet-control)  'core-id) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2 ) '(:struct packet-control) 'flags) #b01010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2 ) '(:struct packet-control) 'sop) #b1)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2 ) '(:struct packet-control) 'eop) #b0)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2 ) '(:struct packet-control) 'data) #b01010101010101010101010101010101)
  (setf (cffi:foreign-slot-value (cffi:mem-aref *pdu-array-1* 'packet-control 2 ) '(:struct packet-control) 'data-valid) #b1)

  (cl-bladerf::bladerf_sync_config dev1 :BLADERF_TX_X1  :BLADERF_FORMAT_PACKET_META  4096 4096 16 10000000)
  (cl-bladerf::bladerf_set_frequency dev1 (cl-bladerf::channel-tx 0) 918000000)
  (cl-bladerf::bladerf_set_sample_rate dev1 (cl-bladerf::channel-tx 0)  250000 (cffi:null-pointer))
  (cl-bladerf::bladerf_set_bandwidth dev1 (cl-bladerf::channel-tx 0) 1500000(cffi:null-pointer)  )
  (cl-bladerf::bladerf_enable_module dev1 (cl-bladerf::channel-tx 0) 1)

  (cl-bladerf::bladerf_sync_tx dev1 *pdu-array-1*  (* 4 (/ (cffi:foreign-type-size '(:struct packet-control)) 4)) (cffi:null-pointer)  0)

  )
