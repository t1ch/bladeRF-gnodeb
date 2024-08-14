;;;; cl-bladerf-gnodeb.lisp
(in-package #:cl-bladerf-gnodeb)

(defvar *dword-bytes*
  (cffi:foreign-alloc :uint32 :count 4 ))

(cl-bladerf::with-bladerf-device (dev1 "")

  (setf (cffi:mem-aref *dword-bytes* :uint32 0 ) #b00000000000000000000000000001101) ; This is the number of Dwords in the transport block
  (setf (cffi:mem-aref *dword-bytes* :uint32 1)  #b00000000000000000000000000001101)
  (setf (cffi:mem-aref *dword-bytes* :uint32 2)  #b00000000000000000000000000001101)
  (setf (cffi:mem-aref *dword-bytes* :uint32 2)  #b00000000000000000000000000001101)

  (cl-bladerf::bladerf_sync_config dev1 :BLADERF_TX_X1  :BLADERF_FORMAT_PACKET_META  4096 4096 16 10000000)
  (cl-bladerf::bladerf_set_frequency dev1 (cl-bladerf::channel-tx 0) 918000000)
  (cl-bladerf::bladerf_set_sample_rate dev1 (cl-bladerf::channel-tx 0)  250000 (cffi:null-pointer))
  (cl-bladerf::bladerf_set_bandwidth dev1 (cl-bladerf::channel-tx 0) 1500000(cffi:null-pointer)  )
  (cl-bladerf::bladerf_enable_module dev1 (cl-bladerf::channel-tx 0) 1)
  (cl-bladerf::bladerf_sync_tx dev1 *dword-bytes*  3   (cffi:null-pointer)  1000))
