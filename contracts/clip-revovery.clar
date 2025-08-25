;; paper-clip-recovery.clar
;; Simple supply conservation system for office materials

(define-map collections
  { collector: principal }
  {
    clips: uint,
    last-collection: uint,
    total-collected: uint
  })

(define-map supply-inventory
  { item-type: (string-ascii 20) }
  {
    available: uint,
    reserved: uint,
    last-updated: uint
  })

(define-map redistribution-schedule
  { schedule-id: uint }
  {
    collector: principal,
    item-type: (string-ascii 20),
    quantity: uint,
    scheduled-block: uint,
    status: (string-ascii 10)
  })

(define-data-var next-schedule-id uint u1)

;; Collection tracking
(define-public (record-collection (clips uint))
  (let ((current-data (default-to
    { clips: u0, last-collection: u0, total-collected: u0 }
    (map-get? collections { collector: tx-sender }))))
    (map-set collections
      { collector: tx-sender }
      {
        clips: (+ (get clips current-data) clips),
        last-collection: stacks-block-height,
        total-collected: (+ (get total-collected current-data) clips)
      })
    (update-inventory "paper-clips" clips)
    (ok true)))

;; Inventory management
(define-private (update-inventory (item-type (string-ascii 20)) (quantity uint))
  (let ((current (default-to
    { available: u0, reserved: u0, last-updated: u0 }
    (map-get? supply-inventory { item-type: item-type }))))
    (map-set supply-inventory
      { item-type: item-type }
      {
        available: (+ (get available current) quantity),
        reserved: (get reserved current),
        last-updated: stacks-block-height
      })))

;; Schedule redistribution
(define-public (schedule-redistribution (item-type (string-ascii 20)) (quantity uint) (blocks-ahead uint))
  (let ((schedule-id (var-get next-schedule-id))
        (inventory (map-get? supply-inventory { item-type: item-type })))
    (asserts! (is-some inventory) (err u404))
    (asserts! (>= (get available (unwrap-panic inventory)) quantity) (err u400))

    ;; Reserve items
    (map-set supply-inventory
      { item-type: item-type }
      (merge (unwrap-panic inventory)
        {
          available: (- (get available (unwrap-panic inventory)) quantity),
          reserved: (+ (get reserved (unwrap-panic inventory)) quantity)
        }))

    ;; Create schedule
    (map-set redistribution-schedule
      { schedule-id: schedule-id }
      {
        collector: tx-sender,
        item-type: item-type,
        quantity: quantity,
        scheduled-block: (+ stacks-block-height blocks-ahead),
        status: "pending"
      })

    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)))

;; Execute redistribution
(define-public (execute-redistribution (schedule-id uint))
  (let ((schedule (map-get? redistribution-schedule { schedule-id: schedule-id })))
    (asserts! (is-some schedule) (err u404))
    (asserts! (>= stacks-block-height (get scheduled-block (unwrap-panic schedule))) (err u425))
    (asserts! (is-eq (get status (unwrap-panic schedule)) "pending") (err u409))

    (map-set redistribution-schedule
      { schedule-id: schedule-id }
      (merge (unwrap-panic schedule) { status: "completed" }))

    (ok true)))

;; Read-only functions
(define-read-only (get-collection (collector principal))
  (map-get? collections { collector: collector }))

(define-read-only (get-inventory (item-type (string-ascii 20)))
  (map-get? supply-inventory { item-type: item-type }))

(define-read-only (get-schedule (schedule-id uint))
  (map-get? redistribution-schedule { schedule-id: schedule-id }))
