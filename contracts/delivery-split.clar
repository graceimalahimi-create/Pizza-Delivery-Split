;; Core contract for group pizza ordering with delivery coordination

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ORDER_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_COMMITTED (err u102))
(define-constant ERR_MIN_NOT_MET (err u103))
(define-constant ERR_ORDER_FINALIZED (err u104))
(define-constant ERR_PAYMENT_FAILED (err u200))
(define-constant ERR_INSUFFICIENT_BALANCE (err u201))
(define-constant ERR_ALREADY_PAID (err u202))

(define-data-var order-nonce uint u0)

(define-map orders
  uint
  {
    organizer: principal,
    restaurant: (string-ascii 64),
    delivery-address: (string-ascii 128),
    min-amount: uint,
    total-committed: uint,
    deadline-block: uint,
    finalized: bool
  }
)

(define-map order-participants
  {order-id: uint, participant: principal}
  {
    amount-committed: uint,
    items: (string-ascii 256),
    paid: bool
  }
)

(define-map order-payments
  uint
  {
    total-collected: uint,
    restaurant-paid: bool
  }
)

(define-public (create-order (restaurant (string-ascii 64))
                           (delivery-address (string-ascii 128))
                           (min-amount uint)
                           (deadline-blocks uint))
  (let ((order-id (var-get order-nonce)))
    (map-set orders order-id {
      organizer: tx-sender,
      restaurant: restaurant,
      delivery-address: delivery-address,
      min-amount: min-amount,
      total-committed: u0,
      deadline-block: (+ stacks-block-height deadline-blocks),
      finalized: false
    })
    (var-set order-nonce (+ order-id u1))
    (ok order-id)))

(define-public (join-order (order-id uint) (amount uint) (items (string-ascii 256)))
  (let ((order (unwrap! (map-get? orders order-id) ERR_ORDER_NOT_FOUND)))
    (asserts! (not (get finalized order)) ERR_ORDER_FINALIZED)
    (asserts! (< stacks-block-height (get deadline-block order)) ERR_ORDER_FINALIZED)

    (map-set order-participants {order-id: order-id, participant: tx-sender} {
      amount-committed: amount,
      items: items,
      paid: false
    })

    (map-set orders order-id (merge order {
      total-committed: (+ (get total-committed order) amount)
    }))
    (ok true)))

(define-public (finalize-order (order-id uint))
  (let ((order (unwrap! (map-get? orders order-id) ERR_ORDER_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get organizer order)) ERR_UNAUTHORIZED)
    (asserts! (>= (get total-committed order) (get min-amount order)) ERR_MIN_NOT_MET)
    (asserts! (not (get finalized order)) ERR_ORDER_FINALIZED)

    (map-set orders order-id (merge order {finalized: true}))
    (ok true)))

(define-public (pay-share (order-id uint))
  (let ((order (unwrap! (map-get? orders order-id) ERR_ORDER_NOT_FOUND))
        (commitment (unwrap! (map-get? order-participants {order-id: order-id, participant: tx-sender}) ERR_ORDER_NOT_FOUND)))

    (asserts! (get finalized order) ERR_ORDER_FINALIZED)
    (asserts! (not (get paid commitment)) ERR_ALREADY_PAID)

    (let ((amount (get amount-committed commitment)))
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

      (map-set order-participants {order-id: order-id, participant: tx-sender}
               (merge commitment {paid: true}))

      (let ((current-payment (default-to {total-collected: u0, restaurant-paid: false}
                                       (map-get? order-payments order-id))))
        (map-set order-payments order-id {
          total-collected: (+ (get total-collected current-payment) amount),
          restaurant-paid: (get restaurant-paid current-payment)
        }))

      (ok true))))

(define-public (release-payment (order-id uint) (restaurant-address principal))
  (let ((order (unwrap! (map-get? orders order-id) ERR_ORDER_NOT_FOUND))
        (payment-info (unwrap! (map-get? order-payments order-id) ERR_ORDER_NOT_FOUND)))

    (asserts! (is-eq tx-sender (get organizer order)) ERR_UNAUTHORIZED)
    (asserts! (not (get restaurant-paid payment-info)) ERR_ALREADY_PAID)

    (let ((total-amount (get total-collected payment-info)))
      (try! (as-contract (stx-transfer? total-amount tx-sender restaurant-address)))

      (map-set order-payments order-id {
        total-collected: total-amount,
        restaurant-paid: true
      })
      (ok true))))

(define-read-only (get-order (order-id uint))
  (map-get? orders order-id))

(define-read-only (get-participant-commitment (order-id uint) (participant principal))
  (map-get? order-participants {order-id: order-id, participant: participant}))

(define-read-only (get-payment-status (order-id uint))
  (map-get? order-payments order-id))
