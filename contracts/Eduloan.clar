(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_LOAN_FULLY_PAID (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_LOAN_NOT_ACTIVE (err u106))

(define-data-var loan-counter uint u0)

(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    lender: principal,
    principal-amount: uint,
    interest-rate: uint,
    remaining-balance: uint,
    monthly-payment: uint,
    start-block: uint,
    term-months: uint,
    payments-made: uint,
    is-active: bool,
    is-defaulted: bool
  }
)

(define-map borrower-loans
  { borrower: principal }
  { loan-ids: (list 50 uint) }
)

(define-map lender-loans
  { lender: principal }
  { loan-ids: (list 50 uint) }
)

(define-map payment-history
  { loan-id: uint, payment-number: uint }
  {
    amount: uint,
    payment-block: uint,
    remaining-balance: uint
  }
)

(define-public (create-loan 
  (borrower principal)
  (principal-amount uint)
  (interest-rate uint)
  (term-months uint)
  (monthly-payment uint))
  (let
    (
      (loan-id (+ (var-get loan-counter) u1))
      (lender tx-sender)
    )
    (asserts! (> principal-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> monthly-payment u0) ERR_INVALID_AMOUNT)
    (asserts! (> term-months u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? loans { loan-id: loan-id })) ERR_LOAN_ALREADY_EXISTS)
    
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: borrower,
        lender: lender,
        principal-amount: principal-amount,
        interest-rate: interest-rate,
        remaining-balance: principal-amount,
        monthly-payment: monthly-payment,
        start-block: stacks-block-height,
        term-months: term-months,
        payments-made: u0,
        is-active: true,
        is-defaulted: false
      }
    )
    
    (update-borrower-loans borrower loan-id)
    (update-lender-loans lender loan-id)
    (var-set loan-counter loan-id)
    (ok loan-id)
  )
)

(define-public (make-payment (loan-id uint) (payment-amount uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (borrower (get borrower loan-data))
      (remaining-balance (get remaining-balance loan-data))
      (payments-made (get payments-made loan-data))
      (new-balance (if (>= payment-amount remaining-balance) u0 (- remaining-balance payment-amount)))
      (new-payments-made (+ payments-made u1))
      (is-fully-paid (is-eq new-balance u0))
    )
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> remaining-balance u0) ERR_LOAN_FULLY_PAID)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        remaining-balance: new-balance,
        payments-made: new-payments-made,
        is-active: (not is-fully-paid)
      })
    )
    
    (map-set payment-history
      { loan-id: loan-id, payment-number: new-payments-made }
      {
        amount: payment-amount,
        payment-block: stacks-block-height,
        remaining-balance: new-balance
      }
    )
    
    (ok { payment-amount: payment-amount, new-balance: new-balance, fully-paid: is-fully-paid })
  )
)

(define-public (mark-loan-default (loan-id uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (lender (get lender loan-data))
    )
    (asserts! (is-eq tx-sender lender) ERR_UNAUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        is-defaulted: true,
        is-active: false
      })
    )
    (ok true)
  )
)

(define-public (update-loan-terms (loan-id uint) (new-monthly-payment uint) (new-interest-rate uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (lender (get lender loan-data))
    )
    (asserts! (is-eq tx-sender lender) ERR_UNAUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (> new-monthly-payment u0) ERR_INVALID_AMOUNT)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        monthly-payment: new-monthly-payment,
        interest-rate: new-interest-rate
      })
    )
    (ok true)
  )
)

(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-borrower-loans (borrower principal))
  (map-get? borrower-loans { borrower: borrower })
)

(define-read-only (get-lender-loans (lender principal))
  (map-get? lender-loans { lender: lender })
)

(define-read-only (get-payment-history (loan-id uint) (payment-number uint))
  (map-get? payment-history { loan-id: loan-id, payment-number: payment-number })
)

(define-read-only (get-loan-status (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data (ok {
      remaining-balance: (get remaining-balance loan-data),
      payments-made: (get payments-made loan-data),
      is-active: (get is-active loan-data),
      is-defaulted: (get is-defaulted loan-data),
      progress-percentage: (/ (* (- (get principal-amount loan-data) (get remaining-balance loan-data)) u100) (get principal-amount loan-data))
    })
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (calculate-total-interest (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data 
    (let
      (
        (principal (get principal-amount loan-data))
        (rate (get interest-rate loan-data))
        (term (get term-months loan-data))
      )
      (ok (/ (* (* principal rate) term) (* u100 u12)))
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-total-loans)
  (var-get loan-counter)
)

(define-read-only (is-payment-overdue (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
    (let
      (
        (start-block (get start-block loan-data))
        (payments-made (get payments-made loan-data))
        (expected-payments (/ (- stacks-block-height start-block) u144))
      )
      (ok (and (get is-active loan-data) (> expected-payments payments-made)))
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-private (update-borrower-loans (borrower principal) (loan-id uint))
  (let
    (
      (current-loans (default-to { loan-ids: (list) } (map-get? borrower-loans { borrower: borrower })))
      (updated-list (unwrap-panic (as-max-len? (append (get loan-ids current-loans) loan-id) u50)))
    )
    (map-set borrower-loans { borrower: borrower } { loan-ids: updated-list })
  )
)

(define-private (update-lender-loans (lender principal) (loan-id uint))
  (let
    (
      (current-loans (default-to { loan-ids: (list) } (map-get? lender-loans { lender: lender })))
      (updated-list (unwrap-panic (as-max-len? (append (get loan-ids current-loans) loan-id) u50)))
    )
    (map-set lender-loans { lender: lender } { loan-ids: updated-list })
  )
)