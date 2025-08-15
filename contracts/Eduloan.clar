(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_LOAN_FULLY_PAID (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_LOAN_NOT_ACTIVE (err u106))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u107))
(define-constant ERR_COLLATERAL_NOT_FOUND (err u108))
(define-constant ERR_LIQUIDATION_THRESHOLD_NOT_MET (err u109))
(define-constant ERR_COLLATERAL_ALREADY_EXISTS (err u110))
(define-constant ERR_INSUFFICIENT_BALANCE (err u111))
(define-constant ERR_INVALID_COLLATERAL_RATIO (err u112))
(define-constant ERR_LOAN_ALREADY_COLLATERALIZED (err u113))
(define-constant LIQUIDATION_THRESHOLD u75)
(define-constant COLLATERAL_RATIO_MINIMUM u125)
(define-constant ERR_SCHOLARSHIP_NOT_FOUND (err u114))
(define-constant ERR_APPLICATION_NOT_FOUND (err u115))
(define-constant ERR_SCHOLARSHIP_DEPLETED (err u116))
(define-constant ERR_ALREADY_APPLIED (err u117))
(define-constant ERR_INSUFFICIENT_GPA (err u118))
(define-constant ERR_APPLICATION_EXPIRED (err u119))
(define-constant ERR_SCHOLARSHIP_INACTIVE (err u120))
(define-constant MIN_GPA_REQUIREMENT u300)

(define-data-var loan-counter uint u0)
(define-data-var collateral-counter uint u0)
(define-data-var scholarship-counter uint u0)
(define-data-var application-counter uint u0)

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
    is-defaulted: bool,
    is-collateralized: bool,
    collateral-amount: uint,
    collateral-value-usd: uint
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

(define-map collateral-positions
  { loan-id: uint }
  {
    collateral-id: uint,
    stx-amount: uint,
    stx-price-usd: uint,
    total-value-usd: uint,
    liquidation-price: uint,
    deposit-block: uint,
    last-update-block: uint,
    is-active: bool
  }
)

(define-map collateral-history
  { collateral-id: uint, action-number: uint }
  {
    action-type: (string-ascii 20),
    amount: uint,
    price-usd: uint,
    block-height: uint,
    loan-id: uint
  }
)

;; Scholarship fund system maps
(define-map scholarship-funds
  { scholarship-id: uint }
  {
    donor: principal,
    title: (string-ascii 100),
    description: (string-ascii 200),
    total-amount: uint,
    remaining-amount: uint,
    min-gpa-requirement: uint,
    max-disbursement: uint,
    application-deadline: uint,
    creation-block: uint,
    is-active: bool,
    recipients-count: uint
  }
)

(define-map scholarship-applications
  { application-id: uint }
  {
    scholarship-id: uint,
    student: principal,
    current-gpa: uint,
    requested-amount: uint,
    academic-statement: (string-ascii 300),
    application-block: uint,
    status: (string-ascii 20),
    disbursed-amount: uint
  }
)

(define-map student-scholarships
  { student: principal }
  { application-ids: (list 20 uint) }
)

(define-map donor-scholarships
  { donor: principal }
  { scholarship-ids: (list 10 uint) }
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
        is-defaulted: false,
        is-collateralized: false,
        collateral-amount: u0,
        collateral-value-usd: u0
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

(define-public (deposit-collateral (loan-id uint) (stx-amount uint) (stx-price-usd uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (borrower (get borrower loan-data))
      (collateral-id (+ (var-get collateral-counter) u1))
      (total-value-usd (* stx-amount stx-price-usd))
      (loan-amount-usd (get principal-amount loan-data))
      (collateral-ratio (/ (* total-value-usd u100) loan-amount-usd))
      (liquidation-price (/ (* loan-amount-usd LIQUIDATION_THRESHOLD) (* stx-amount u100)))
    )
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (not (get is-collateralized loan-data)) ERR_LOAN_ALREADY_COLLATERALIZED)
    (asserts! (> stx-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> stx-price-usd u0) ERR_INVALID_AMOUNT)
    (asserts! (>= collateral-ratio COLLATERAL_RATIO_MINIMUM) ERR_INSUFFICIENT_COLLATERAL)
    
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        is-collateralized: true,
        collateral-amount: stx-amount,
        collateral-value-usd: total-value-usd
      })
    )
    
    (map-set collateral-positions
      { loan-id: loan-id }
      {
        collateral-id: collateral-id,
        stx-amount: stx-amount,
        stx-price-usd: stx-price-usd,
        total-value-usd: total-value-usd,
        liquidation-price: liquidation-price,
        deposit-block: stacks-block-height,
        last-update-block: stacks-block-height,
        is-active: true
      }
    )
    
    (record-collateral-action collateral-id u1 "DEPOSIT" stx-amount stx-price-usd loan-id)
    (var-set collateral-counter collateral-id)
    (ok { collateral-id: collateral-id, collateral-ratio: collateral-ratio, liquidation-price: liquidation-price })
  )
)

(define-public (add-collateral (loan-id uint) (additional-stx uint) (current-stx-price uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (collateral-data (unwrap! (map-get? collateral-positions { loan-id: loan-id }) ERR_COLLATERAL_NOT_FOUND))
      (borrower (get borrower loan-data))
      (current-stx-amount (get stx-amount collateral-data))
      (new-stx-amount (+ current-stx-amount additional-stx))
      (new-total-value (* new-stx-amount current-stx-price))
      (loan-amount-usd (get principal-amount loan-data))
      (new-liquidation-price (/ (* loan-amount-usd LIQUIDATION_THRESHOLD) (* new-stx-amount u100)))
    )
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (get is-active collateral-data) ERR_COLLATERAL_NOT_FOUND)
    (asserts! (> additional-stx u0) ERR_INVALID_AMOUNT)
    (asserts! (> current-stx-price u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? additional-stx tx-sender (as-contract tx-sender)))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        collateral-amount: new-stx-amount,
        collateral-value-usd: new-total-value
      })
    )
    
    (map-set collateral-positions
      { loan-id: loan-id }
      (merge collateral-data {
        stx-amount: new-stx-amount,
        stx-price-usd: current-stx-price,
        total-value-usd: new-total-value,
        liquidation-price: new-liquidation-price,
        last-update-block: stacks-block-height
      })
    )
    
    (record-collateral-action (get collateral-id collateral-data) u2 "ADD" additional-stx current-stx-price loan-id)
    (ok { new-stx-amount: new-stx-amount, new-total-value: new-total-value, new-liquidation-price: new-liquidation-price })
  )
)

(define-public (withdraw-excess-collateral (loan-id uint) (withdrawal-amount uint) (current-stx-price uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (collateral-data (unwrap! (map-get? collateral-positions { loan-id: loan-id }) ERR_COLLATERAL_NOT_FOUND))
      (borrower (get borrower loan-data))
      (current-stx-amount (get stx-amount collateral-data))
      (remaining-stx (- current-stx-amount withdrawal-amount))
      (remaining-value (* remaining-stx current-stx-price))
      (loan-amount-usd (get principal-amount loan-data))
      (new-collateral-ratio (/ (* remaining-value u100) loan-amount-usd))
      (new-liquidation-price (/ (* loan-amount-usd LIQUIDATION_THRESHOLD) (* remaining-stx u100)))
    )
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (get is-active collateral-data) ERR_COLLATERAL_NOT_FOUND)
    (asserts! (> withdrawal-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> current-stx-price u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-stx-amount withdrawal-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= new-collateral-ratio COLLATERAL_RATIO_MINIMUM) ERR_INSUFFICIENT_COLLATERAL)
    
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender borrower)))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        collateral-amount: remaining-stx,
        collateral-value-usd: remaining-value
      })
    )
    
    (map-set collateral-positions
      { loan-id: loan-id }
      (merge collateral-data {
        stx-amount: remaining-stx,
        stx-price-usd: current-stx-price,
        total-value-usd: remaining-value,
        liquidation-price: new-liquidation-price,
        last-update-block: stacks-block-height
      })
    )
    
    (record-collateral-action (get collateral-id collateral-data) u3 "WITHDRAW" withdrawal-amount current-stx-price loan-id)
    (ok { remaining-stx: remaining-stx, remaining-value: remaining-value, new-collateral-ratio: new-collateral-ratio })
  )
)

(define-public (liquidate-collateral (loan-id uint) (current-stx-price uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (collateral-data (unwrap! (map-get? collateral-positions { loan-id: loan-id }) ERR_COLLATERAL_NOT_FOUND))
      (lender (get lender loan-data))
      (stx-amount (get stx-amount collateral-data))
      (liquidation-price (get liquidation-price collateral-data))
      (current-value (* stx-amount current-stx-price))
      (loan-amount-usd (get principal-amount loan-data))
      (collateral-ratio (/ (* current-value u100) loan-amount-usd))
    )
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_ACTIVE)
    (asserts! (get is-active collateral-data) ERR_COLLATERAL_NOT_FOUND)
    (asserts! (> current-stx-price u0) ERR_INVALID_AMOUNT)
    (asserts! (<= current-stx-price liquidation-price) ERR_LIQUIDATION_THRESHOLD_NOT_MET)
    
    (try! (as-contract (stx-transfer? stx-amount tx-sender lender)))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        is-active: false,
        is-defaulted: true,
        is-collateralized: false,
        collateral-amount: u0,
        collateral-value-usd: u0
      })
    )
    
    (map-set collateral-positions
      { loan-id: loan-id }
      (merge collateral-data {
        is-active: false,
        last-update-block: stacks-block-height
      })
    )
    
    (record-collateral-action (get collateral-id collateral-data) u4 "LIQUIDATE" stx-amount current-stx-price loan-id)
    (ok { liquidated-stx: stx-amount, liquidation-value: current-value, collateral-ratio: collateral-ratio })
  )
)

(define-public (release-collateral (loan-id uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (collateral-data (unwrap! (map-get? collateral-positions { loan-id: loan-id }) ERR_COLLATERAL_NOT_FOUND))
      (borrower (get borrower loan-data))
      (stx-amount (get stx-amount collateral-data))
      (remaining-balance (get remaining-balance loan-data))
    )
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (not (get is-active loan-data)) ERR_LOAN_NOT_ACTIVE)
    (asserts! (get is-active collateral-data) ERR_COLLATERAL_NOT_FOUND)
    (asserts! (is-eq remaining-balance u0) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (not (get is-defaulted loan-data)) ERR_LOAN_NOT_ACTIVE)
    
    (try! (as-contract (stx-transfer? stx-amount tx-sender borrower)))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        is-collateralized: false,
        collateral-amount: u0,
        collateral-value-usd: u0
      })
    )
    
    (map-set collateral-positions
      { loan-id: loan-id }
      (merge collateral-data {
        is-active: false,
        last-update-block: stacks-block-height
      })
    )
    
    (record-collateral-action (get collateral-id collateral-data) u5 "RELEASE" stx-amount u0 loan-id)
    (ok { released-stx: stx-amount })
  )
)

(define-read-only (get-collateral-position (loan-id uint))
  (map-get? collateral-positions { loan-id: loan-id })
)

(define-read-only (get-collateral-history (collateral-id uint) (action-number uint))
  (map-get? collateral-history { collateral-id: collateral-id, action-number: action-number })
)

(define-read-only (calculate-collateral-ratio (loan-id uint) (current-stx-price uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
    (match (map-get? collateral-positions { loan-id: loan-id })
      collateral-data
      (let
        (
          (stx-amount (get stx-amount collateral-data))
          (current-value (* stx-amount current-stx-price))
          (loan-amount-usd (get principal-amount loan-data))
        )
        (ok (/ (* current-value u100) loan-amount-usd))
      )
      ERR_COLLATERAL_NOT_FOUND
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (is-collateral-at-risk (loan-id uint) (current-stx-price uint))
  (match (map-get? collateral-positions { loan-id: loan-id })
    collateral-data
    (let
      (
        (liquidation-price (get liquidation-price collateral-data))
        (price-buffer (/ (* liquidation-price u10) u100))
      )
      (ok (<= current-stx-price (+ liquidation-price price-buffer)))
    )
    ERR_COLLATERAL_NOT_FOUND
  )
)

(define-read-only (get-liquidation-threshold-price (loan-id uint))
  (match (map-get? collateral-positions { loan-id: loan-id })
    collateral-data (ok (get liquidation-price collateral-data))
    ERR_COLLATERAL_NOT_FOUND
  )
)

(define-private (record-collateral-action (collateral-id uint) (action-number uint) (action-type (string-ascii 20)) (amount uint) (price-usd uint) (loan-id uint))
  (map-set collateral-history
    { collateral-id: collateral-id, action-number: action-number }
    {
      action-type: action-type,
      amount: amount,
      price-usd: price-usd,
      block-height: stacks-block-height,
      loan-id: loan-id
    }
  )
)

;; Scholarship Fund Functions

;; Create a new scholarship fund
(define-public (create-scholarship-fund 
  (title (string-ascii 100))
  (description (string-ascii 200))
  (total-amount uint)
  (min-gpa-requirement uint)
  (max-disbursement uint)
  (application-deadline uint))
  (let
    (
      (scholarship-id (+ (var-get scholarship-counter) u1))
      (donor tx-sender)
    )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> max-disbursement u0) ERR_INVALID_AMOUNT)
    (asserts! (<= max-disbursement total-amount) ERR_INVALID_AMOUNT)
    (asserts! (>= min-gpa-requirement MIN_GPA_REQUIREMENT) ERR_INSUFFICIENT_GPA)
    (asserts! (> application-deadline stacks-block-height) ERR_APPLICATION_EXPIRED)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Create scholarship fund
    (map-set scholarship-funds
      { scholarship-id: scholarship-id }
      {
        donor: donor,
        title: title,
        description: description,
        total-amount: total-amount,
        remaining-amount: total-amount,
        min-gpa-requirement: min-gpa-requirement,
        max-disbursement: max-disbursement,
        application-deadline: application-deadline,
        creation-block: stacks-block-height,
        is-active: true,
        recipients-count: u0
      }
    )
    
    ;; Update donor's scholarship list
    (update-donor-scholarships donor scholarship-id)
    (var-set scholarship-counter scholarship-id)
    (ok scholarship-id)
  )
)

;; Apply for a scholarship
(define-public (apply-for-scholarship 
  (scholarship-id uint)
  (current-gpa uint)
  (requested-amount uint)
  (academic-statement (string-ascii 300)))
  (let
    (
      (scholarship-data (unwrap! (map-get? scholarship-funds { scholarship-id: scholarship-id }) ERR_SCHOLARSHIP_NOT_FOUND))
      (application-id (+ (var-get application-counter) u1))
      (student tx-sender)
      (min-gpa (get min-gpa-requirement scholarship-data))
      (max-amount (get max-disbursement scholarship-data))
      (deadline (get application-deadline scholarship-data))
      (remaining-funds (get remaining-amount scholarship-data))
    )
    (asserts! (get is-active scholarship-data) ERR_SCHOLARSHIP_INACTIVE)
    (asserts! (>= current-gpa min-gpa) ERR_INSUFFICIENT_GPA)
    (asserts! (<= requested-amount max-amount) ERR_INVALID_AMOUNT)
    (asserts! (<= requested-amount remaining-funds) ERR_SCHOLARSHIP_DEPLETED)
    (asserts! (< stacks-block-height deadline) ERR_APPLICATION_EXPIRED)

    
    ;; Create application
    (map-set scholarship-applications
      { application-id: application-id }
      {
        scholarship-id: scholarship-id,
        student: student,
        current-gpa: current-gpa,
        requested-amount: requested-amount,
        academic-statement: academic-statement,
        application-block: stacks-block-height,
        status: "PENDING",
        disbursed-amount: u0
      }
    )
    
    ;; Update student's application list
    (update-student-applications student application-id)
    (var-set application-counter application-id)
    (ok application-id)
  )
)

;; Award scholarship to a student
(define-public (award-scholarship (application-id uint))
  (let
    (
      (application-data (unwrap! (map-get? scholarship-applications { application-id: application-id }) ERR_APPLICATION_NOT_FOUND))
      (scholarship-id (get scholarship-id application-data))
      (scholarship-data (unwrap! (map-get? scholarship-funds { scholarship-id: scholarship-id }) ERR_SCHOLARSHIP_NOT_FOUND))
      (student (get student application-data))
      (award-amount (get requested-amount application-data))
      (donor (get donor scholarship-data))
      (remaining-amount (get remaining-amount scholarship-data))
      (new-remaining (- remaining-amount award-amount))
      (new-recipients (+ (get recipients-count scholarship-data) u1))
    )
    (asserts! (is-eq tx-sender donor) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status application-data) "PENDING") ERR_APPLICATION_NOT_FOUND)
    (asserts! (>= remaining-amount award-amount) ERR_SCHOLARSHIP_DEPLETED)
    (asserts! (get is-active scholarship-data) ERR_SCHOLARSHIP_INACTIVE)
    
    ;; Transfer scholarship funds to student
    (try! (as-contract (stx-transfer? award-amount tx-sender student)))
    
    ;; Update application status
    (map-set scholarship-applications
      { application-id: application-id }
      (merge application-data {
        status: "AWARDED",
        disbursed-amount: award-amount
      })
    )
    
    ;; Update scholarship fund
    (map-set scholarship-funds
      { scholarship-id: scholarship-id }
      (merge scholarship-data {
        remaining-amount: new-remaining,
        recipients-count: new-recipients,
        is-active: (> new-remaining u0)
      })
    )
    
    (ok { awarded-amount: award-amount, recipient: student, remaining-funds: new-remaining })
  )
)

;; Withdraw remaining scholarship funds (donor only)
(define-public (withdraw-scholarship-funds (scholarship-id uint))
  (let
    (
      (scholarship-data (unwrap! (map-get? scholarship-funds { scholarship-id: scholarship-id }) ERR_SCHOLARSHIP_NOT_FOUND))
      (donor (get donor scholarship-data))
      (remaining-amount (get remaining-amount scholarship-data))
      (deadline (get application-deadline scholarship-data))
    )
    (asserts! (is-eq tx-sender donor) ERR_UNAUTHORIZED)
    (asserts! (> remaining-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> stacks-block-height deadline) ERR_APPLICATION_EXPIRED)
    
    ;; Transfer remaining funds back to donor
    (try! (as-contract (stx-transfer? remaining-amount tx-sender donor)))
    
    ;; Mark scholarship as inactive
    (map-set scholarship-funds
      { scholarship-id: scholarship-id }
      (merge scholarship-data {
        remaining-amount: u0,
        is-active: false
      })
    )
    
    (ok remaining-amount)
  )
)

;; Get scholarship fund details
(define-read-only (get-scholarship-fund (scholarship-id uint))
  (map-get? scholarship-funds { scholarship-id: scholarship-id })
)

;; Get scholarship application details
(define-read-only (get-scholarship-application (application-id uint))
  (map-get? scholarship-applications { application-id: application-id })
)

;; Get student's scholarship applications
(define-read-only (get-student-applications (student principal))
  (map-get? student-scholarships { student: student })
)

;; Get donor's scholarship funds
(define-read-only (get-donor-scholarships (donor principal))
  (map-get? donor-scholarships { donor: donor })
)

;; Get total scholarship funds count
(define-read-only (get-total-scholarships)
  (var-get scholarship-counter)
)

;; Check if student has already applied for a specific scholarship  
(define-read-only (has-applied-for-scholarship (student principal) (scholarship-id uint))
  false
)

;; Private helper functions
(define-private (update-student-applications (student principal) (application-id uint))
  (let
    (
      (current-apps (default-to { application-ids: (list) } (map-get? student-scholarships { student: student })))
      (updated-list (unwrap-panic (as-max-len? (append (get application-ids current-apps) application-id) u20)))
    )
    (map-set student-scholarships { student: student } { application-ids: updated-list })
  )
)

(define-private (update-donor-scholarships (donor principal) (scholarship-id uint))
  (let
    (
      (current-scholarships (default-to { scholarship-ids: (list) } (map-get? donor-scholarships { donor: donor })))
      (updated-list (unwrap-panic (as-max-len? (append (get scholarship-ids current-scholarships) scholarship-id) u10)))
    )
    (map-set donor-scholarships { donor: donor } { scholarship-ids: updated-list })
  )
)


