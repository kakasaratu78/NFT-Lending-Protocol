;; NFT Lending Protocol - Main Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-LOAN-EXISTS (err u102))
(define-constant ERR-NO-LOAN-FOUND (err u103))
(define-constant ERR-LOAN-NOT-EXPIRED (err u104))

;; Data Variables
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    nft-id: uint,
    loan-amount: uint,
    interest-rate: uint,
    start-block: uint,
    duration: uint,
    status: (string-ascii 20)
  }
)

(define-data-var loan-nonce uint u0)

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

;; Public functions
(define-public (create-loan (nft-id uint) (amount uint) (duration uint))
  (let
    (
      (loan-id (+ (var-get loan-nonce) u1))
      (interest-rate (calculate-interest-rate nft-id))
    )
    ;; (try! (nft-transfer? nft-id tx-sender (as-contract tx-sender)))
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        nft-id: nft-id,
        loan-amount: amount,
        interest-rate: interest-rate,
        start-block: stacks-block-height,
        duration: duration,
        status: "ACTIVE"
      }
    )
    (var-set loan-nonce loan-id)
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    (ok loan-id)
  )
)

(define-public (repay-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (total-amount (calculate-repayment-amount loan-id))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    ;; (try! (nft-transfer? (get nft-id loan) (as-contract tx-sender) tx-sender))
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "REPAID" })
    )
    (ok true)
  )
)

;; Internal functions
(define-private (calculate-interest-rate (nft-id uint))
  ;; For MVP, using a simple fixed rate of 10%
  u10
)

(define-private (calculate-repayment-amount (loan-id uint))
  (let
    (
      (loan (unwrap-panic (get-loan loan-id)))
      (interest-amount (/ (* (get loan-amount loan) (get interest-rate loan)) u100))
    )
    (+ (get loan-amount loan) interest-amount)
  )
)



;; Add to constants
(define-constant LIQUIDATION-THRESHOLD u120) ;; 120% of loan value

;; Add to public functions
(define-public (liquidate-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (current-block stacks-block-height)
      (loan-end-block (+ (get start-block loan) (get duration loan)))
    )
    (asserts! (> current-block loan-end-block) ERR-LOAN-NOT-EXPIRED)
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "LIQUIDATED" })
    )
    (ok true)
  )
)


;; Add to data variables
(define-map nft-collection-rates
  { collection-id: uint }
  { base-rate: uint }
)

;; Helper function to get collection ID from NFT ID
(define-private (get-collection-id (nft-id uint))
  ;; For MVP, returning a default collection ID of 1
  u1
)

;; Helper function to get market volatility
(define-private (get-market-volatility)
  ;; For MVP, returning a fixed volatility rate of 2%
  u2
)

;; New function
(define-read-only (get-dynamic-interest-rate (nft-id uint))
  (let
    (
      (collection-id (get-collection-id nft-id))
      (market-rate (default-to u10 (get base-rate (map-get? nft-collection-rates { collection-id: collection-id }))))
    )
    (+ market-rate (get-market-volatility))
  )
)



(define-public (extend-loan-duration (loan-id uint) (additional-blocks uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (new-duration (+ (get duration loan) additional-blocks))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set loans
      { loan-id: loan-id }
      (merge loan { duration: new-duration })
    )
    (ok true)
  )
)



(define-map partial-repayments
  { loan-id: uint }
  { amount-paid: uint }
)

(define-public (make-partial-repayment (loan-id uint) (amount uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (current-paid (default-to u0 (get amount-paid (map-get? partial-repayments { loan-id: loan-id }))))
    )
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set partial-repayments
      { loan-id: loan-id }
      { amount-paid: (+ current-paid amount) }
    )
    (ok true)
  )
)


(define-public (transfer-loan (loan-id uint) (new-borrower principal))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set loans
      { loan-id: loan-id }
      (merge loan { borrower: new-borrower })
    )
    (ok true)
  )
)
