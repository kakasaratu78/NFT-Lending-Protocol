;; NFT Lending Protocol - Main Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-LOAN-EXISTS (err u102))
(define-constant ERR-NO-LOAN-FOUND (err u103))
(define-constant ERR-LOAN-NOT-EXPIRED (err u104))
(define-constant ERR-CONTRACT-PAUSED (err u105))

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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set loans
      { loan-id: loan-id }
      (merge loan { borrower: new-borrower })
    )
    (ok true)
  )
)


;; Add to data variables
(define-data-var contract-paused bool false)
(define-data-var contract-owner principal tx-sender)

(define-public (toggle-contract-pause)
  (begin
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-paused (not (var-get contract-paused))))
  )
)


;; Add to data variables
(define-map emergency-contacts 
  { user: principal }
  { backup: principal }
)

(define-public (set-emergency-contact (backup-address principal))
  (begin
    (map-set emergency-contacts
      { user: tx-sender }
      { backup: backup-address }
    )
    (ok true)
  )
)

(define-public (emergency-withdraw (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (emergency-contact (unwrap! (map-get? emergency-contacts { user: (get borrower loan) }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (is-eq tx-sender (get backup emergency-contact)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get loan-amount loan) (as-contract tx-sender) tx-sender))
    (ok true)
  )
)


(define-constant ERR-INVALID-REFINANCE (err u106))

(define-public (refinance-loan (loan-id uint) (new-duration uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
      (new-interest-rate (calculate-interest-rate (get nft-id loan)))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status loan) "ACTIVE") ERR-INVALID-REFINANCE)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan 
        { 
          interest-rate: new-interest-rate,
          duration: new-duration,
          start-block: stacks-block-height
        }
      )
    )
    (ok true)
  )
)



(define-map nft-valuations
  { collection-id: uint }
  { floor-price: uint }
)

(define-read-only (get-nft-valuation (nft-id uint))
  (let
    (
      (collection-id (get-collection-id nft-id))
      (floor-price (default-to u0 (get floor-price (map-get? nft-valuations { collection-id: collection-id }))))
    )
    (ok floor-price)
  )
)

(define-public (update-floor-price (collection-id uint) (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set nft-valuations
      { collection-id: collection-id }
      { floor-price: new-price }
    )
    (ok true)
  )
)



(define-public (set-base-rate (collection-id uint) (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set nft-collection-rates
      { collection-id: collection-id }
      { base-rate: new-rate }
    )
    (ok true)
  )
)


(define-map loan-bundles
  { bundle-id: uint }
  { loan-ids: (list 10 uint), owner: principal }
)

(define-data-var bundle-nonce uint u0)

(define-public (create-loan-bundle (loan-ids (list 10 uint)))
  (let
    (
      (bundle-id (+ (var-get bundle-nonce) u1))
    )
    (var-set bundle-nonce bundle-id)
    (map-set loan-bundles
      { bundle-id: bundle-id }
      { loan-ids: loan-ids, owner: tx-sender }
    )
    (ok bundle-id)
  )
)



(define-map insurance-pool
  { participant: principal }
  { amount: uint, active: bool }
)

(define-data-var total-insurance-pool uint u0)

(define-public (join-insurance-pool (amount uint))
  (let
    (
      (current-pool (default-to { amount: u0, active: false } 
        (map-get? insurance-pool { participant: tx-sender })))
    )
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set total-insurance-pool (+ (var-get total-insurance-pool) amount))
    (map-set insurance-pool
      { participant: tx-sender }
      { amount: (+ (get amount current-pool) amount), active: true }
    )
    (ok true)
  )
)


;; Add to data variables
(define-map auctions 
    { loan-id: uint }
    { 
        highest-bid: uint,
        highest-bidder: principal,
        end-block: uint,
        active: bool 
    }
)

(define-public (start-auction (loan-id uint) (duration uint) (starting-bid uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
        (end-block (+ stacks-block-height duration))
    )
        (map-set auctions
            { loan-id: loan-id }
            {
                highest-bid: starting-bid,
                highest-bidder: tx-sender,
                end-block: end-block,
                active: true
            }
        )
        (ok true)
    )
)


;; Add to data variables
(define-map user-points 
    { user: principal }
    { points: uint }
)

(define-public (award-points (user principal) (amount uint))
    (let (
        (current-points (default-to { points: u0 } (map-get? user-points { user: user })))
    )
        (map-set user-points
            { user: user }
            { points: (+ (get points current-points) amount) }
        )
        (ok true)
    )
)


(define-public (redeem-points (amount uint))
    (let (
        (current-points (default-to { points: u0 } (map-get? user-points { user: tx-sender })))
    )
        (asserts! (>= (get points current-points) amount) ERR-INVALID-AMOUNT)
        (map-set user-points
            { user: tx-sender }
            { points: (- (get points current-points) amount) }
        )
        (ok true)
    )
)


;; Add to data variables
(define-map referrals
    { referrer: principal }
    { referral-count: uint, total-rewards: uint }
)

(define-public (register-referral (referrer principal))
    (let (
        (current-stats (default-to { referral-count: u0, total-rewards: u0 } 
            (map-get? referrals { referrer: referrer })))
    )
        (map-set referrals
            { referrer: referrer }
            {
                referral-count: (+ (get referral-count current-stats) u1),
                total-rewards: (+ (get total-rewards current-stats) u100)
            }
        )
        (ok true)
    )
)


(define-public (get-referral-stats (referrer principal))
    (let (
        (stats (default-to { referral-count: u0, total-rewards: u0 } 
            (map-get? referrals { referrer: referrer })))
    )
        (ok stats)
    )
)

;; Add to data variables
(define-map loan-ratings
    { loan-id: uint }
    { risk-score: uint, rating: (string-ascii 2) }
)

(define-public (set-loan-rating (loan-id uint) (risk-score uint))
    (let (
        (rating (if (< risk-score u30) "A+"
                (if (< risk-score u50) "B+"
                (if (< risk-score u70) "C+" "D+"))))
    )
        (map-set loan-ratings
            { loan-id: loan-id }
            { 
                risk-score: risk-score,
                rating: rating
            }
        )
        (ok true)
    )
)


(define-public (get-loan-rating (loan-id uint))
    (let (
        (rating (unwrap! (map-get? loan-ratings { loan-id: loan-id }) ERR-NO-LOAN-FOUND))
    )
        (ok rating)
    )
)


(define-public (get-loan-risk-score (loan-id uint))
    (let (
        (rating (unwrap! (map-get? loan-ratings { loan-id: loan-id }) ERR-NO-LOAN-FOUND))
    )
        (ok (get risk-score rating))
    )
)


;; Add to data variables
(define-map staked-loans
    { staker: principal }
    { amount: uint, start-block: uint }
)

(define-public (stake-loan (loan-id uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
    )
        (map-set staked-loans
            { staker: tx-sender }
            {
                amount: (get loan-amount loan),
                start-block: stacks-block-height
            }
        )
        (ok true)
    )
)


;; Add to data variables
(define-map vip-status
    { borrower: principal }
    { 
        tier: uint,
        discount-rate: uint,
        expiry: uint 
    }
)

(define-public (grant-vip-status (borrower principal) (tier uint))
    (let (
        (discount-rate (if (is-eq tier u1) u5
                        (if (is-eq tier u2) u10 u15)))
    )
        (map-set vip-status
            { borrower: borrower }
            {
                tier: tier,
                discount-rate: discount-rate,
                expiry: (+ stacks-block-height u50000)
            }
        )
        (ok true)
    )
)


;; Add to data variables
(define-map flash-loans
    { loan-id: uint }
    { amount: uint, block: uint }
)

(define-public (execute-flash-loan (amount uint))
    (let (
        (loan-id (+ (var-get loan-nonce) u1))
    )
        (map-set flash-loans
            { loan-id: loan-id }
            {
                amount: amount,
                block: stacks-block-height
            }
        )
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (var-set loan-nonce loan-id)
        (ok loan-id)
    )
)



;; Add to data variables
(define-map collateral-swaps
    { loan-id: uint }
    { 
        original-nft: uint,
        new-nft: uint,
        swap-block: uint 
    }
)

(define-public (swap-collateral (loan-id uint) (new-nft-id uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR-NO-LOAN-FOUND))
    )
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set collateral-swaps
            { loan-id: loan-id }
            {
                original-nft: (get nft-id loan),
                new-nft: new-nft-id,
                swap-block: stacks-block-height
            }
        )
        (ok true)
    )
)
