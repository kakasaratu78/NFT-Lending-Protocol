;; Credit Scoring & Risk Assessment System Contract

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SCORE (err u301))
(define-constant ERR-BORROWER-NOT-FOUND (err u302))
(define-constant ERR-INSUFFICIENT-HISTORY (err u303))
(define-constant ERR-SCORE-LOCKED (err u304))
(define-constant ERR-INVALID-WEIGHT (err u305))

;; Credit score constants
(define-constant MIN-CREDIT-SCORE u300)
(define-constant MAX-CREDIT-SCORE u850)
(define-constant DEFAULT-CREDIT-SCORE u600)
(define-constant SCORE-DECAY-RATE u2)
(define-constant MIN-TRANSACTIONS-FOR-SCORE u3)

;; Risk tier constants
(define-constant RISK-EXCELLENT u1)
(define-constant RISK-GOOD u2)
(define-constant RISK-FAIR u3)
(define-constant RISK-POOR u4)
(define-constant RISK-VERY-POOR u5)

;; Main credit profile data structure
(define-map credit-profiles
    { borrower: principal }
    {
        current-score: uint,
        score-history: (list 10 uint),
        last-update: uint,
        total-loans: uint,
        successful-repayments: uint,
        defaults: uint,
        average-loan-size: uint,
        longest-loan-duration: uint,
        consecutive-on-time-payments: uint,
        risk-tier: uint,
        score-locked-until: uint
    }
)

;; Behavioral scoring factors
(define-map behavioral-factors
    { borrower: principal }
    {
        repayment-consistency: uint,
        loan-to-value-preference: uint,
        early-repayment-frequency: uint,
        collateral-quality-score: uint,
        transaction-volume: uint,
        account-age: uint,
        diversification-score: uint
    }
)

;; Risk assessment weights - adjustable by admin
(define-map scoring-weights
    { factor: (string-ascii 30) }
    { weight: uint }
)

;; Real-time risk metrics
(define-map risk-metrics
    { borrower: principal }
    {
        current-exposure: uint,
        debt-to-income-estimate: uint,
        utilization-rate: uint,
        recent-activity-score: uint,
        market-sentiment-impact: uint
    }
)

;; Credit score appeal system
(define-map score-appeals
    { appeal-id: uint }
    {
        borrower: principal,
        current-score: uint,
        requested-score: uint,
        reason: (string-ascii 200),
        status: (string-ascii 20),
        submitted-block: uint,
        reviewer: (optional principal)
    }
)

;; System configuration
(define-data-var contract-owner principal tx-sender)
(define-data-var appeal-nonce uint u0)
(define-data-var score-calculation-fee uint u10)
(define-data-var score-lock-duration uint u1440) ;; ~24 hours in blocks

;; Initialize default scoring weights
(define-private (setup-default-weights)
    (begin
        (map-set scoring-weights { factor: "repayment-history" } { weight: u35 })
        (map-set scoring-weights { factor: "loan-amounts" } { weight: u20 })
        (map-set scoring-weights { factor: "loan-duration" } { weight: u15 })
        (map-set scoring-weights { factor: "defaults" } { weight: u25 })
        (map-set scoring-weights { factor: "consistency" } { weight: u5 })
        true
    )
)

;; Main credit score calculation function
(define-public (calculate-credit-score (borrower principal))
    (let
        (
            (profile (default-to 
                {
                    current-score: DEFAULT-CREDIT-SCORE,
                    score-history: (list),
                    last-update: u0,
                    total-loans: u0,
                    successful-repayments: u0,
                    defaults: u0,
                    average-loan-size: u0,
                    longest-loan-duration: u0,
                    consecutive-on-time-payments: u0,
                    risk-tier: RISK-FAIR,
                    score-locked-until: u0
                }
                (map-get? credit-profiles { borrower: borrower })))
            (behavioral (default-to
                {
                    repayment-consistency: u50,
                    loan-to-value-preference: u50,
                    early-repayment-frequency: u0,
                    collateral-quality-score: u50,
                    transaction-volume: u0,
                    account-age: u0,
                    diversification-score: u50
                }
                (map-get? behavioral-factors { borrower: borrower })))
            (current-block stacks-block-height)
        )
        
        ;; Check if score is locked
        (asserts! (< (get score-locked-until profile) current-block) ERR-SCORE-LOCKED)
        
        ;; Require minimum transaction history for accurate scoring
        (asserts! (>= (get total-loans profile) MIN-TRANSACTIONS-FOR-SCORE) ERR-INSUFFICIENT-HISTORY)
        
        (let
            (
                (repayment-score (calculate-repayment-score profile))
                (behavioral-score (calculate-behavioral-score behavioral))
                (risk-adjustment (calculate-risk-adjustment borrower))
                (time-decay (calculate-time-decay profile current-block))
                
                ;; Weighted final score calculation
                (raw-score (+ 
                    (/ (* repayment-score u50) u100)
                    (/ (* behavioral-score u30) u100)
                    (/ (* risk-adjustment u20) u100)
                ))
                
                ;; Apply time decay and bounds
                (adjusted-score (- raw-score time-decay))
                (final-score (if (< adjusted-score MIN-CREDIT-SCORE) 
                               MIN-CREDIT-SCORE 
                               (if (> adjusted-score MAX-CREDIT-SCORE) 
                                 MAX-CREDIT-SCORE 
                                 adjusted-score)))
                
                (new-risk-tier (determine-risk-tier final-score))
                (updated-history (unwrap-panic (as-max-len? 
                    (append (get score-history profile) final-score) u10)))
            )
            
            ;; Update credit profile with new score
            (map-set credit-profiles
                { borrower: borrower }
                (merge profile {
                    current-score: final-score,
                    score-history: updated-history,
                    last-update: current-block,
                    risk-tier: new-risk-tier,
                    score-locked-until: (+ current-block (var-get score-lock-duration))
                })
            )
            
            (ok final-score)
        )
    )
)

;; Calculate repayment history score component
(define-private (calculate-repayment-score (profile {current-score: uint, score-history: (list 10 uint),
                                                    last-update: uint, total-loans: uint, successful-repayments: uint,
                                                    defaults: uint, average-loan-size: uint, longest-loan-duration: uint,
                                                    consecutive-on-time-payments: uint, risk-tier: uint, score-locked-until: uint}))
    (let
        (
            (repayment-rate (if (> (get total-loans profile) u0)
                              (/ (* (get successful-repayments profile) u100) (get total-loans profile))
                              u0))
            (default-impact (if (> (get defaults profile) u0)
                             (- u100 (* (get defaults profile) u25))
                             u100))
            (consistency-bonus (* (get consecutive-on-time-payments profile) u5))
        )
        (+ (/ (* repayment-rate u70) u100)
           (/ (* default-impact u20) u100)
           (/ (* consistency-bonus u10) u100))
    )
)

;; Calculate behavioral score component
(define-private (calculate-behavioral-score (behavioral {repayment-consistency: uint, loan-to-value-preference: uint,
                                                        early-repayment-frequency: uint, collateral-quality-score: uint,
                                                        transaction-volume: uint, account-age: uint, diversification-score: uint}))
    (let
        (
            (consistency-score (get repayment-consistency behavioral))
            (ltv-score (get loan-to-value-preference behavioral))
            (early-payment-bonus (if (> (get early-repayment-frequency behavioral) u20) 
                                    u20 
                                    (get early-repayment-frequency behavioral)))
            (collateral-score (get collateral-quality-score behavioral))
            (age-bonus (if (> (/ (get account-age behavioral) u100) u15) 
                         u15 
                         (/ (get account-age behavioral) u100)))
        )
        (+ (/ (* consistency-score u30) u100)
           (/ (* ltv-score u25) u100)
           (/ (* early-payment-bonus u15) u100)
           (/ (* collateral-score u20) u100)
           (/ (* age-bonus u10) u100))
    )
)

;; Calculate risk adjustment based on current market conditions
(define-private (calculate-risk-adjustment (borrower principal))
    (let
        (
            (risk-data (default-to
                {
                    current-exposure: u0,
                    debt-to-income-estimate: u50,
                    utilization-rate: u0,
                    recent-activity-score: u50,
                    market-sentiment-impact: u50
                }
                (map-get? risk-metrics { borrower: borrower })))
        )
        (+ (/ (* (get debt-to-income-estimate risk-data) u40) u100)
           (/ (* (get recent-activity-score risk-data) u30) u100)
           (/ (* (get market-sentiment-impact risk-data) u30) u100))
    )
)

;; Calculate time-based score decay
(define-private (calculate-time-decay (profile {current-score: uint, score-history: (list 10 uint),
                                               last-update: uint, total-loans: uint, successful-repayments: uint,
                                               defaults: uint, average-loan-size: uint, longest-loan-duration: uint,
                                               consecutive-on-time-payments: uint, risk-tier: uint, score-locked-until: uint})
                                     (current-block uint))
    (let
        (
            (blocks-since-update (- current-block (get last-update profile)))
            (weeks-inactive (/ blocks-since-update u1008)) ;; ~1 week in blocks
        )
        (if (> (* weeks-inactive SCORE-DECAY-RATE) u50) 
          u50 
          (* weeks-inactive SCORE-DECAY-RATE))
    )
)

;; Determine risk tier based on credit score
(define-private (determine-risk-tier (score uint))
    (if (>= score u750)
        RISK-EXCELLENT
        (if (>= score u650)
            RISK-GOOD
            (if (>= score u550)
                RISK-FAIR
                (if (>= score u450)
                    RISK-POOR
                    RISK-VERY-POOR))))
)

;; Update borrower statistics after loan activity
(define-public (update-borrower-stats (borrower principal) (loan-amount uint) (duration uint) 
                                     (successful-repayment bool) (was-early bool))
    (let
        (
            (profile (default-to 
                {
                    current-score: DEFAULT-CREDIT-SCORE,
                    score-history: (list),
                    last-update: u0,
                    total-loans: u0,
                    successful-repayments: u0,
                    defaults: u0,
                    average-loan-size: u0,
                    longest-loan-duration: u0,
                    consecutive-on-time-payments: u0,
                    risk-tier: RISK-FAIR,
                    score-locked-until: u0
                }
                (map-get? credit-profiles { borrower: borrower })))
            (behavioral (default-to
                {
                    repayment-consistency: u50,
                    loan-to-value-preference: u50,
                    early-repayment-frequency: u0,
                    collateral-quality-score: u50,
                    transaction-volume: u0,
                    account-age: u0,
                    diversification-score: u50
                }
                (map-get? behavioral-factors { borrower: borrower })))
            (new-total-loans (+ (get total-loans profile) u1))
            (new-successful (if successful-repayment 
                              (+ (get successful-repayments profile) u1)
                              (get successful-repayments profile)))
            (new-defaults (if successful-repayment
                            (get defaults profile)
                            (+ (get defaults profile) u1)))
            (new-consecutive (if successful-repayment
                               (+ (get consecutive-on-time-payments profile) u1)
                               u0))
            (new-avg-loan (/ (+ (* (get average-loan-size profile) (get total-loans profile)) loan-amount) 
                           new-total-loans))
            (new-longest-duration (if (> duration (get longest-loan-duration profile)) 
                                     duration 
                                     (get longest-loan-duration profile)))
            (new-early-frequency (if was-early
                                   (+ (get early-repayment-frequency behavioral) u1)
                                   (get early-repayment-frequency behavioral)))
        )
        
        ;; Update credit profile
        (map-set credit-profiles
            { borrower: borrower }
            (merge profile {
                total-loans: new-total-loans,
                successful-repayments: new-successful,
                defaults: new-defaults,
                average-loan-size: new-avg-loan,
                longest-loan-duration: new-longest-duration,
                consecutive-on-time-payments: new-consecutive
            })
        )
        
        ;; Update behavioral factors
        (map-set behavioral-factors
            { borrower: borrower }
            (merge behavioral {
                early-repayment-frequency: new-early-frequency,
                transaction-volume: (+ (get transaction-volume behavioral) u1)
            })
        )
        
        (ok true)
    )
)

;; Submit credit score appeal
(define-public (submit-score-appeal (requested-score uint) (reason (string-ascii 200)))
    (let
        (
            (appeal-id (+ (var-get appeal-nonce) u1))
            (profile (unwrap! (map-get? credit-profiles { borrower: tx-sender }) ERR-BORROWER-NOT-FOUND))
        )
        (asserts! (<= requested-score MAX-CREDIT-SCORE) ERR-INVALID-SCORE)
        (asserts! (>= requested-score MIN-CREDIT-SCORE) ERR-INVALID-SCORE)
        
        (map-set score-appeals
            { appeal-id: appeal-id }
            {
                borrower: tx-sender,
                current-score: (get current-score profile),
                requested-score: requested-score,
                reason: reason,
                status: "PENDING",
                submitted-block: stacks-block-height,
                reviewer: none
            }
        )
        
        (var-set appeal-nonce appeal-id)
        (ok appeal-id)
    )
)

;; Get credit score and risk assessment
(define-read-only (get-credit-assessment (borrower principal))
    (let
        (
            (profile (default-to 
                {
                    current-score: DEFAULT-CREDIT-SCORE,
                    score-history: (list),
                    last-update: u0,
                    total-loans: u0,
                    successful-repayments: u0,
                    defaults: u0,
                    average-loan-size: u0,
                    longest-loan-duration: u0,
                    consecutive-on-time-payments: u0,
                    risk-tier: RISK-FAIR,
                    score-locked-until: u0
                }
                (map-get? credit-profiles { borrower: borrower })))
            (risk-data (map-get? risk-metrics { borrower: borrower }))
        )
        (ok {
            credit-score: (get current-score profile),
            risk-tier: (get risk-tier profile),
            total-loans: (get total-loans profile),
            repayment-rate: (if (> (get total-loans profile) u0)
                              (/ (* (get successful-repayments profile) u100) (get total-loans profile))
                              u0),
            consecutive-payments: (get consecutive-on-time-payments profile),
            last-update: (get last-update profile),
            risk-metrics: risk-data
        })
    )
)

;; Calculate recommended interest rate based on credit score
(define-read-only (get-risk-based-interest-rate (borrower principal))
    (let
        (
            (profile (default-to 
                {
                    current-score: DEFAULT-CREDIT-SCORE,
                    score-history: (list),
                    last-update: u0,
                    total-loans: u0,
                    successful-repayments: u0,
                    defaults: u0,
                    average-loan-size: u0,
                    longest-loan-duration: u0,
                    consecutive-on-time-payments: u0,
                    risk-tier: RISK-FAIR,
                    score-locked-until: u0
                }
                (map-get? credit-profiles { borrower: borrower })))
            (base-rate u8)
            (risk-tier (get risk-tier profile))
        )
        (ok (+ base-rate
            (if (is-eq risk-tier RISK-EXCELLENT) u0
                (if (is-eq risk-tier RISK-GOOD) u2
                    (if (is-eq risk-tier RISK-FAIR) u5
                        (if (is-eq risk-tier RISK-POOR) u8
                            u12))))
        ))
    )
)

;; Administrative functions
(define-public (update-scoring-weight (factor (string-ascii 30)) (weight uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= weight u100) ERR-INVALID-WEIGHT)
        (map-set scoring-weights { factor: factor } { weight: weight })
        (ok true)
    )
)

(define-public (set-score-lock-duration (duration uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set score-lock-duration duration)
        (ok true)
    )
)

;; Initialize contract with default weights
(define-public (initialize-system)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (setup-default-weights) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

;; Read-only helper functions
(define-read-only (get-borrower-profile (borrower principal))
    (map-get? credit-profiles { borrower: borrower })
)

(define-read-only (get-appeal-details (appeal-id uint))
    (map-get? score-appeals { appeal-id: appeal-id })
)

(define-read-only (get-scoring-weight (factor (string-ascii 30)))
    (map-get? scoring-weights { factor: factor })
)

