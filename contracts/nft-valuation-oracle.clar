;; NFT Valuation Oracle Contract
;; Provides decentralized price feed data for NFT collateral valuation

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-PRICE (err u401))
(define-constant ERR-STALE-PRICE (err u402))
(define-constant ERR-INSUFFICIENT-ORACLES (err u403))
(define-constant ERR-ORACLE-ALREADY-REGISTERED (err u404))
(define-constant ERR-ORACLE-NOT-FOUND (err u405))

;; Price freshness threshold (in blocks)
(define-constant PRICE-FRESHNESS-THRESHOLD u144) ;; ~24 hours in blocks
(define-constant MIN-ORACLES-REQUIRED u3)
(define-constant MAX-PRICE-DEVIATION-PERCENTAGE u20)

;; Data Maps
(define-map oracle-registry
    { oracle: principal }
    {
        name: (string-ascii 50),
        reliability-score: uint,
        last-update: uint,
        total-submissions: uint,
        status: (string-ascii 10)
    }
)

(define-map nft-price-feeds
    { collection-id: uint }
    {
        floor-price: uint,
        last-update: uint,
        total-submissions: uint,
        oracle-count: uint
    }
)

(define-map oracle-submissions
    { collection-id: uint, oracle: principal }
    {
        price: uint,
        timestamp: uint
    }
)

(define-map collection-metadata
    { collection-id: uint }
    {
        name: (string-ascii 50),
        contract-address: principal,
        volatility-score: uint,
        liquidity-score: uint
    }
)

;; Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-oracles uint u0)
(define-data-var min-submission-interval uint u72) ;; ~12 hours in blocks

;; Governance Functions
(define-public (register-oracle (name (string-ascii 50)))
    (let (
        (oracle-exists (is-some (map-get? oracle-registry { oracle: tx-sender })))
    )
        (asserts! (not oracle-exists) ERR-ORACLE-ALREADY-REGISTERED)
        
        (map-set oracle-registry
            { oracle: tx-sender }
            {
                name: name,
                reliability-score: u100,
                last-update: u0,
                total-submissions: u0,
                status: "ACTIVE"
            }
        )
        (var-set total-oracles (+ (var-get total-oracles) u1))
        (ok true)
    )
)

(define-public (update-oracle-status (oracle principal) (new-status (string-ascii 10)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? oracle-registry { oracle: oracle })) ERR-ORACLE-NOT-FOUND)
        
        (map-set oracle-registry
            { oracle: oracle }
            (merge (unwrap-panic (map-get? oracle-registry { oracle: oracle }))
                  { status: new-status })
        )
        (ok true)
    )
)

;; Price Feed Functions
(define-public (submit-price (collection-id uint) (price uint))
    (let (
        (oracle-info (unwrap! (map-get? oracle-registry { oracle: tx-sender }) ERR-ORACLE-NOT-FOUND))
        (current-feed (default-to 
            { floor-price: u0, last-update: u0, total-submissions: u0, oracle-count: u0 }
            (map-get? nft-price-feeds { collection-id: collection-id })))
        (blocks-since-last-update (- stacks-block-height (get last-update oracle-info)))
    )
        ;; Validation checks
        (asserts! (is-eq (get status oracle-info) "ACTIVE") ERR-NOT-AUTHORIZED)
        (asserts! (>= blocks-since-last-update (var-get min-submission-interval)) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        
        ;; Update oracle submission
        (map-set oracle-submissions
            { collection-id: collection-id, oracle: tx-sender }
            {
                price: price,
                timestamp: stacks-block-height
            }
        )
        
        ;; Update oracle stats
        (map-set oracle-registry
            { oracle: tx-sender }
            (merge oracle-info {
                last-update: stacks-block-height,
                total-submissions: (+ (get total-submissions oracle-info) u1)
            })
        )
        
        ;; Update price feed
        ;; (try! (update-price-feed collection-id))
        (ok true)
    )
)

(define-private (update-price-feed (collection-id uint))
    (let (
        (current-feed (default-to 
            { floor-price: u0, last-update: u0, total-submissions: u0, oracle-count: u0 }
            (map-get? nft-price-feeds { collection-id: collection-id })))
        (new-price (calculate-median-price collection-id))
    )
        (if (> new-price u0)
            (map-set nft-price-feeds
                { collection-id: collection-id }
                {
                    floor-price: new-price,
                    last-update: stacks-block-height,
                    total-submissions: (+ (get total-submissions current-feed) u1),
                    oracle-count: (var-get total-oracles)
                }
            )
            false
        )
        (ok true)
    )
)

;; Helper Functions
(define-private (calculate-median-price (collection-id uint))
    ;; For MVP, using a simple average instead of true median
    ;; This should be enhanced with a proper median calculation in production
    (let (
        (valid-submissions (get-valid-submissions collection-id))
        (submission-count (len valid-submissions))
    )
        (if (>= submission-count MIN-ORACLES-REQUIRED)
            (/ (fold + valid-submissions u0) submission-count)
            u0
        )
    )
)

(define-private (get-valid-submissions (collection-id uint))
    ;; This is a simplified version. In production, implement proper filtering
    ;; of outliers and stale prices
    (filter is-valid-submission 
        (map get-submission-price 
            (get-active-oracles)))
)

(define-private (is-valid-submission (price uint))
    (> price u0)
)

(define-private (get-submission-price (oracle principal))
    (default-to u0 
        (get price 
            (map-get? oracle-submissions 
                { collection-id: u1, oracle: oracle })))
)

(define-private (get-active-oracles)
    (filter is-active-oracle (get-all-oracles))
)

(define-private (is-active-oracle (oracle principal))
    (is-eq 
        (get status 
            (default-to { status: "INACTIVE" }
                (map-get? oracle-registry { oracle: oracle })))
        "ACTIVE")
)

(define-private (get-all-oracles)
    ;; In production, implement proper oracle enumeration
    ;; This is a simplified version
    (list tx-sender)
)

;; Read-only Functions
(define-read-only (get-nft-price (collection-id uint))
    (let (
        (price-feed (unwrap! (map-get? nft-price-feeds { collection-id: collection-id }) ERR-ORACLE-NOT-FOUND))
        (blocks-since-update (- stacks-block-height (get last-update price-feed)))
    )
        (asserts! (<= blocks-since-update PRICE-FRESHNESS-THRESHOLD) ERR-STALE-PRICE)
        (ok (get floor-price price-feed))
    )
)

(define-read-only (get-oracle-info (oracle principal))
    (map-get? oracle-registry { oracle: oracle })
)

(define-read-only (get-collection-info (collection-id uint))
    (map-get? collection-metadata { collection-id: collection-id })
)

;; Administrative Functions
(define-public (set-min-submission-interval (new-interval uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set min-submission-interval new-interval)
        (ok true)
    )
)

(define-public (update-collection-metadata (collection-id uint) 
                                         (name (string-ascii 50))
                                         (contract-address principal)
                                         (volatility-score uint)
                                         (liquidity-score uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set collection-metadata
            { collection-id: collection-id }
            {
                name: name,
                contract-address: contract-address,
                volatility-score: volatility-score,
                liquidity-score: liquidity-score
            }
        )
        (ok true)
    )
)

(define-public (update-oracle-reliability (oracle principal) (adjustment int))
    (let (
        (oracle-info (unwrap! (map-get? oracle-registry { oracle: oracle }) ERR-ORACLE-NOT-FOUND))
        (current-score (get reliability-score oracle-info))
        (new-score (+ u23))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= new-score u0) (<= new-score u100)) ERR-INVALID-PRICE)
        
        (map-set oracle-registry
            { oracle: oracle }
            (merge oracle-info { reliability-score: new-score })
        )
        (ok true)
    )
)
