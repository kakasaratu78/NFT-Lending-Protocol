;; NFT Achievement System Contract

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-UNLOCKED (err u202))
(define-constant ERR-CRITERIA-NOT-MET (err u203))

(define-map achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    category: (string-ascii 20),
    criteria-type: (string-ascii 20),
    criteria-value: uint,
    rarity: (string-ascii 10),
    token-uri: (string-ascii 200)
  }
)

(define-map user-achievements
  { user: principal, achievement-id: uint }
  { unlocked: bool, unlock-block: uint }
)

(define-map user-stats
  { user: principal }
  {
    loans-created: uint,
    loans-repaid: uint,
    total-borrowed: uint,
    total-repaid: uint,
    consecutive-repayments: uint,
    referrals-made: uint,
    points-earned: uint
  }
)

(define-map achievement-nfts
  { token-id: uint }
  {
    owner: principal,
    achievement-id: uint,
    metadata-uri: (string-ascii 200)
  }
)

(define-data-var achievement-nonce uint u0)
(define-data-var token-nonce uint u0)
(define-data-var contract-owner principal tx-sender)

(define-public (create-achievement (name (string-ascii 50)) (description (string-ascii 100)) 
                                  (category (string-ascii 20)) (criteria-type (string-ascii 20))
                                  (criteria-value uint) (rarity (string-ascii 10)) 
                                  (token-uri (string-ascii 200)))
  (let
    (
      (achievement-id (+ (var-get achievement-nonce) u1))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    (map-set achievements
      { achievement-id: achievement-id }
      {
        name: name,
        description: description,
        category: category,
        criteria-type: criteria-type,
        criteria-value: criteria-value,
        rarity: rarity,
        token-uri: token-uri
      }
    )
    
    (var-set achievement-nonce achievement-id)
    (ok achievement-id)
  )
)

(define-public (update-user-stats (user principal) (loans-created uint) (loans-repaid uint)
                                 (total-borrowed uint) (total-repaid uint) (consecutive-repayments uint)
                                 (referrals-made uint) (points-earned uint))
  (let
    (
      (current-stats (default-to { loans-created: u0, loans-repaid: u0, total-borrowed: u0,
                                   total-repaid: u0, consecutive-repayments: u0, referrals-made: u0,
                                   points-earned: u0 }
                      (map-get? user-stats { user: user })))
    )
    (map-set user-stats
      { user: user }
      {
        loans-created: (+ (get loans-created current-stats) loans-created),
        loans-repaid: (+ (get loans-repaid current-stats) loans-repaid),
        total-borrowed: (+ (get total-borrowed current-stats) total-borrowed),
        total-repaid: (+ (get total-repaid current-stats) total-repaid),
        consecutive-repayments: consecutive-repayments,
        referrals-made: (+ (get referrals-made current-stats) referrals-made),
        points-earned: (+ (get points-earned current-stats) points-earned)
      }
    )
    (ok true)
  )
)

(define-public (unlock-achievement (achievement-id uint))
  (let
    (
      (achievement (unwrap! (map-get? achievements { achievement-id: achievement-id }) ERR-ACHIEVEMENT-NOT-FOUND))
      (user-achievement (default-to { unlocked: false, unlock-block: u0 }
                        (map-get? user-achievements { user: tx-sender, achievement-id: achievement-id })))
      (user-stats-data (default-to { loans-created: u0, loans-repaid: u0, total-borrowed: u0,
                                     total-repaid: u0, consecutive-repayments: u0, referrals-made: u0,
                                     points-earned: u0 }
                       (map-get? user-stats { user: tx-sender })))
      (token-id (+ (var-get token-nonce) u1))
    )
    
    (asserts! (not (get unlocked user-achievement)) ERR-ALREADY-UNLOCKED)
    (asserts! (check-achievement-criteria achievement user-stats-data) ERR-CRITERIA-NOT-MET)
    
    (map-set user-achievements
      { user: tx-sender, achievement-id: achievement-id }
      { unlocked: true, unlock-block: stacks-block-height }
    )
    
    (map-set achievement-nfts
      { token-id: token-id }
      {
        owner: tx-sender,
        achievement-id: achievement-id,
        metadata-uri: (get token-uri achievement)
      }
    )
    
    (var-set token-nonce token-id)
    (ok token-id)
  )
)

(define-private (check-achievement-criteria (achievement {name: (string-ascii 50), description: (string-ascii 100),
                                                          category: (string-ascii 20), criteria-type: (string-ascii 20),
                                                          criteria-value: uint, rarity: (string-ascii 10),
                                                          token-uri: (string-ascii 200)})
                                            (stats {loans-created: uint, loans-repaid: uint, total-borrowed: uint,
                                                   total-repaid: uint, consecutive-repayments: uint, referrals-made: uint,
                                                   points-earned: uint}))
  (let
    (
      (criteria-type (get criteria-type achievement))
      (criteria-value (get criteria-value achievement))
    )
    (if (is-eq criteria-type "loans-created")
      (>= (get loans-created stats) criteria-value)
      (if (is-eq criteria-type "loans-repaid")
        (>= (get loans-repaid stats) criteria-value)
        (if (is-eq criteria-type "total-borrowed")
          (>= (get total-borrowed stats) criteria-value)
          (if (is-eq criteria-type "consecutive")
            (>= (get consecutive-repayments stats) criteria-value)
            (if (is-eq criteria-type "referrals")
              (>= (get referrals-made stats) criteria-value)
              (>= (get points-earned stats) criteria-value)
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (get-user-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-achievement-nft (token-id uint))
  (map-get? achievement-nfts { token-id: token-id })
)

(define-read-only (check-unlockable-achievements (user principal))
  (let
    (
      (stats (default-to { loans-created: u0, loans-repaid: u0, total-borrowed: u0,
                          total-repaid: u0, consecutive-repayments: u0, referrals-made: u0,
                          points-earned: u0 }
             (map-get? user-stats { user: user })))
    )
    (ok (list
      (check-single-achievement user u1 stats)
      (check-single-achievement user u2 stats)
      (check-single-achievement user u3 stats)
      (check-single-achievement user u4 stats)
      (check-single-achievement user u5 stats)
    ))
  )
)

(define-private (check-single-achievement (user principal) (achievement-id uint) 
                                         (stats {loans-created: uint, loans-repaid: uint, total-borrowed: uint,
                                                total-repaid: uint, consecutive-repayments: uint, referrals-made: uint,
                                                points-earned: uint}))
  (let
    (
      (achievement (map-get? achievements { achievement-id: achievement-id }))
      (user-achievement (map-get? user-achievements { user: user, achievement-id: achievement-id }))
    )
    (if (and (is-some achievement) 
             (or (is-none user-achievement) 
                 (not (get unlocked (unwrap-panic user-achievement)))))
      (check-achievement-criteria (unwrap-panic achievement) stats)
      false
    )
  )
)

(define-read-only (get-user-achievement-count (user principal))
  (let
    (
      (achievement-1 (default-to { unlocked: false, unlock-block: u0 }
                     (map-get? user-achievements { user: user, achievement-id: u1 })))
      (achievement-2 (default-to { unlocked: false, unlock-block: u0 }
                     (map-get? user-achievements { user: user, achievement-id: u2 })))
      (achievement-3 (default-to { unlocked: false, unlock-block: u0 }
                     (map-get? user-achievements { user: user, achievement-id: u3 })))
      (achievement-4 (default-to { unlocked: false, unlock-block: u0 }
                     (map-get? user-achievements { user: user, achievement-id: u4 })))
      (achievement-5 (default-to { unlocked: false, unlock-block: u0 }
                     (map-get? user-achievements { user: user, achievement-id: u5 })))
    )
    (+ 
      (if (get unlocked achievement-1) u1 u0)
      (if (get unlocked achievement-2) u1 u0)
      (if (get unlocked achievement-3) u1 u0)
      (if (get unlocked achievement-4) u1 u0)
      (if (get unlocked achievement-5) u1 u0)
    )
  )
)

(define-public (setup-default-achievements)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    (try! (create-achievement "First Steps" "Created your first loan" "milestone" "loans-created" u1 "common" "ipfs://achievement1"))
    (try! (create-achievement "Loan Shark" "Created 10 loans" "milestone" "loans-created" u10 "rare" "ipfs://achievement2"))
    (try! (create-achievement "Reliable Borrower" "Repaid 5 loans on time" "reliability" "loans-repaid" u5 "uncommon" "ipfs://achievement3"))
    (try! (create-achievement "High Roller" "Borrowed over 1000 STX total" "volume" "total-borrowed" u1000000000 "epic" "ipfs://achievement4"))
    (try! (create-achievement "Streak Master" "Made 5 consecutive repayments" "streak" "consecutive" u5 "legendary" "ipfs://achievement5"))
    
    (ok true)
  )
)
