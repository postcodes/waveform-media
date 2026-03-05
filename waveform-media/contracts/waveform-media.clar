;; WaveForm Media - Living Media Assets

(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; ============================================================
;; Constants
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-TOKEN-NOT-FOUND       (err u101))
(define-constant ERR-ALREADY-MINTED        (err u102))
(define-constant ERR-INVALID-ROYALTY       (err u103))
(define-constant ERR-INVALID-CONTRIBUTOR   (err u104))
(define-constant ERR-LICENSE-NOT-FOUND     (err u105))
(define-constant ERR-INSUFFICIENT-PAYMENT  (err u106))
(define-constant ERR-DISPUTE-NOT-FOUND     (err u107))
(define-constant ERR-INVALID-PARAMS        (err u108))
(define-constant ERR-TRANSFER-FAILED       (err u109))

;; License type identifiers
(define-constant LICENSE-SINGLE-USE    u1)
(define-constant LICENSE-MULTI-USE     u2)
(define-constant LICENSE-PERPETUAL     u3)

;; Dispute status codes
(define-constant DISPUTE-OPEN       u1)
(define-constant DISPUTE-RESOLVED   u2)
(define-constant DISPUTE-REJECTED   u3)

;; Royalty basis points denominator (100.00%)
(define-constant BASIS-POINTS u10000)

;; Maximum contributors per asset
(define-constant MAX-CONTRIBUTORS u10)

;; ============================================================
;; Data Variables
;; ============================================================

(define-data-var last-token-id uint u0)
(define-data-var last-license-id uint u0)
(define-data-var last-dispute-id uint u0)
(define-data-var platform-fee-bps uint u250) ;; 2.50% platform fee

;; ============================================================
;; Data Maps
;; ============================================================

;; Core NFT ownership
(define-map token-owner
  { token-id: uint }
  { owner: principal }
)

;; Media asset metadata and IPFS reference
(define-map media-assets
  { token-id: uint }
  {
    creator:        principal,
    ipfs-hash:      (string-ascii 64),   ;; IPFS CID for media file
    content-hash:   (buff 32),            ;; on-chain cryptographic fingerprint
    token-uri:      (string-ascii 256),
    is-derivative:  bool,
    parent-id:      (optional uint),      ;; parent token if derivative
    created-at:     uint,                 ;; block height
    is-active:      bool
  }
)

;; Contributor graph: up to MAX-CONTRIBUTORS contributors per token
;; Each entry stores one contributor and their royalty share in basis points
(define-map contributors
  { token-id: uint, index: uint }
  {
    contributor:  principal,
    share-bps:    uint              ;; e.g. u5000 = 50.00%
  }
)

(define-map contributor-count
  { token-id: uint }
  { count: uint }
)

;; License records
(define-map licenses
  { license-id: uint }
  {
    token-id:     uint,
    licensee:     principal,
    license-type: uint,             ;; LICENSE-SINGLE-USE | LICENSE-MULTI-USE | LICENSE-PERPETUAL
    price-ustx:   uint,
    granted-at:   uint,             ;; block height
    expires-at:   (optional uint),  ;; block height, none = perpetual
    is-active:    bool
  }
)

;; Track licenses issued per token
(define-map token-license-count
  { token-id: uint }
  { count: uint }
)

;; Dynamic pricing parameters per token
(define-map pricing-config
  { token-id: uint }
  {
    base-price-single-ustx:    uint,
    base-price-multi-ustx:     uint,
    base-price-perpetual-ustx: uint,
    usage-count:               uint   ;; incremented on each license grant
  }
)

;; Dispute records
(define-map disputes
  { dispute-id: uint }
  {
    token-id:    uint,
    claimant:    principal,
    respondent:  principal,
    reason:      (string-ascii 256),
    status:      uint,
    raised-at:   uint,
    resolved-at: (optional uint)
  }
)

;; Accumulated royalty balances available for withdrawal
(define-map royalty-balance
  { recipient: principal }
  { balance-ustx: uint }
)

;; ============================================================
;; Private Helpers
;; ============================================================

;; Add an amount to a principal's royalty balance
(define-private (credit-royalty (recipient principal) (amount-ustx uint))
  (let (
    (current (default-to { balance-ustx: u0 }
               (map-get? royalty-balance { recipient: recipient })))
  )
    (map-set royalty-balance
      { recipient: recipient }
      { balance-ustx: (+ (get balance-ustx current) amount-ustx) }
    )
  )
)

;; Distribute payment among contributors according to their share-bps.
;; Iterates over indices 0..9 and credits each contributor.
;; Returns the total amount distributed to contributors (platform fee excluded).
(define-private (distribute-to-contributor
    (index uint)
    (state { token-id: uint, remaining: uint, distributed: uint })
  )
  (let (
    (token-id  (get token-id state))
    (entry     (map-get? contributors { token-id: token-id, index: index }))
    (count-rec (default-to { count: u0 }
                 (map-get? contributor-count { token-id: token-id })))
  )
    (if (and (is-some entry) (< index (get count count-rec)))
      (let (
        (c      (unwrap-panic entry))
        (share  (/ (* (get remaining state) (get share-bps c)) BASIS-POINTS))
      )
        (credit-royalty (get contributor c) share)
        { token-id: token-id,
          remaining: (get remaining state),
          distributed: (+ (get distributed state) share) }
      )
      state
    )
  )
)

;; ============================================================
;; SIP-009 Read-Only Functions
;; ============================================================

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
  (match (map-get? media-assets { token-id: token-id })
    asset (ok (some (get token-uri asset)))
    (err u101)
  )
)

(define-read-only (get-owner (token-id uint))
  (match (map-get? token-owner { token-id: token-id })
    rec (ok (some (get owner rec)))
    (err u101)
  )
)

;; ============================================================
;; SIP-009 Transfer
;; ============================================================

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (match (map-get? token-owner { token-id: token-id })
      rec
        (begin
          (asserts! (is-eq (get owner rec) sender) ERR-NOT-AUTHORIZED)
          (map-set token-owner { token-id: token-id } { owner: recipient })
          (ok true)
        )
      ERR-TOKEN-NOT-FOUND
    )
  )
)

;; ============================================================
;; Minting
;; ============================================================

;; Mint a new Living Media Asset.
;; contributor-principals and contributor-shares-bps must be parallel
;; lists of equal length (up to MAX-CONTRIBUTORS).
;; All share-bps values must sum to BASIS-POINTS (10000).
(define-public (mint-asset
    (ipfs-hash            (string-ascii 64))
    (content-hash         (buff 32))
    (token-uri-str        (string-ascii 256))
    (is-derivative        bool)
    (parent-id            (optional uint))
    (contributor-list     (list 10 { contributor: principal, share-bps: uint }))
    (base-single-ustx     uint)
    (base-multi-ustx      uint)
    (base-perpetual-ustx  uint)
  )
  (let (
    (new-id (+ (var-get last-token-id) u1))
    (n      (len contributor-list))
  )
    ;; Validate contributor count
    (asserts! (and (> n u0) (<= n MAX-CONTRIBUTORS)) ERR-INVALID-PARAMS)

    ;; Validate share sum = 10000
    (asserts!
      (is-eq BASIS-POINTS
        (fold + (map get-share contributor-list) u0))
      ERR-INVALID-ROYALTY
    )

    ;; If derivative, parent must exist
    (match parent-id
      pid (asserts! (is-some (map-get? media-assets { token-id: pid })) ERR-TOKEN-NOT-FOUND)
      true
    )

    ;; Record token ownership
    (map-set token-owner { token-id: new-id } { owner: tx-sender })

    ;; Record asset metadata
    (map-set media-assets { token-id: new-id }
      {
        creator:       tx-sender,
        ipfs-hash:     ipfs-hash,
        content-hash:  content-hash,
        token-uri:     token-uri-str,
        is-derivative: is-derivative,
        parent-id:     parent-id,
        created-at:    block-height,
        is-active:     true
      }
    )

    ;; Store contributors via fold
    (fold store-contributor contributor-list { token-id: new-id, index: u0 })
    (map-set contributor-count { token-id: new-id } { count: n })

    ;; Store pricing config
    (map-set pricing-config { token-id: new-id }
      {
        base-price-single-ustx:    base-single-ustx,
        base-price-multi-ustx:     base-multi-ustx,
        base-price-perpetual-ustx: base-perpetual-ustx,
        usage-count:               u0
      }
    )

    ;; Advance token counter
    (var-set last-token-id new-id)
    (ok new-id)
  )
)

;; Helper: extract share-bps from a contributor tuple (used in fold for sum)
(define-private (get-share (c { contributor: principal, share-bps: uint }))
  (get share-bps c)
)

;; Helper: store a single contributor entry during mint fold
(define-private (store-contributor
    (c     { contributor: principal, share-bps: uint })
    (state { token-id: uint, index: uint })
  )
  (begin
    (map-set contributors
      { token-id: (get token-id state), index: (get index state) }
      { contributor: (get contributor c), share-bps: (get share-bps c) }
    )
    { token-id: (get token-id state), index: (+ (get index state) u1) }
  )
)

;; ============================================================
;; Licensing
;; ============================================================

;; Purchase a license for a media asset.
;; Payment (in uSTX) must match or exceed the asset's price for the chosen type.
;; Royalties are distributed to contributors; platform fee credited to CONTRACT-OWNER.
(define-public (purchase-license
    (token-id     uint)
    (license-type uint)
    (duration     (optional uint))   ;; duration in blocks; none = perpetual
  )
  (let (
    (asset   (unwrap! (map-get? media-assets { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
    (pricing (unwrap! (map-get? pricing-config { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
  )
    (asserts! (get is-active asset) ERR-NOT-AUTHORIZED)
    (asserts!
      (or (is-eq license-type LICENSE-SINGLE-USE)
          (is-eq license-type LICENSE-MULTI-USE)
          (is-eq license-type LICENSE-PERPETUAL))
      ERR-INVALID-PARAMS
    )

    (let (
      (base-price
        (if (is-eq license-type LICENSE-SINGLE-USE)
          (get base-price-single-ustx pricing)
          (if (is-eq license-type LICENSE-MULTI-USE)
            (get base-price-multi-ustx pricing)
            (get base-price-perpetual-ustx pricing)
          )
        )
      )
      ;; Dynamic price: increases 1% for every 100 licenses issued
      (dynamic-price
        (+ base-price
           (* (/ (get usage-count pricing) u100)
              (/ base-price u100)))
      )
      (platform-fee   (/ (* dynamic-price (var-get platform-fee-bps)) BASIS-POINTS))
      (creator-pool   (- dynamic-price platform-fee))
      (new-license-id (+ (var-get last-license-id) u1))
      (expires
        (match duration
          d (some (+ block-height d))
          none
        )
      )
    )
      ;; Collect payment from licensee
      (unwrap! (stx-transfer? dynamic-price tx-sender (as-contract tx-sender)) ERR-TRANSFER-FAILED)

      ;; Credit platform fee
      (credit-royalty CONTRACT-OWNER platform-fee)

      ;; Distribute creator pool to contributors
      (fold distribute-to-contributor
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
        { token-id: token-id, remaining: creator-pool, distributed: u0 }
      )

      ;; If parent exists (derivative), credit parent creator a 5% override
      (match (get parent-id asset)
        pid
          (match (map-get? media-assets { token-id: pid })
            parent-asset
              (credit-royalty (get creator parent-asset)
                              (/ creator-pool u20))
            true
          )
        true
      )

      ;; Record license
      (map-set licenses { license-id: new-license-id }
        {
          token-id:     token-id,
          licensee:     tx-sender,
          license-type: license-type,
          price-ustx:   dynamic-price,
          granted-at:   block-height,
          expires-at:   expires,
          is-active:    true
        }
      )

      ;; Increment usage count and license counter
      (map-set pricing-config { token-id: token-id }
        (merge pricing { usage-count: (+ (get usage-count pricing) u1) })
      )
      (var-set last-license-id new-license-id)

      (ok new-license-id)
    )
  )
)

;; ============================================================
;; Royalty Withdrawal
;; ============================================================

;; Withdraw accumulated royalties to the caller's wallet.
(define-public (withdraw-royalties)
  (let (
    (record (default-to { balance-ustx: u0 }
               (map-get? royalty-balance { recipient: tx-sender })))
    (amount (get balance-ustx record))
  )
    (asserts! (> amount u0) ERR-INSUFFICIENT-PAYMENT)
    (map-set royalty-balance { recipient: tx-sender } { balance-ustx: u0 })
    (unwrap! (as-contract (stx-transfer? amount tx-sender tx-sender)) ERR-TRANSFER-FAILED)
    (ok amount)
  )
)

;; ============================================================
;; Dispute Resolution
;; ============================================================

;; Raise a dispute (e.g. unauthorized usage claim).
(define-public (raise-dispute
    (token-id   uint)
    (respondent principal)
    (reason     (string-ascii 256))
  )
  (let (
    (new-dispute-id (+ (var-get last-dispute-id) u1))
  )
    (asserts! (is-some (map-get? media-assets { token-id: token-id })) ERR-TOKEN-NOT-FOUND)
    (map-set disputes { dispute-id: new-dispute-id }
      {
        token-id:    token-id,
        claimant:    tx-sender,
        respondent:  respondent,
        reason:      reason,
        status:      DISPUTE-OPEN,
        raised-at:   block-height,
        resolved-at: none
      }
    )
    (var-set last-dispute-id new-dispute-id)
    (ok new-dispute-id)
  )
)

;; Resolve a dispute (CONTRACT-OWNER acts as arbitrator).
(define-public (resolve-dispute (dispute-id uint) (outcome uint))
  (let (
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status dispute) DISPUTE-OPEN) ERR-NOT-AUTHORIZED)
    (asserts!
      (or (is-eq outcome DISPUTE-RESOLVED) (is-eq outcome DISPUTE-REJECTED))
      ERR-INVALID-PARAMS
    )
    (map-set disputes { dispute-id: dispute-id }
      (merge dispute { status: outcome, resolved-at: (some block-height) })
    )
    (ok true)
  )
)

;; ============================================================
;; Admin
;; ============================================================

;; Deactivate a media asset (creator or CONTRACT-OWNER only).
(define-public (deactivate-asset (token-id uint))
  (let (
    (asset (unwrap! (map-get? media-assets { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
  )
    (asserts!
      (or (is-eq tx-sender CONTRACT-OWNER)
          (is-eq tx-sender (get creator asset)))
      ERR-NOT-AUTHORIZED
    )
    (map-set media-assets { token-id: token-id }
      (merge asset { is-active: false })
    )
    (ok true)
  )
)

;; Update platform fee (CONTRACT-OWNER only). Max 10%.
(define-public (set-platform-fee (new-bps uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-bps u1000) ERR-INVALID-PARAMS)
    (var-set platform-fee-bps new-bps)
    (ok true)
  )
)

;; ============================================================
;; Read-Only Queries
;; ============================================================

(define-read-only (get-asset (token-id uint))
  (map-get? media-assets { token-id: token-id })
)

(define-read-only (get-contributor (token-id uint) (index uint))
  (map-get? contributors { token-id: token-id, index: index })
)

(define-read-only (get-contributor-count (token-id uint))
  (default-to { count: u0 } (map-get? contributor-count { token-id: token-id }))
)

(define-read-only (get-license (license-id uint))
  (map-get? licenses { license-id: license-id })
)

(define-read-only (get-pricing (token-id uint))
  (map-get? pricing-config { token-id: token-id })
)

(define-read-only (get-royalty-balance (recipient principal))
  (default-to { balance-ustx: u0 } (map-get? royalty-balance { recipient: recipient }))
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-bps)
)
