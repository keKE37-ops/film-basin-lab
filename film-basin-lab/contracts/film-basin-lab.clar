;; FilmBasinLab Smart Contract

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PARAMS (err u103))
(define-constant ERR-SESSION-LOCKED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-INVALID-LICENSE (err u106))
(define-constant ERR-ALREADY-INSPIRED (err u107))

;; Royalty basis points (100 = 1%)
(define-constant ROYALTY-CREATOR-BPS u700)       ;; 7% to original creator
(define-constant ROYALTY-LINEAGE-BPS u200)        ;; 2% split across creative lineage
(define-constant ROYALTY-PLATFORM-BPS u100)       ;; 1% platform fee
(define-constant MAX-BPS u10000)

;; Minimum session lock duration in blocks (~10 minutes at ~10s/block)
(define-constant MIN-SESSION-BLOCKS u60)

;; Max lineage depth for micro-payment distribution
(define-constant MAX-LINEAGE-DEPTH u10)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var last-artwork-id uint u0)
(define-data-var last-layer-id uint u0)
(define-data-var last-inspiration-id uint u0)
(define-data-var platform-balance uint u0)

;; ============================================================
;; NFT DEFINITIONS
;; ============================================================

;; Primary artwork NFT - the final tokenized creative work
(define-non-fungible-token artwork uint)

;; Layer NFT - each significant creative stage/snapshot
(define-non-fungible-token creative-layer uint)

;; Proof-of-Inspiration token - minted when one work inspires another
(define-non-fungible-token proof-of-inspiration uint)

;; ============================================================
;; FUNGIBLE TOKEN
;; ============================================================

;; Basin token for in-platform rewards and micro-payments
(define-fungible-token basin-token)

;; ============================================================
;; MAPS
;; ============================================================

;; Core artwork metadata
(define-map artworks
  { artwork-id: uint }
  {
    creator: principal,
    title: (string-ascii 128),
    content-hash: (buff 32),          ;; SHA-256 of final content
    layer-count: uint,
    created-at: uint,                 ;; block height
    is-for-sale: bool,
    price: uint,                      ;; in microSTX
    license-type: (string-ascii 32),  ;; e.g. "CC-BY", "CC-BY-SA", "CUSTOM"
    derivative-royalty-bps: uint,     ;; royalty bps for derivatives
    parent-artwork-id: (optional uint) ;; set if this is a derivative work
  }
)

;; Creative layer metadata - each stage of creation
(define-map creative-layers
  { layer-id: uint }
  {
    artwork-id: uint,
    creator: principal,
    layer-index: uint,                ;; sequential stage number
    content-hash: (buff 32),          ;; hash of this layer's content
    tool-signature: (string-ascii 64),;; identifier of tool used
    session-id: uint,                 ;; associated proof-of-work session
    created-at: uint
  }
)

;; Proof-of-Work sessions - time-locked creation periods
(define-map creation-sessions
  { session-id: uint }
  {
    creator: principal,
    artwork-id: uint,
    start-block: uint,
    end-block: (optional uint),       ;; none = still in progress
    layer-snapshots: uint,            ;; number of snapshots taken
    is-verified: bool
  }
)

;; Proof-of-Inspiration token metadata
(define-map inspirations
  { inspiration-id: uint }
  {
    source-artwork-id: uint,
    derived-artwork-id: uint,
    credited-by: principal,
    credit-note: (string-ascii 256),
    created-at: uint
  }
)

;; Creative lineage - tracks ancestry chain for royalty distribution
(define-map artwork-lineage
  { artwork-id: uint, depth: uint }
  { ancestor-artwork-id: uint, ancestor-creator: principal }
)

;; Tracks lineage depth per artwork
(define-map lineage-depth
  { artwork-id: uint }
  { depth: uint }
)

;; Revenue accrued per principal (withdrawable)
(define-map creator-balances
  { creator: principal }
  { balance: uint }
)

;; License usage grants
(define-map license-grants
  { artwork-id: uint, licensee: principal }
  {
    granted-at: uint,
    license-type: (string-ascii 32),
    usage-scope: (string-ascii 128),
    expires-at: (optional uint)
  }
)

;; Session counter per creator (for unique session IDs)
(define-map creator-session-count
  { creator: principal }
  { count: uint }
)

;; Inspiration deduplication guard
(define-map inspiration-exists
  { source-artwork-id: uint, derived-artwork-id: uint }
  { exists: bool }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (get-next-artwork-id)
  (let ((next (+ (var-get last-artwork-id) u1)))
    (var-set last-artwork-id next)
    next))

(define-private (get-next-layer-id)
  (let ((next (+ (var-get last-layer-id) u1)))
    (var-set last-layer-id next)
    next))

(define-private (get-next-inspiration-id)
  (let ((next (+ (var-get last-inspiration-id) u1)))
    (var-set last-inspiration-id next)
    next))

(define-private (get-next-session-id (creator principal))
  (let (
    (current (default-to { count: u0 } (map-get? creator-session-count { creator: creator })))
    (next (+ (get count current) u1))
  )
    (map-set creator-session-count { creator: creator } { count: next })
    next))

(define-private (credit-balance (recipient principal) (amount uint))
  (let ((current (default-to { balance: u0 } (map-get? creator-balances { creator: recipient }))))
    (map-set creator-balances
      { creator: recipient }
      { balance: (+ (get balance current) amount) })))

;; Distribute royalties across lineage ancestors up to MAX-LINEAGE-DEPTH
(define-private (distribute-lineage-royalty (artwork-id uint) (total-lineage-amount uint))
  (let ((depth (default-to { depth: u0 } (map-get? lineage-depth { artwork-id: artwork-id }))))
    (if (is-eq (get depth depth) u0)
      true
      ;; Simple flat split: give the full lineage share to immediate parent creator
      ;; A full recursive implementation would require fold over a list of ancestors
      (let ((ancestor-opt (map-get? artwork-lineage { artwork-id: artwork-id, depth: u1 })))
        (match ancestor-opt
          ancestor (begin
            (credit-balance (get ancestor-creator ancestor) total-lineage-amount)
            true)
          true)))))

;; Process a sale: split proceeds among creator, lineage, and platform
(define-private (process-sale-royalties (artwork-id uint) (sale-price uint) (seller principal))
  (let (
    (creator-share (/ (* sale-price ROYALTY-CREATOR-BPS) MAX-BPS))
    (lineage-share (/ (* sale-price ROYALTY-LINEAGE-BPS) MAX-BPS))
    (platform-share (/ (* sale-price ROYALTY-PLATFORM-BPS) MAX-BPS))
  )
    (credit-balance seller creator-share)
    (distribute-lineage-royalty artwork-id lineage-share)
    (var-set platform-balance (+ (var-get platform-balance) platform-share))
    true))

;; ============================================================
;; PUBLIC FUNCTIONS
;; ============================================================

;; --- Creation Sessions (Proof of Work) ---

;; Start a new time-locked creation session for an artwork
(define-public (start-creation-session (artwork-id uint))
  (let (
    (session-id (get-next-session-id tx-sender))
    (art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set creation-sessions
      { session-id: session-id }
      {
        creator: tx-sender,
        artwork-id: artwork-id,
        start-block: block-height,
        end-block: none,
        layer-snapshots: u0,
        is-verified: false
      })
    (ok session-id)))

;; End and verify a creation session (requires MIN-SESSION-BLOCKS elapsed)
(define-public (end-creation-session (session-id uint))
  (let (
    (session (unwrap! (map-get? creation-sessions { session-id: session-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get creator session) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (get end-block session)) ERR-SESSION-LOCKED)
    (asserts! (>= (- block-height (get start-block session)) MIN-SESSION-BLOCKS) ERR-SESSION-LOCKED)
    (map-set creation-sessions
      { session-id: session-id }
      (merge session {
        end-block: (some block-height),
        is-verified: true
      }))
    (ok true)))

;; --- Artwork Minting ---

;; Mint a new primary artwork NFT
(define-public (mint-artwork
  (title (string-ascii 128))
  (content-hash (buff 32))
  (license-type (string-ascii 32))
  (derivative-royalty-bps uint)
  (price uint)
  (parent-artwork-id (optional uint))
)
  (let (
    (artwork-id (get-next-artwork-id))
  )
    ;; Validate royalty bps
    (asserts! (<= derivative-royalty-bps u3000) ERR-INVALID-PARAMS) ;; max 30%
    ;; If derivative, verify parent exists and record lineage
    (match parent-artwork-id
      pid (let ((parent (unwrap! (map-get? artworks { artwork-id: pid }) ERR-NOT-FOUND)))
            (let ((parent-depth (default-to { depth: u0 } (map-get? lineage-depth { artwork-id: pid }))))
              (map-set artwork-lineage
                { artwork-id: artwork-id, depth: u1 }
                { ancestor-artwork-id: pid, ancestor-creator: (get creator parent) })
              (map-set lineage-depth
                { artwork-id: artwork-id }
                { depth: (+ (get depth parent-depth) u1) })))
      true)
    ;; Mint the NFT to creator
    (try! (nft-mint? artwork artwork-id tx-sender))
    ;; Store metadata
    (map-set artworks
      { artwork-id: artwork-id }
      {
        creator: tx-sender,
        title: title,
        content-hash: content-hash,
        layer-count: u0,
        created-at: block-height,
        is-for-sale: false,
        price: price,
        license-type: license-type,
        derivative-royalty-bps: derivative-royalty-bps,
        parent-artwork-id: parent-artwork-id
      })
    (ok artwork-id)))

;; --- Creative Layer Minting ---

;; Mint a creative layer (snapshot) for an artwork - requires verified session
(define-public (mint-creative-layer
  (artwork-id uint)
  (session-id uint)
  (layer-content-hash (buff 32))
  (tool-signature (string-ascii 64))
)
  (let (
    (layer-id (get-next-layer-id))
    (art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND))
    (session (unwrap! (map-get? creation-sessions { session-id: session-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get creator session) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get is-verified session) ERR-SESSION-LOCKED)
    ;; Mint layer NFT to creator
    (try! (nft-mint? creative-layer layer-id tx-sender))
    ;; Record layer metadata
    (map-set creative-layers
      { layer-id: layer-id }
      {
        artwork-id: artwork-id,
        creator: tx-sender,
        layer-index: (get layer-count art-data),
        content-hash: layer-content-hash,
        tool-signature: tool-signature,
        session-id: session-id,
        created-at: block-height
      })
    ;; Increment layer count on artwork
    (map-set artworks
      { artwork-id: artwork-id }
      (merge art-data { layer-count: (+ (get layer-count art-data) u1) }))
    ;; Mint basin token reward for layer creation
    (try! (ft-mint? basin-token u10 tx-sender))
    (ok layer-id)))

;; --- Marketplace ---

;; List an artwork for sale
(define-public (list-artwork (artwork-id uint) (sale-price uint))
  (let ((art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> sale-price u0) ERR-INVALID-PARAMS)
    (map-set artworks
      { artwork-id: artwork-id }
      (merge art-data { is-for-sale: true, price: sale-price }))
    (ok true)))

;; Delist an artwork from sale
(define-public (delist-artwork (artwork-id uint))
  (let ((art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set artworks
      { artwork-id: artwork-id }
      (merge art-data { is-for-sale: false }))
    (ok true)))

;; Purchase an artwork - triggers full royalty split
(define-public (purchase-artwork (artwork-id uint))
  (let (
    (art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND))
    (owner (unwrap! (nft-get-owner? artwork artwork-id) ERR-NOT-FOUND))
    (price (get price art-data))
  )
    (asserts! (get is-for-sale art-data) ERR-INVALID-PARAMS)
    (asserts! (not (is-eq tx-sender owner)) ERR-NOT-AUTHORIZED)
    ;; Transfer STX from buyer
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    ;; Distribute royalties
    (process-sale-royalties artwork-id price owner)
    ;; Transfer NFT to buyer
    (try! (nft-transfer? artwork artwork-id owner tx-sender))
    ;; Mark as no longer for sale
    (map-set artworks
      { artwork-id: artwork-id }
      (merge art-data { is-for-sale: false }))
    (ok true)))

;; --- Proof of Inspiration ---

;; Credit a source artwork as inspiration for a derivative
(define-public (credit-inspiration
  (source-artwork-id uint)
  (derived-artwork-id uint)
  (credit-note (string-ascii 256))
)
  (let (
    (inspiration-id (get-next-inspiration-id))
    (source (unwrap! (map-get? artworks { artwork-id: source-artwork-id }) ERR-NOT-FOUND))
    (derived (unwrap! (map-get? artworks { artwork-id: derived-artwork-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get creator derived) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? inspiration-exists
      { source-artwork-id: source-artwork-id, derived-artwork-id: derived-artwork-id }))
      ERR-ALREADY-INSPIRED)
    ;; Mark as exists
    (map-set inspiration-exists
      { source-artwork-id: source-artwork-id, derived-artwork-id: derived-artwork-id }
      { exists: true })
    ;; Mint Proof-of-Inspiration NFT to source creator
    (try! (nft-mint? proof-of-inspiration inspiration-id (get creator source)))
    ;; Record metadata
    (map-set inspirations
      { inspiration-id: inspiration-id }
      {
        source-artwork-id: source-artwork-id,
        derived-artwork-id: derived-artwork-id,
        credited-by: tx-sender,
        credit-note: credit-note,
        created-at: block-height
      })
    ;; Reward both parties with basin tokens
    (try! (ft-mint? basin-token u5 tx-sender))
    (try! (ft-mint? basin-token u5 (get creator source)))
    (ok inspiration-id)))

;; --- Licensing ---

;; Grant a license to a specific principal
(define-public (grant-license
  (artwork-id uint)
  (licensee principal)
  (license-type (string-ascii 32))
  (usage-scope (string-ascii 128))
  (expires-at (optional uint))
)
  (let ((art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set license-grants
      { artwork-id: artwork-id, licensee: licensee }
      {
        granted-at: block-height,
        license-type: license-type,
        usage-scope: usage-scope,
        expires-at: expires-at
      })
    (ok true)))

;; Revoke a license
(define-public (revoke-license (artwork-id uint) (licensee principal))
  (let ((art-data (unwrap! (map-get? artworks { artwork-id: artwork-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq (get creator art-data) tx-sender) ERR-NOT-AUTHORIZED)
    (map-delete license-grants { artwork-id: artwork-id, licensee: licensee })
    (ok true)))

;; --- Withdrawals ---

;; Creator withdraws their accrued balance
(define-public (withdraw-balance)
  (let (
    (record (unwrap! (map-get? creator-balances { creator: tx-sender }) ERR-NOT-FOUND))
    (amount (get balance record))
  )
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    (map-set creator-balances { creator: tx-sender } { balance: u0 })
    (as-contract (stx-transfer? amount tx-sender tx-sender))))

;; Platform owner withdraws platform fees
(define-public (withdraw-platform-balance)
  (let ((amount (var-get platform-balance)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    (var-set platform-balance u0)
    (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER))))

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks { artwork-id: artwork-id }))

(define-read-only (get-creative-layer (layer-id uint))
  (map-get? creative-layers { layer-id: layer-id }))

(define-read-only (get-creation-session (session-id uint))
  (map-get? creation-sessions { session-id: session-id }))

(define-read-only (get-inspiration (inspiration-id uint))
  (map-get? inspirations { inspiration-id: inspiration-id }))

(define-read-only (get-license-grant (artwork-id uint) (licensee principal))
  (map-get? license-grants { artwork-id: artwork-id, licensee: licensee }))

(define-read-only (get-creator-balance (creator principal))
  (default-to { balance: u0 } (map-get? creator-balances { creator: creator })))

(define-read-only (get-artwork-owner (artwork-id uint))
  (nft-get-owner? artwork artwork-id))

(define-read-only (get-layer-owner (layer-id uint))
  (nft-get-owner? creative-layer layer-id))

(define-read-only (get-basin-balance (holder principal))
  (ft-get-balance basin-token holder))

(define-read-only (get-platform-balance)
  (var-get platform-balance))

(define-read-only (get-artwork-lineage-ancestor (artwork-id uint) (depth uint))
  (map-get? artwork-lineage { artwork-id: artwork-id, depth: depth }))

(define-read-only (get-artwork-lineage-depth (artwork-id uint))
  (default-to { depth: u0 } (map-get? lineage-depth { artwork-id: artwork-id })))

(define-read-only (check-inspiration-exists (source-artwork-id uint) (derived-artwork-id uint))
  (is-some (map-get? inspiration-exists
    { source-artwork-id: source-artwork-id, derived-artwork-id: derived-artwork-id })))

(define-read-only (has-valid-license (artwork-id uint) (licensee principal))
  (match (map-get? license-grants { artwork-id: artwork-id, licensee: licensee })
    grant (match (get expires-at grant)
             expiry (< block-height expiry)
             true)
    false))
