;; ThunderRelease - Decentralized Storage Network Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-invalid-tier (err u104))

;; Data tier definitions
(define-constant tier-hot u1)
(define-constant tier-warm u2)
(define-constant tier-cold u3)

;; Minimum stake required for storage nodes (in microSTX)
(define-constant min-node-stake u1000000)

;; Data Variables
(define-data-var total-nodes uint u0)
(define-data-var total-storage-allocated uint u0)

;; Storage Node Registry
(define-map storage-nodes
  principal
  {
    stake: uint,
    capacity: uint,
    performance-score: uint,
    availability-score: uint,
    total-rewards: uint,
    active: bool
  }
)

;; Data Objects stored in the network
(define-map data-objects
  {object-id: (string-ascii 64)}
  {
    owner: principal,
    size: uint,
    tier: uint,
    access-count: uint,
    replication-factor: uint,
    ipfs-hash: (string-ascii 64),
    created-at: uint,
    last-accessed: uint
  }
)

;; Node assignments for data objects
(define-map object-node-assignments
  {object-id: (string-ascii 64), node: principal}
  {assigned-at: uint, confirmed: bool}
)

;; Read-only functions

(define-read-only (get-node-info (node principal))
  (map-get? storage-nodes node)
)

(define-read-only (get-object-info (object-id (string-ascii 64)))
  (map-get? data-objects {object-id: object-id})
)

(define-read-only (get-total-nodes)
  (ok (var-get total-nodes))
)

(define-read-only (get-total-storage)
  (ok (var-get total-storage-allocated))
)

(define-read-only (is-node-assigned (object-id (string-ascii 64)) (node principal))
  (map-get? object-node-assignments {object-id: object-id, node: node})
)

;; Public functions

;; Register as a storage node
(define-public (register-node (capacity uint))
  (let
    (
      (stake-amount min-node-stake)
    )
    (asserts! (is-none (map-get? storage-nodes tx-sender)) err-already-exists)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set storage-nodes tx-sender {
      stake: stake-amount,
      capacity: capacity,
      performance-score: u100,
      availability-score: u100,
      total-rewards: u0,
      active: true
    })
    (var-set total-nodes (+ (var-get total-nodes) u1))
    (ok true)
  )
)

;; Store a new data object
(define-public (store-object 
    (object-id (string-ascii 64))
    (size uint)
    (tier uint)
    (replication-factor uint)
    (ipfs-hash (string-ascii 64)))
  (begin
    (asserts! (is-none (map-get? data-objects {object-id: object-id})) err-already-exists)
    (asserts! (or (is-eq tier tier-hot) (or (is-eq tier tier-warm) (is-eq tier tier-cold))) err-invalid-tier)
    (map-set data-objects {object-id: object-id} {
      owner: tx-sender,
      size: size,
      tier: tier,
      access-count: u0,
      replication-factor: replication-factor,
      ipfs-hash: ipfs-hash,
      created-at: block-height,
      last-accessed: block-height
    })
    (var-set total-storage-allocated (+ (var-get total-storage-allocated) size))
    (ok true)
  )
)

;; Record data access (updates metrics for tier migration)
(define-public (record-access (object-id (string-ascii 64)))
  (let
    (
      (object (unwrap! (map-get? data-objects {object-id: object-id}) err-not-found))
    )
    (map-set data-objects {object-id: object-id}
      (merge object {
        access-count: (+ (get access-count object) u1),
        last-accessed: block-height
      })
    )
    (ok true)
  )
)

;; Migrate data between tiers based on access patterns
(define-public (migrate-tier (object-id (string-ascii 64)) (new-tier uint))
  (let
    (
      (object (unwrap! (map-get? data-objects {object-id: object-id}) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner object)) err-owner-only)
    (asserts! (or (is-eq new-tier tier-hot) (or (is-eq new-tier tier-warm) (is-eq new-tier tier-cold))) err-invalid-tier)
    (map-set data-objects {object-id: object-id}
      (merge object {tier: new-tier})
    )
    (ok true)
  )
)

;; Assign a storage node to host an object
(define-public (assign-node-to-object (object-id (string-ascii 64)) (node principal))
  (let
    (
      (object (unwrap! (map-get? data-objects {object-id: object-id}) err-not-found))
      (node-info (unwrap! (map-get? storage-nodes node) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner object)) err-owner-only)
    (asserts! (get active node-info) err-not-found)
    (map-set object-node-assignments {object-id: object-id, node: node} {
      assigned-at: block-height,
      confirmed: false
    })
    (ok true)
  )
)

;; Node confirms it has stored the data
(define-public (confirm-storage (object-id (string-ascii 64)))
  (let
    (
      (assignment (unwrap! (map-get? object-node-assignments {object-id: object-id, node: tx-sender}) err-not-found))
    )
    (map-set object-node-assignments {object-id: object-id, node: tx-sender}
      (merge assignment {confirmed: true})
    )
    (ok true)
  )
)

;; Update node performance metrics
(define-public (update-node-score (node principal) (performance uint) (availability uint))
  (let
    (
      (node-info (unwrap! (map-get? storage-nodes node) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set storage-nodes node
      (merge node-info {
        performance-score: performance,
        availability-score: availability
      })
    )
    (ok true)
  )
)

;; Distribute storage mining rewards
(define-public (distribute-rewards (node principal) (reward-amount uint))
  (let
    (
      (node-info (unwrap! (map-get? storage-nodes node) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? reward-amount tx-sender node)))
    (map-set storage-nodes node
      (merge node-info {
        total-rewards: (+ (get total-rewards node-info) reward-amount)
      })
    )
    (ok true)
  )
)

;; Deactivate a storage node
(define-public (deactivate-node)
  (let
    (
      (node-info (unwrap! (map-get? storage-nodes tx-sender) err-not-found))
    )
    (map-set storage-nodes tx-sender
      (merge node-info {active: false})
    )
    (ok true)
  )
)