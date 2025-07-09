;; CryptoGarden Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-PRICE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))

;; Contract owner
(define-constant GARDEN-MASTER tx-sender)

;; Plant NFT definition
(define-non-fungible-token crypto-plant uint)

;; Data variables
(define-data-var next-plant-id uint u1)
(define-data-var last-yield-rate uint u500) ;; Starting yield rate in basis points
(define-data-var nursery-fee uint u250) ;; 2.5% fee (250 basis points)

;; Plant metadata structure
(define-map plant-metadata uint {
    species: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    growth-stage: uint,
    planted-block: uint,
    last-watered: uint,
    health-score: uint
})

;; Nursery marketplace listings
(define-map nursery-listings uint {
    gardener: principal,
    price: uint,
    listed-at: uint
})

;; Gardener behavior tracking
(define-map gardener-stats principal {
    total-plants: uint,
    total-waterings: uint,
    last-activity: uint,
    green-thumb-score: uint
})

;; Growth thresholds
(define-map growth-thresholds uint {
    yield-threshold: uint,
    activity-threshold: uint,
    care-threshold: uint
})

;; Initialize growth thresholds
(map-set growth-thresholds u1 {yield-threshold: u400, activity-threshold: u100, care-threshold: u50})
(map-set growth-thresholds u2 {yield-threshold: u600, activity-threshold: u200, care-threshold: u100})
(map-set growth-thresholds u3 {yield-threshold: u800, activity-threshold: u300, care-threshold: u200})

;; Read-only functions

;; Get plant metadata
(define-read-only (get-plant-metadata (plant-id uint))
    (map-get? plant-metadata plant-id)
)

;; Get nursery listing
(define-read-only (get-nursery-listing (plant-id uint))
    (map-get? nursery-listings plant-id)
)

;; Get gardener stats
(define-read-only (get-gardener-stats (gardener principal))
    (map-get? gardener-stats gardener)
)

;; Get plant owner
(define-read-only (get-plant-owner (plant-id uint))
    (nft-get-owner? crypto-plant plant-id)
)

;; Calculate growth stage based on various factors
(define-read-only (calculate-growth-stage (plant-id uint))
    (let ((metadata (unwrap! (get-plant-metadata plant-id) u0))
          (current-yield (var-get last-yield-rate))
          (current-block block-height)
          (planted-block (get planted-block metadata))
          (health-score (get health-score metadata)))
        (let ((block-age (- current-block planted-block))
              (yield-factor (if (> current-yield u600) u2 u1))
              (age-factor (if (> block-age u1000) u2 u1))
              (health-factor (if (> health-score u100) u2 u1)))
            (+ yield-factor age-factor health-factor)
        )
    )
)

;; Get current nursery fee
(define-read-only (get-nursery-fee)
    (var-get nursery-fee)
)

;; Public functions

;; Plant new crypto plant
(define-public (plant-seed (species (string-ascii 64)) (description (string-ascii 256)) (image-uri (string-ascii 256)))
    (let ((plant-id (var-get next-plant-id)))
        (try! (nft-mint? crypto-plant plant-id tx-sender))
        (map-set plant-metadata plant-id {
            species: species,
            description: description,
            image-uri: image-uri,
            growth-stage: u1,
            planted-block: block-height,
            last-watered: block-height,
            health-score: u0
        })
        (update-gardener-stats tx-sender u1 u1)
        (var-set next-plant-id (+ plant-id u1))
        (ok plant-id)
    )
)

;; List plant in nursery
(define-public (list-in-nursery (plant-id uint) (price uint))
    (let ((owner (unwrap! (nft-get-owner? crypto-plant plant-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        (map-set nursery-listings plant-id {
            gardener: tx-sender,
            price: price,
            listed-at: block-height
        })
        (ok true)
    )
)

;; Remove plant from nursery
(define-public (remove-from-nursery (plant-id uint))
    (let ((listing (unwrap! (get-nursery-listing plant-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get gardener listing)) ERR-NOT-AUTHORIZED)
        (map-delete nursery-listings plant-id)
        (ok true)
    )
)

;; Adopt plant from nursery
(define-public (adopt-plant (plant-id uint))
    (let ((listing (unwrap! (get-nursery-listing plant-id) ERR-NOT-FOUND))
          (price (get price listing))
          (seller (get gardener listing))
          (fee (/ (* price (var-get nursery-fee)) u10000)))
        (try! (stx-transfer? (- price fee) tx-sender seller))
        (try! (stx-transfer? fee tx-sender GARDEN-MASTER))
        (try! (nft-transfer? crypto-plant plant-id seller tx-sender))
        (map-delete nursery-listings plant-id)
        (update-gardener-stats tx-sender u1 u1)
        (update-gardener-stats seller u0 u1)
        (update-plant-interaction plant-id)
        (ok true)
    )
)

;; Transfer plant (updates gardener behavior)
(define-public (transfer-plant (plant-id uint) (recipient principal))
    (let ((owner (unwrap! (nft-get-owner? crypto-plant plant-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? crypto-plant plant-id tx-sender recipient))
        (update-gardener-stats tx-sender u0 u1)
        (update-gardener-stats recipient u1 u1)
        (update-plant-interaction plant-id)
        (ok true)
    )
)

;; Update yield rate (can be called by oracle or admin)
(define-public (update-yield-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender GARDEN-MASTER) ERR-NOT-AUTHORIZED)
        (var-set last-yield-rate new-rate)
        (ok true)
    )
)

;; Grow plant based on current conditions
(define-public (grow-plant (plant-id uint))
    (let ((metadata (unwrap! (get-plant-metadata plant-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? crypto-plant plant-id) ERR-NOT-FOUND))
          (new-growth-stage (calculate-growth-stage plant-id)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set plant-metadata plant-id (merge metadata {
            growth-stage: new-growth-stage,
            last-watered: block-height
        }))
        (update-gardener-stats tx-sender u0 u1)
        (ok new-growth-stage)
    )
)

;; Water plant (increases health score)
(define-public (water-plant (plant-id uint))
    (let ((metadata (unwrap! (get-plant-metadata plant-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? crypto-plant plant-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set plant-metadata plant-id (merge metadata {
            last-watered: block-height,
            health-score: (+ (get health-score metadata) u10)
        }))
        (update-gardener-stats tx-sender u0 u1)
        (ok true)
    )
)

;; Private functions

;; Update gardener statistics
(define-private (update-gardener-stats (gardener principal) (plants-change uint) (activity-increment uint))
    (let ((current-stats (default-to {total-plants: u0, total-waterings: u0, last-activity: u0, green-thumb-score: u0} 
                                   (get-gardener-stats gardener))))
        (map-set gardener-stats gardener {
            total-plants: (+ (get total-plants current-stats) plants-change),
            total-waterings: (+ (get total-waterings current-stats) activity-increment),
            last-activity: block-height,
            green-thumb-score: (+ (get green-thumb-score current-stats) (* activity-increment u5))
        })
    )
)

;; Update plant interaction timestamp
(define-private (update-plant-interaction (plant-id uint))
    (let ((metadata (unwrap! (get-plant-metadata plant-id) false)))
        (map-set plant-metadata plant-id (merge metadata {
            last-watered: block-height
        }))
        true
    )
)

;; Admin functions

;; Update nursery fee (only garden master)
(define-public (set-nursery-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender GARDEN-MASTER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-PRICE) ;; Max 10% fee
        (var-set nursery-fee new-fee)
        (ok true)
    )
)