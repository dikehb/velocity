;; VelocityBid - Advanced Web3 Auction Platform

;; Define error constants with descriptive names
(define-constant ERR_INVALID_AMOUNT (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_AUCTION_NOT_FOUND (err u102))
(define-constant ERR_UNAUTHORIZED (err u103))
(define-constant ERR_ALREADY_BIDDING (err u104))
(define-constant ERR_INVALID_PRINCIPAL (err u105))
(define-constant ERR_NOT_PARTICIPATING (err u106))
(define-constant ERR_ZERO_AMOUNT (err u107))
(define-constant ERR_BID_ALREADY_PROCESSED (err u108))
(define-constant ERR_TREASURY_EMPTY (err u109))
(define-constant ERR_AUCTION_NOT_EXPIRED (err u110))
(define-constant ERR_BID_EXCEEDS_LIMIT (err u111))

;; Define the contract state variables
(define-data-var platform-treasury uint u0)
(define-data-var protocol-curator principal tx-sender)
(define-map active-bidders principal uint)
(define-map auction-bids { bidder: principal, amount: uint } { status: (string-ascii 20), timestamp: uint, settlement-amount: uint })

;; Define the auction validity period (30 days in blocks, assuming 10-minute block times)
(define-constant AUCTION_VALIDITY_PERIOD u4320)

;; Function to register for auction participation
(define-public (register-participation (bidding-limit uint))
  (let ((participant tx-sender))
    (asserts! (> bidding-limit u0) ERR_ZERO_AMOUNT)
    (asserts! (is-none (map-get? active-bidders participant)) ERR_ALREADY_BIDDING)
    (match (stx-transfer? bidding-limit participant (as-contract tx-sender))
      success (begin
        (var-set platform-treasury (+ (var-get platform-treasury) bidding-limit))
        (map-set active-bidders participant bidding-limit)
        (print { event: "participation-registered", bidding-limit: bidding-limit, participant: participant })
        (ok true))
      error (err error))))

;; Function to submit an auction bid
(define-public (submit-bid (bid-amount uint))
  (let (
    (bidder tx-sender)
    (bidding-limit (default-to u0 (map-get? active-bidders bidder)))
  )
    (asserts! (> bid-amount u0) ERR_ZERO_AMOUNT)
    (asserts! (is-some (map-get? active-bidders bidder)) ERR_NOT_PARTICIPATING)
    (asserts! (>= bidding-limit bid-amount) ERR_BID_EXCEEDS_LIMIT)
    (asserts! (is-none (map-get? auction-bids { bidder: bidder, amount: bid-amount })) ERR_BID_ALREADY_PROCESSED)
    (map-set auction-bids { bidder: bidder, amount: bid-amount } { status: "pending", timestamp: block-height, settlement-amount: u0 })
    (print { event: "bid-submitted", bidder: bidder, bid-amount: bid-amount, timestamp: block-height })
    (ok true)))

;; Helper function to calculate settlement amount
(define-private (calculate-settlement (bid-amount uint) (treasury-balance uint))
  (if (>= treasury-balance bid-amount)
      bid-amount
      treasury-balance))

;; Function to approve and process a bid
(define-public (approve-bid (bidder principal) (bid-amount uint))
  (let (
    (bid-key { bidder: bidder, amount: bid-amount })
    (bid-data (unwrap! (map-get? auction-bids bid-key) ERR_AUCTION_NOT_FOUND))
    (treasury-balance (var-get platform-treasury))
    (bidding-limit (unwrap! (map-get? active-bidders bidder) ERR_NOT_PARTICIPATING))
  )
    (asserts! (is-eq tx-sender (var-get protocol-curator)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bid-data) "pending") ERR_BID_ALREADY_PROCESSED)
    (asserts! (> treasury-balance u0) ERR_TREASURY_EMPTY)
    (asserts! (<= bid-amount bidding-limit) ERR_BID_EXCEEDS_LIMIT)
    (asserts! (< (- block-height (get timestamp bid-data)) AUCTION_VALIDITY_PERIOD) ERR_AUCTION_NOT_EXPIRED)
    (let ((settlement (calculate-settlement bid-amount treasury-balance)))
      (match (as-contract (stx-transfer? settlement tx-sender bidder))
        success (begin
          (var-set platform-treasury (- treasury-balance settlement))
          (if (< settlement bid-amount)
              (map-set auction-bids bid-key { status: "partial-settlement", timestamp: block-height, settlement-amount: settlement })
              (begin
                (map-delete auction-bids bid-key)
                (map-delete active-bidders bidder)))
          (print { event: "bid-approved", bidder: bidder, bid-amount: bid-amount, settlement: settlement })
          (ok settlement))
        error (err error)))))

;; Function to reject a bid
(define-public (reject-bid (bidder principal) (bid-amount uint))
  (let (
    (bid-key { bidder: bidder, amount: bid-amount })
    (bid-data (unwrap! (map-get? auction-bids bid-key) ERR_AUCTION_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get protocol-curator)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bid-data) "pending") ERR_BID_ALREADY_PROCESSED)
    (asserts! (< (- block-height (get timestamp bid-data)) AUCTION_VALIDITY_PERIOD) ERR_AUCTION_NOT_EXPIRED)
    (map-set auction-bids bid-key { status: "rejected", timestamp: (get timestamp bid-data), settlement-amount: u0 })
    (print { event: "bid-rejected", bidder: bidder, bid-amount: bid-amount })
    (ok true)))

;; Function to check and expire an outdated bid
(define-public (expire-outdated-bid (bidder principal) (bid-amount uint))
  (let (
    (bid-key { bidder: bidder, amount: bid-amount })
    (bid-data (unwrap! (map-get? auction-bids bid-key) ERR_AUCTION_NOT_FOUND))
  )
    (if (and (is-eq (get status bid-data) "pending")
             (>= (- block-height (get timestamp bid-data)) AUCTION_VALIDITY_PERIOD))
        (begin
          (map-set auction-bids bid-key { status: "expired", timestamp: (get timestamp bid-data), settlement-amount: u0 })
          (print { event: "bid-expired", bidder: bidder, bid-amount: bid-amount })
          (ok true))
        (ok false))))

;; Function to transfer curator privileges
(define-public (transfer-curator (new-curator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-curator)) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq new-curator 'SP000000000000000000002Q6VF78)) ERR_INVALID_PRINCIPAL)
    (print { event: "curator-transferred", previous-curator: (var-get protocol-curator), new-curator: new-curator })
    (ok (var-set protocol-curator new-curator))))

;; Function to query the current platform treasury balance
(define-read-only (get-treasury-balance)
  (ok (var-get platform-treasury)))

;; Function to check if a participant is active in bidding
(define-read-only (has-active-participation (participant principal))
  (is-some (map-get? active-bidders participant)))

;; Function to query the bidding limit for a participant
(define-read-only (get-bidding-limit (participant principal))
  (ok (default-to u0 (map-get? active-bidders participant))))

;; Function to check the status of a bid
(define-read-only (get-bid-details (bidder principal) (bid-amount uint))
  (match (map-get? auction-bids { bidder: bidder, amount: bid-amount })
    bid-data (ok { status: (get status bid-data), timestamp: (get timestamp bid-data), settlement-amount: (get settlement-amount bid-data) })
    ERR_AUCTION_NOT_FOUND))