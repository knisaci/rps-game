;; Rock Paper Scissors - 1 Player vs Contract
;; 0 = Rock, 1 = Paper, 2 = Scissors

(define-map scores principal { wins: uint, losses: uint, draws: uint })

(define-private (get-contract-move)
  (mod block-height u3)
)

(define-private (get-result (player-move uint) (contract-move uint))
  (if (is-eq player-move contract-move)
    u0
    (if (or
          (and (is-eq player-move u0) (is-eq contract-move u2))
          (and (is-eq player-move u1) (is-eq contract-move u0))
          (and (is-eq player-move u2) (is-eq contract-move u1))
        )
      u1
      u2
    )
  )
)

(define-private (update-score (result uint))
  (let ((current (default-to { wins: u0, losses: u0, draws: u0 } (map-get? scores tx-sender))))
    (if (is-eq result u1)
      (map-set scores tx-sender (merge current { wins: (+ (get wins current) u1) }))
      (if (is-eq result u2)
        (map-set scores tx-sender (merge current { losses: (+ (get losses current) u1) }))
        (map-set scores tx-sender (merge current { draws: (+ (get draws current) u1) }))
      )
    )
  )
)

(define-public (play (player-move uint))
  (begin
    (asserts! (< player-move u3) (err u1))
    (let (
      (contract-move (get-contract-move))
      (result (get-result player-move contract-move))
    )
      (update-score result)
      (ok { player: player-move, contract: contract-move, result: result })
    )
  )
)

(define-read-only (get-score (player principal))
  (default-to { wins: u0, losses: u0, draws: u0 } (map-get? scores player))
)
