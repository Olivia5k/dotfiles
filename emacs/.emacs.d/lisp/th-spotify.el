(use-package counsel-spotify
  :bind ("s-s" . spotify-hydra/body))

(defhydra spotify-hydra (:exit t :idle 1)
  "Spotify"
  ("s-s" counsel-spotify-toggle-play-pause "play/pause")
  ("p" counsel-spotify-next "next")
  ("n" counsel-spotify-previous "prev"))

(provide 'th-spotify)
