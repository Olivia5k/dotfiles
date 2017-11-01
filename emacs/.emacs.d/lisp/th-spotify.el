(use-package counsel-spotify
  :bind ("s-s" . spotify-hydra/body))

(defhydra spotify-hydra (:exit t)
  "Spotify"
  ("s-s" counsel-spotify-toggle-play-pause "play/pause")
  ("n" counsel-spotify-next "next")
  ("p" counsel-spotify-previous "prev"))

(provide 'th-spotify)
