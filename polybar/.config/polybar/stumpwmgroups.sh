echo "
(apply #'concatenate 'string (mapcar
                              (lambda (g)
                                (let* ((name (string-upcase (group-name g)))
                                       (display-text (cond ((string-equal name \"SOCIAL\" ) \"   SOCIAL  \")
                                                           ((string-equal name \"SLACK\") \"   SLACK  \")
                                                           (t (concat \"   \" name \"  \")))))
                                  (if (eq g (current-group))
                                      (concat \"%{F#fff B#dda47dd0 u#3C3246 +u}\" display-text \"%{F- B- u- -u}\")
                                      (concat \"%{F#aaa}\" display-text \"%{F-}\"))))
                              (sort (screen-groups (current-screen)) #'< :key #'group-number)))" | stumpish -e eval
