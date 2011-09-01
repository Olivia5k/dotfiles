# USB, yeah
alias mmu='mount /mnt/usb'
alias muu='umount /mnt/usb'

# Warez
alias mmw='sshfs nl:/warez ~/ssh/warez -o allow_other'
alias muw='fusermount -u ~/ssh/warez'

# sshfs home
alias mmn='sshfs nl: ~/ssh/ninjaloot'
alias mun='fusermount -u ~/ssh/ninjaloot'

# Force unmounting
alias muf='sudo umount -l /mnt/warez && sudo umount -l ~/ssh/ninjaloot'
