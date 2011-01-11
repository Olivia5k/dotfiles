# USB, yeah
alias mmu='mount /mnt/usb'
alias muu='umount /mnt/usb'

# Warez
alias mmw='sshfs ninjaloot.se:/warez /mnt/warez -o allow_other'
alias mmwl='sshfs 10.0.0.6:/warez /mnt/warez -o allow_other'
alias muw='fusermount -u /mnt/warez'

# sshfs home
alias mmn='sshfs ninjaloot.se: ~/ssh/ninjaloot'
alias mmnl='sshfs 10.0.0.6: ~/ssh/ninjaloot'
alias mun='fusermount -u ~/ssh/ninjaloot'

# Force unmounting
alias muf='sudo umount -l /mnt/warez && sudo umount -l ~/ssh/ninjaloot'
