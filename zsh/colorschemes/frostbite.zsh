if [[ $TCOLORS = 256 ]] ; then
    c=()
    c+="239"       # 1.  Prompt decoration
    c+="81"        # 2.  Hostname
    c+="126"       # 3.  Hostname root
    c+="67"        # 4.  Directory
    c+="96"        # 5.  Directory non-multi
    c+="196"       # 6.  Error
    c+="74"        # 7.  Jobs
    c+="67"        # 8.  Clock
    c+="195"       # 9.  Mail
    c+="62"        # 10. Battery percent
    c+="63"        # 11. Battery timeleft
    c+="196"       # 12. TMOUT
    c+="66"        # 13. CVS System
    c+="123"       # 14. CVS Repo
    c+="114"       # 15. CVS Clean
    c+="161"       # 16. CVS Unstaged
    c+="95"        # 17. CVS Action
    c+="36"        # 18. Debug
    c+="120"       # 19. Enabled / Success
    c+="240"       # 20. Disabled
    c+="67"        # 21. Doc: Function
    c+="240"       # 22. Doc: Undocumented
    c+="182"       # 23. Doc: File
    c+="48"        # 24. Comp: Descriptions
    c+="67"        # 25. Comp: Directories
    c+="196"       # 26. Comp: Corrections
    c+="45"        # 27. IP Adress

    export c
else
    _zerror "This colorscheme requires a terminal that supports 256 colors."
    _zerror $TCOLORS
fi
