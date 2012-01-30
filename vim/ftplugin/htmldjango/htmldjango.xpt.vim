XPTemplate priority=personal

XPT t " trans
{% trans "`trans^" %}
XPT c " clear
<div class="clear"></div>

XPT spr " Mancx CSS sprite
XSET sprite=Choose(["email", "facebook", "twitter", "linkedin", "refer", "check", "red-exclamation", "yellow-exclamation", "avatar", "speech", "trash", "arrow", "question", "edit", "eye", "close", "debug", "clock", "bubble-green", "bubble-gray", "bubble-yellow", "bubble-red", "bubble-red-x", "office", "profilematch", "networkmatch", "profilematch", "networkmatch", "sign-minus", "sign-plus"])
XSET kind=Choose(['normal', 'hover', 'active'])
XSET align=Choose(['left', 'right'])
<div class="spr-20 spr-`sprite^ spr-`kind^ `align^">
    <div></div>
    <`span^>
        `cursor^
    </`span^>
</div>
