XPTemplate priority=personal

XPT maybeFunction hidden
(`$SParg^`function...{{^
    function(`e?^) {
        `cursor^
    }
`}}^`$SParg^)

XPT _funExp hidden
`function...{{^function(`e?^) {
    `cursor^
}`}}^
..XPT

XPT _ev_fun hidden

    function(`$SParg^`e^`$SParg^) {
        `^
    }

..XPT

XPT attr " attr\(..
attr`:optionalVal:^

XPT t " \$\(this)
$(this)

XPT d " debug\()
debug(`^);

XPT es " e.stopPropagation\();
`e^.stopPropagation();`^
