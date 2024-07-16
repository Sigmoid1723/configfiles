#!/bin/bash

current_hour=$(date +"%H")

if [[ $current_hour -ge 6 && $current_hour -lt 12 ]]; then
    # Replace this with your actual i3 command
    i3-msg "workspace 3; exec firefox"
    i3-msg "workspace 1; exec xdg-open ~/Downloads/Books/Programming/Rust/print.html.pdf "
    emacs
elif [[ $current_hour -ge 12 && $current_hour -lt 20 ]]; then
    # Replace this with your actual i3 command
    i3-msg "workspace 1; exec xdg-open ~/Downloads/Books/Code_Optimization/The\ Art\ of\ Writing\ Efficient\ Programs.pdf "
    i3-msg "workspace 3; exec firefox"
    emacs
elif [[ $current_hour -ge 20 && $current_hour -lt 24 || $current_hour -ge 0 && $current_hour -lt 6 ]]; then
    # Replace this with your actual i3 command
    i3-msg "workspace 3; exec firefox"
    i3-msg "workspace 1; exec xdg-open ~/Downloads/Books/AI/modern/Artificial\ Intelligence_Modern\ approch.pdf "
    emacs
fi
