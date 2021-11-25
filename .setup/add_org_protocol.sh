cat > "${HOME}/.local/share/applications/org-protocol.desktop" << EOF
[Desktop Entry]
Name=org-protocol
Exec=emacsclient %u
Type=Application
Terminal=false
Categories=System;
MimeType=x-scheme-handler/org-protocol;
EOF

update-desktop-database ~/.local/share/applications/
