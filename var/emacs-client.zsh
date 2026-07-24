#!/bin/zsh

client=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
service=gui/$(id -u)/gnu.emacs.daemon

# Ask launchd to start the configured daemon if it is not already running.
/bin/launchctl kickstart "$service" >/dev/null 2>&1 || true

# Give a daemon that is still loading its initialization a short head start.
for attempt in {1..20}; do
  if "$client" --eval t >/dev/null 2>&1; then
    exec "$client" --create-frame --no-wait
  fi
  /bin/sleep 0.25
done

# Final fallback: emacsclient starts a daemon itself when the alternate editor
# is the empty string, and then creates the requested graphical frame.
exec "$client" --create-frame --no-wait --alternate-editor=""
