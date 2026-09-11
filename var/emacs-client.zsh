#!/bin/zsh
# launchd owns the daemon; this launcher must never create a competing daemon.
client=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
service=gui/$(id -u)/gnu.emacs.daemon
if ! "$client" --alternate-editor=false --eval t >/dev/null 2>&1; then
  if ! /bin/launchctl kickstart "$service"; then
    print -u2 "Kunne ikke starte Emacs-tjenesten: $service"
    exit 1
  fi
fi
for attempt in {1..120}; do
  if "$client" --alternate-editor=false --eval t >/dev/null 2>&1; then
    exec "$client" --alternate-editor=false --create-frame --no-wait "$@"
  fi
  /bin/sleep 0.25
done
print -u2 "Emacs svarede ikke inden 30 sekunder. Se EmacsDaemon.error.log."
exit 1
