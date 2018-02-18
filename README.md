# discord-emacs
Discord rich presence for emacs.

# Install

```elisp
(load-file "~/path/to/discord-emacs/discord-emacs.el")
```
or use a Quelpa recipe

```elisp
(discord-ipc :fetcher github :repo "nitros12/discord-ipc.el")
```

# Use

```elisp
(discord-ipc-run "your-app-id")
```
Where the app id is of a rich presence enabled app.

I have put together an app with a few assets for c, haskell, hy, python and racket. Make an issue if you want some more added.
```
"384815451978334208"
```
