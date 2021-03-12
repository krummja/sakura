(defgroup doom-sakura-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-sakura-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-sakura-light-theme
  :type 'boolean)

(defcustom doom-sakura-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-sakura-light-theme
  :type 'boolean)

(defcustom doom-sakura-comment-bg doom-sakura-brighter-comments
  "If non-nil, comments will have a subtler, darker background, enhancing legibility."
  :group 'doom-sakura-light-theme
  :type 'boolean)

;; (def-doom-theme NAME DOCSTRING DEFS &optional EXTRA-FACES EXTRA-VARS)
(def-doom-theme 
  doom-sakura-light
  "A pleasant light theme, soft as a cherry blossom."

  ;; name           default     256         16
  ( (bg           '("#FBF7EF"   "#FBF7EF"   "white"))
    (bg-alt       '("#E2D8F5"   "#E2D8F5"   "white"))
    (base0        '("#363636"   "#363636"   "black"))
    (base1        '("#414141"   "#414141"   nil))
    (base2        '("#BF9B9F"   "#BF9B9F"   nil))
    (base3        '("#EBE6EA"   "#EBE6EA"   nil)) ;; block highlights
    (base4        '("#C9678D"   "#C9678D"   nil))
    (base5        '("#ECA7D5"   "#ECA7D5"   nil))
    (base6        '("#C9678D"   "#C9678D"   nil))
    (base7        '("#E7CEEE"   "#E7CEEE"   nil))
    (base8        '("#E2D8F5"   "#E2D8F5"   nil))
    (fg           '("#2A2A2A"   "#2A2A2A"   nil))
    (fg-alt       '("#2A2A2A"   "#2A2A2A"   nil))

    (grey base6)
    (red          '("#BE3445"   "#BE3445"   nil))
    (orange       '("#D36745"   "#D36745"   nil))     
    (green        '("#768E42"   "#768E42"   nil))
    (yellow       '("#C38913"   "#C38913"   nil))
    (magenta      '("#CE67CF"   "#CE67CF"   nil))

    (teal         '("#29838D"   "#29838D"   nil))
    (blue         '("#3B6EA8"   "#3B6EA8"   nil))
    (dark-blue    '("#5272AF"   "#5272AF"   nil))
    (violet       '("#842879"   "#842879"   nil))
    (cyan         '("#398EAC"   "#398EAC"   nil))
    (dark-cyan    '("#2C7088"   "#2C7088"   nil))
    (iosvkem-bg   '("#1b1d1e"   "#1b1d1e"   nil))
    (iosvkem-bga  '("#262829"   "#262829"   nil))
    ;; I can add arbitrarily many more definitions :o

    ;; face categories -- required for all themes
    (highlight          (doom-blend red bg 0.95))
    (vertical-bar       (doom-darken bg 0.15))
    (selection          (doom-blend base5 bg 0.75))
    (builtin            teal)
    (comments           (doom-darken base5 0.45))
    (doc-comments       (doom-darken base5 0.45))
    (constants          magenta)
    (functions          teal)
    (keywords           blue)
    (methods            teal)
    (operators          blue)
    (type               yellow)
    (strings            green)
    (variables          violet)
    (numbers            magenta)
    (region             `(,(doom-lighten (car base5) 0.15) ,@(doom-lighten (cdr base0) 0.15)))
    (error              red)
    (warning            yellow)
    (success            green)
    (vc-modified        orange)
    (vc-added           green)
    (vc-deleted         red)
    (hl-line            (doom-blend base5 bg 0.5))
    (cursor-color       '("#000000"))
    (+evil--default-cursor-color '("#000000"))
    (modeline-fg        nil))

  ;; EXTRA-FACES
  (
   ((outline-1 &override)  :foreground base0 :weight 'normal)
   ((outline-2 &override)  :foreground base0 :weight 'normal)
   ((outline-3 &override)  :foreground base0 :weight 'normal)
   ((outline-4 &override)  :foreground base0 :weight 'normal)
   ((outline-5 &override)  :foreground base0 :weight 'normal)
   ((outline-6 &override)  :foreground base0 :weight 'normal)
   ((outline-7 &override)  :foreground base0 :weight 'normal)
   ((outline-8 &override)  :foreground base0 :weight 'normal)

   ((org-block &override) 
    :inherit 'fixed-pitch)

   ((org-verbatim &override) 
    :foreground red 
    :background nil 
    :weight 'normal)

   ((org-table &override) 
    :background bg)

   ((org-formula &override) 
    :background bg)

   ((org-ref-cite-face &override) 
    :foreground red)

   ((org-drawer &override) 
    :foreground "#9F9F9F")

   ((org-block-begin-line &override) 
    :foreground base2 
    :background nil 
    :underline fg)

   ((org-block-end-line &override) 
    :foreground base2 
    :background nil 
    :underline nil 
    :overline fg)

   ((org-document-info-keyword &override) 
    :foreground base2)

   ((ivy-current-match &override) 
    :foreground fg)
   
   ((org-link &override)
    :foreground red
    :background nil
    :underline red)
   ) 

  ;; EXTRA-VARS
  ())
