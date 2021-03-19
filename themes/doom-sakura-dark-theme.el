(defgroup doom-sakura-dark-theme nil
  "Options for doom themes."
  :group 'doom-themes)

(def-doom-theme 
  doom-sakura-dark
  "A soothing dark theme, tranquil as a warm evening breeze."
  
  ;;   name              default       256         16
  (   (bg              '("#2a2a2a"     "#2a2a2a"   "black"))
      (bg-alt          '("#363636"     "#363636"   "black"))
      (base0           '("#414141"     "#414141"   "black"))
      (base1           '("#363636"     "#363636"   nil))
      (base2           '("#c9678d"     "#c9678d"   nil))
      (base3           '("#414141"     "#414141"   nil))
      (base4           '("#c9678d"     "#c9678d"   nil))
      (base5           '("#414141"     "#414141"   nil))
      (base6           '("#bf9b9f"     "#bf9b9f"   nil))
      (base7           '("#414141"     "#414141"   nil))
      (base8           '("#363636"     "#363636"   nil))
      (fg              '("#fbf7ef"     "#fbf7ef"   nil))
      (fg-alt          '("#fbf7ef"     "#fbf7ef"   nil))

      (grey base5)
      (red             '("#BE3445"     "#BE3445"   nil))
      (orange          '("#D36745"     "#D36745"   nil))     
      (green           '("#768E42"     "#768E42"   nil))
      (yellow          '("#E1B967"     "#E1B967"   nil))
      (magenta         '("#CE67CF"     "#CE67CF"   nil))

      (teal            '("#29838D"     "#29838D"   nil))
      (blue            '("#3B6EA8"     "#3B6EA8"   nil))
      (dark-blue       '("#5272AF"     "#5272AF"   nil))
      (violet          '("#842879"     "#842879"   nil))
      (cyan            '("#398EAC"     "#398EAC"   nil))
      (dark-cyan       '("#2C7088"     "#2C7088"   nil))
      (iosvkem-bg      '("#1b1d1e"     "#1b1d1e"   nil))
      (iosvkem-bga     '("#262829"     "#262829"   nil))

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
   ((outline-1 &override)  :foreground fg :weight 'normal)
   ((outline-2 &override)  :foreground fg :weight 'normal)
   ((outline-3 &override)  :foreground fg :weight 'normal)
   ((outline-4 &override)  :foreground fg :weight 'normal)
   ((outline-5 &override)  :foreground fg :weight 'normal)
   ((outline-6 &override)  :foreground fg :weight 'normal)
   ((outline-7 &override)  :foreground fg :weight 'normal)
   ((outline-8 &override)  :foreground fg :weight 'normal)

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
