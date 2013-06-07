#lang racket
(require louDBus/unsafe)
(provide
         
         script-fu-round-corners
         file-xjt-load
         gimp-gradient-segment-set-right-color
         plug-in-emboss
         file-ps-save
         file-png-get-defaults
         gimp-layer-is-floating-sel
         gimp-layer-resize
         plug-in-colormap-remap
         gimp-pencil
         file-tiff-save2
         gimp-item-get-linked
         gimp-text-layer-set-kerning
         gimp-vectors-new-from-text-layer
         gimp-gimprc-set
         plug-in-vpropagate
         plug-in-smooth-palette
         gimp-selection-bounds
         script-fu-speed-text
         plug-in-softglow
         gimp-image-floating-sel-attached-to
         gimp-context-get-sample-merged
         gimp-context-set-sample-criterion
         python-fu-palette-offset
         file-png-load
         gimp-context-get-sample-threshold
         gimp-help-2-using-photography
         gimp-selection-all
         gimp-image-get-resolution
         plug-in-curve-bend
         file-raw-save
         gimp-context-set-default-colors
         gimp-context-set-feather
         gimp-layer-flatten
         gimp-item-detach-parasite
         gimp-drawable-free-shadow
         gimp-drawable-type-with-alpha
         gimp-edit-named-copy
         plug-in-despeckle
         file-faxg3-load
         file-gih-save
         gimp-image-free-shadow
         gimp-drawable-transform-scale
         gimp-procedural-db-proc-val
         plug-in-zealouscrop
         gimp-brush-get-spacing
         gimp-edit-named-cut
         gimp-image-get-layer-by-name
         gimp-help-2-concepts-usage
         gimp-airbrush
         file-openraster-load-thumb
         gimp-drawable-update
         gimp-patterns-set-popup
         gimp-image-get-vectors
         plug-in-laplace
         gimp-layer-from-mask
         gimp-progress-uninstall
         gimp-item-get-visible
         gimp-edit-fill
         gimp-image-lower-item-to-bottom
         script-fu-blend-anim
         plug-in-sample-colorize
         plug-in-shift
         gimp-dodgeburn
         file-pgm-save
         gimp-edit-named-paste
         script-fu-refresh
         gimp-vectors-export-to-string
         gimp-context-pop
         gimp-vectors-bezier-stroke-conicto
         gimp-floating-sel-to-layer
         script-fu-title-header
         plug-in-color-enhance
         plug-in-displace
         file-xmc-save
         gimp-text-layer-get-hinting
         gimp-shear
         gimp-brush-get-info
         gimp-vectors-import-from-string
         gimp-get-color-configuration
         gimp-text-get-extents
         gimp-paintbrush
         plug-in-decompose
         gimp-item-is-vectors
         gimp-palette-add-entry
         gimp-help-2-using-simpleobjects
         script-fu-spyrogimp
         script-fu-textured-logo
         gimp-image-grid-set-background-color
         gimp-path-set-locked
         file-pix-save
         gimp-palettes-close-popup
         plug-in-waves
         script-fu-3d-outline-logo-alpha
         file-bz2-load
         gimp-channel-copy
         gimp-context-set-sample-transparent
         gimp-brush-get-angle
         gimp-layer-new-from-drawable
         gimp-edit-paste
         gimp-text-layer-set-line-spacing
         plug-in-spheredesigner
         gimp-palette-rename
         gimp-gradient-segment-get-middle-pos
         gimp-context-get-opacity
         gimp-buffer-rename
         gimp-vectors-import-from-file
         plug-in-rgb-noise
         gimp-palettes-get-palette-entry
         gimp-procedural-db-proc-arg
         gimp-image-add-vectors
         file-fli-save
         gimp-item-transform-2d
         python-fu-console
         gimp-brush-duplicate
         file-ico-load-thumb
         gimp-image-get-item-position
         gimp-context-set-font
         gimp-edit-copy-visible
         gimp-image-unset-active-channel
         script-fu-textured-logo-alpha
         gimp-online-developer-web-site
         gimp-unit-get-abbreviation
         file-pcx-load
         gimp-smudge
         plug-in-the-old-egg
         gimp-context-set-brush-aspect-ratio
         plug-in-edge
         gimp-drawable-transform-shear
         gimp-layer-get-opacity
         script-fu-glowing-logo
         gimp-text-layer-get-letter-spacing
         gimp-palettes-get-palette
         gimp-context-set-dynamics
         gimp-text-layer-get-color
         file-tiff-load
         plug-in-sparkle
         script-fu-sota-chrome-it
         python-fu-gradient-save-as-css
         gimp-layer-get-apply-mask
         gimp-channel-set-show-masked
         gimp-context-get-ink-blob-angle
         script-fu-comic-logo
         gimp-context-get-transform-recursion
         script-fu-chrome-logo-alpha
         gimp-image-get-active-vectors
         gimp-item-get-name
         gimp-progress-get-window-handle
         gimp-image-grid-set-offset
         gimp-vectors-export-to-file
         script-fu-newsprint-text
         gimp-image-attach-parasite
         gimp-flip
         python-fu-palette-to-gradient-repeating
         gimp-context-get-sample-transparent
         gimp-palette-new
         gimp-convolve-default
         script-fu-labels-gimp-org
         plug-in-align-layers
         gimp-edit-clear
         gimp-image-set-component-active
         gimp-context-set-antialias
         gimp-text-layer-get-kerning
         gimp-gradient-segment-range-set-blending-function
         gimp-procedural-db-proc-exists
         gimp-palette-delete
         script-fu-alien-glow-right-arrow
         plug-in-wind
         file-uri-save
         gimp-image-get-component-visible
         gimp-buffer-delete
         gimp-path-get-points
         script-fu-grid-system
         script-fu-slide
         gimp-get-monitor-resolution
         gimp-context-swap-colors
         gimp-gradient-segment-set-right-pos
         plug-in-iwarp
         gimp-image-select-round-rectangle
         file-header-save
         gimp-help-concepts-paths
         gimp-image-pick-correlate-layer
         gimp-context-get-brush-aspect-ratio
         script-fu-selection-to-image
         file-ppm-save
         script-fu-basic1-logo-alpha
         gimp-text-layer-new
         gimp-context-get-sample-criterion
         plug-in-photocopy
         gimp-image-get-colormap
         file-bmp-save
         plug-in-colormap-swap
         script-fu-tile-blur
         gimp-path-get-current
         gimp-fuzzy-select-full
         gimp-edit-paste-as-new
         gimp-item-transform-rotate-simple
         plug-in-autocrop-layer
         gimp-attach-parasite
         gimp-selection-combine
         gimp-context-get-brush-size
         file-xbm-save
         gimp-path-get-tattoo
         gimp-gradient-rename
         gimp-buffer-get-bytes
         gimp-vectors-bezier-stroke-cubicto
         gimp-item-get-tattoo
         gimp-item-is-layer
         gimp-text-layer-set-letter-spacing
         gimp-image-flip
         plug-in-pixelize2
         file-gif-save
         plug-in-metadata-set-simple
         gimp-item-transform-rotate
         gimp-context-set-ink-blob-angle
         gimp-gradients-sample-uniform
         file-cel-load
         script-fu-fuzzy-border
         gimp-image-set-active-vectors
         gimp-plugin-set-pdb-error-handler
         gimp-help-using-photography
         gimp-brush-set-spikes
         script-fu-gradient-example
         script-fu-starscape-logo
         script-fu-alien-glow-logo-alpha
         file-sunras-load
         gimp-gradients-get-gradient-data
         gimp-palette-set-columns
         script-fu-difference-clouds
         gimp-heal-default
         gimp-gradient-get-number-of-segments
         plug-in-drawable-compose
         script-fu-beveled-pattern-button
         gimp-context-set-paint-mode
         script-fu-blended-logo
         gimp-procedural-db-proc-info
         gimp-display-is-valid
         gimp-item-get-parent
         plug-in-recompose
         gimp-clone
         gimp-selection-grow
         gimp-image-set-colormap
         gimp-gradient-segment-get-right-pos
         gimp-layer-set-show-mask
         script-fu-tube-button-label-gimp-org
         plug-in-papertile
         gimp-context-list-paint-methods
         script-fu-flatland
         gimp-image-raise-item
         script-fu-t-o-p-logo-alpha
         gimp-image-get-layer-by-tattoo
         plug-in-sel-gauss
         plug-in-ifscompose
         gimp-progress-end
         gimp-image-remove-channel
         gimp-detach-parasite
         gimp-help-using-docks
         gimp-selection-sharpen
         gimp-image-get-unit
         plug-in-autostretch-hsv
         plug-in-icc-profile-apply-rgb
         gimp-image-get-guide-orientation
         gimp-layer-new-from-visible
         plug-in-ccanalyze
         gimp-gradient-delete
         gimp-layer-scale-full
         plug-in-tile
         plug-in-unit-editor
         gimp-channel-set-opacity
         gimp-image-clean-all
         gimp-selection-is-empty
         file-xjt-save
         gimp-image-remove-layer
         gimp-vectors-stroke-scale
         gimp-context-get-ink-size
         plug-in-rotate-colormap
         gimp-image-grid-get-spacing
         gimp-gradient-segment-range-redistribute-handles
         gimp-item-set-linked
         extension-gimp-help-browser
         gimp-image-get-xcf-uri
         gimp-vectors-bezier-stroke-new-ellipse
         gimp-dynamics-get-list
         gimp-drawable-transform-shear-default
         gimp-fonts-set-popup
         script-fu-copy-visible
         gimp-get-parasite-list
         gimp-image-convert-set-dither-matrix
         file-csource-save
         gimp-context-push
         gimp-image-scale-full
         gimp-image-select-rectangle
         file-png-save
         gimp-by-color-select-full
         gimp-free-select
         gimp-context-set-sample-threshold-int
         plug-in-palettemap
         gimp-gradient-segment-set-left-pos
         gimp-gradient-segment-range-delete
         gimp-drawable-transform-2d
         plug-in-threshold-alpha
         gimp-register-magic-load-handler
         gimp-channel-get-color
         gimp-context-get-pattern
         gimp-drawable-merge-shadow
         gimp-unit-get-identifier
         script-fu-add-bevel
         plug-in-gflare
         plug-in-compose
         gimp-context-set-ink-speed-sensitivity
         script-fu-line-nova
         script-fu-spinning-globe
         gimp-image-thumbnail
         script-fu-font-map
         gimp-layer-get-show-mask
         gimp-palette-is-editable
         script-fu-weave
         gimp-gradients-refresh
         gimp-brushes-set-spacing
         gimp-levels-auto
         gimp-help-using-simpleobjects
         gimp-image-insert-channel
         plug-in-qbist
         gimp-drawable-transform-flip
         gimp-help-using-selections
         plug-in-sharpen
         gimp-context-get-feather-radius
         gimp-convolve
         gimp-text-layer-set-hint-style
         gimp-context-set-ink-size
         gimp-image-get-guide-position
         file-gbr-load
         gimp-unit-get-number-of-units
         plug-in-metadata-import
         file-gtm-save
         gimp-image-undo-enable
         gimp-image-delete-guide
         gimp-item-attach-parasite
         gimp-unit-get-digits
         gimp-get-default-comment
         gimp-path-stroke-current
         gimp-text-layer-set-justification
         gimp-gradient-segment-range-split-midpoint
         plug-in-engrave
         file-svg-load
         gimp-text-layer-get-line-spacing
         script-fu-alien-glow-bullet
         plug-in-cml-explorer
         plug-in-animationoptimize
         gimp-gradient-segment-range-set-coloring-type
         gimp-selection-value
         gimp-text-layer-set-font
         gimp-brush-set-hardness
         gimp-brush-set-angle
         gimp-item-transform-matrix
         file-sgi-load
         script-fu-carved-logo
         script-fu-bovinated-logo
         gimp-drawable-is-rgb
         gimp-layer-remove-mask
         gimp-image-undo-group-end
         gimp-layer-set-mode
         gimp-image-grid-set-style
         gimp-image-get-selection
         gimp-xcf-load
         gimp-context-set-brush
         gimp-image-resize-to-layers
         plug-in-metadata-get
         gimp-context-get-paint-method
         plug-in-script-fu-text-console
         gimp-layer-set-lock-alpha
         gimp-palette-export-text
         file-bz2-save
         gimp-brush-get-radius
         gimp-item-transform-perspective
         script-fu-perspective-shadow
         gimp-image-get-imported-uri
         gimp-image-get-vectors-by-tattoo
         gimp-image-height
         gimp-image-undo-thaw
         gimp-text-layer-get-markup
         gimp-image-is-valid
         gimp-image-convert-grayscale
         gimp-palette-entry-get-color
         plug-in-mblur-inward
         gimp-help-concepts-usage
         plug-in-lens-distortion
         script-fu-bovinated-logo-alpha
         file-fits-load
         gimp-image-merge-down
         script-fu-coffee-stain
         extension-gimp-help
         gimp-register-load-handler
         gimp-drawable-transform-perspective
         gimp-vectors-stroke-new-from-points
         script-fu-addborder
         gimp-text-layer-get-base-direction
         plug-in-map-object
         gimp-brush-get-shape
         gimp-context-set-ink-blob-type
         gimp-image-lower-item
         plug-in-flame
         plug-in-apply-canvas
         gimp-image-get-layers
         gimp-equalize
         python-fu-foggify
         gimp-drawable-set-image
         file-pcx-save
         gimp-display-get-window-handle
         gimp-text-layer-get-justification
         gimp-get-default-unit
         gimp-context-set-feather-radius
         script-fu-predator
         gimp-rect-select
         gimp-displays-flush
         plug-in-convmatrix
         gimp-posterize
         gimp-context-set-defaults
         file-tiff-save
         gimp-progress-cancel
         script-fu-circuit
         plug-in-animationunoptimize
         gimp-floating-sel-attach
         plug-in-c-astretch
         script-fu-paste-as-pattern
         gimp-channel-get-show-masked
         plug-in-unsharp-mask
         gimp-context-get-ink-blob-type
         gimp-plugin-domain-register
         gimp-layer-scale
         python-fu-eval
         gimp-get-module-load-inhibit
         plug-in-semiflatten
         script-fu-basic1-logo
         gimp-progress-install
         file-xwd-load
         script-fu-selection-round
         gimp-context-get-ink-blob-aspect-ratio
         file-pat-load
         script-fu-blended-logo-alpha
         gimp-vectors-copy
         gimp-path-set-points
         gimp-drawable-sub-thumbnail
         gimp-text
         gimp-progress-init
         plug-in-destripe
         script-fu-guide-new-percent
         gimp-item-is-channel
         gimp-patterns-popup
         file-mng-save
         gimp-drawable-is-indexed
         gimp-context-get-dynamics
         gimp-image-undo-is-enabled
         gimp-path-import
         gimp-smudge-default
         file-wmf-load-thumb
         gimp-brush-get-aspect-ratio
         gimp-progress-pulse
         gimp-context-set-transform-recursion
         plug-in-noisify
         gimp-text-get-extents-fontname
         gimp-text-layer-set-base-direction
         plug-in-metadata-export
         gimp-fonts-get-list
         gimp-brush-is-generated
         gimp-gradient-get-uniform-samples
         plug-in-neon
         plug-in-animationplay
         gimp-path-set-tattoo
         gimp-image-add-channel
         script-fu-swirly-pattern
         script-fu-tube-subsubbutton-label-gimp-org
         gimp-item-set-tattoo
         plug-in-mail-image
         gimp-drawable-transform-matrix-default
         gimp-image-get-floating-sel
         plug-in-gimpressionist
         gimp-selection-invert
         script-fu-t-o-p-logo
         plug-in-sel2path
         gimp-file-load-layer
         gimp-brush-new
         gimp-context-set-paint-method
         script-fu-text-circle
         script-fu-alien-neon-logo
         gimp-buffer-get-height
         file-cel-save
         plug-in-sel2path-advanced
         plug-in-web-browser
         gimp-levels
         gimp-brushes-get-brush
         gimp-palettes-get-list
         gimp-threshold
         gimp-drawable-set-pixel
         file-sunras-save
         python-fu-slice
         plug-in-mblur
         gimp-text-layer-get-text
         gimp-item-get-children
         plug-in-script-fu-console
         gimp-context-get-paint-mode
         gimp-context-set-gradient
         gimp-procedural-db-get-data-size
         gimp-image-add-layer-mask
         gimp-curves-spline
         gimp-image-get-active-channel
         gimp-help-2-using-selections
         gimp-selection-feather
         gimp-colorize
         plug-in-animationoptimize-diff
         script-fu-beveled-pattern-heading
         file-xpm-load
         script-fu-3d-outline-logo
         gimp-item-transform-flip-simple
         gimp-help-2-using-fileformats
         gimp-layer-add-mask
         plug-in-gauss-rle
         plug-in-polar-coords
         script-fu-unsharp-mask
         gimp-item-transform-scale
         script-fu-alien-glow-logo
         gimp-edit-copy
         gimp-procedural-db-get-data
         python-fu-brush-from-text
         script-fu-carve-it
         gimp-palette-get-info
         gimp-palette-entry-get-name
         gimp-drawable-transform-flip-simple
         script-fu-make-brush-elliptical
         script-fu-make-brush-rectangular-feathered
         plug-in-gfig
         plug-in-pixelize
         gimp-drawable-transform-rotate-default
         gimp-image-select-polygon
         gimp-drawable-transform-rotate-simple
         gimp-drawable-bpp
         gimp-clone-default
         file-psd-load
         gimp-vectors-stroke-flip
         gimp-image-select-item
         plug-in-randomize-hurl
         gimp-by-color-select
         plug-in-glasstile
         gimp-item-get-image
         script-fu-chalk-logo-alpha
         gimp-temp-name
         python-fu-palette-to-gradient
         gimp-context-get-feather
         gimp-round-rect-select
         file-pdf-load
         script-fu-xach-effect
         script-fu-beveled-pattern-hrule
         gimp-vectors-stroke-get-points
         gimp-image-new
         file-psd-load-thumb
         gimp-gradient-is-editable
         plug-in-spread
         gimp-edit-named-copy-visible
         gimp-context-set-background
         gimp-item-is-valid
         gimp-online-docs-web-site
         file-png-save-defaults
         gimp-unit-get-factor
         gimp-context-set-interpolation
         gimp-patterns-close-popup
         script-fu-gradient-bevel-logo-alpha
         gimp-plugin-icon-register
         gimp-gradients-set-popup
         gimp-get-parasite
         gimp-desaturate-full
         gimp-unit-set-deletion-flag
         gimp-unit-get-plural
         file-ico-load
         plug-in-film
         gimp-image-raise-item-to-top
         gimp-layer-translate
         gimp-unit-get-symbol
         gimp-selection-load
         gimp-gradients-sample-custom
         gimp-brush-set-aspect-ratio
         gimp-image-resize
         gimp-path-to-selection
         gimp-palette-export-php
         gimp-path-delete
         script-fu-make-brush-rectangular
         gimp-image-list
         gimp-edit-bucket-fill-full
         gimp-vectors-stroke-get-point-at-dist
         gimp-image-set-active-channel
         script-fu-guide-new
         gimp-image-get-parasite
         plug-in-antialias
         gimp-airbrush-default
         gimp-gradient-segment-set-left-color
         gimp-heal
         gimp-item-delete
         gimp-drawable-thumbnail
         script-fu-3dtruchet
         file-aa-save
         gimp-selection-shrink
         gimp-drawable-foreground-extract
         plug-in-colors-channel-mixer
         gimp-text-layer-get-hint-style
         gimp-image-undo-freeze
         gimp-palettes-refresh
         gimp-image-duplicate
         gimp-layer-get-edit-mask
         gimp-image-get-channels
         python-fu-palette-sort
         file-gbr-save
         script-fu-distress-selection
         plug-in-make-seamless
         gimp-perspective
         gimp-progress-update
         gimp-context-set-palette
         plug-in-ripple
         gimp-help-2-using-web
         plug-in-fractal-trace
         gimp-gradient-segment-get-left-pos
         gimp-fuzzy-select
         script-fu-chalk-logo
         file-fli-info
         gimp-drawable-fill
         gimp-gradient-segment-set-middle-pos
         gimp-context-get-sample-threshold-int
         gimp-item-transform-shear
         gimp-context-get-interpolation
         script-fu-selection-rounded-rectangle
         gimp-floating-sel-relax
         gimp-image-get-channel-by-name
         gimp-text-layer-set-font-size
         plug-in-bump-map
         gimp-fonts-popup
         plug-in-icc-profile-info
         file-sgi-save
         gimp-layer-get-lock-alpha
         gimp-edit-named-paste-as-new
         gimp-brush-set-radius
         gimp-vectors-stroke-translate
         gimp-xcf-save
         gimp-selection-layer-alpha
         gimp-palette-entry-set-color
         gimp-image-get-vectors-by-name
         gimp-color-balance
         gimp-vectors-stroke-close
         plug-in-colorify
         script-fu-crystal-logo
         gimp-brush-set-shape
         script-fu-render-map
         gimp-selection-float
         file-psp-load
         gimp-context-set-transform-direction
         file-dicom-load
         gimp-fonts-close-popup
         file-fits-save
         gimp-item-set-name
         gimp-palette-get-colors
         file-pnm-load
         plug-in-depth-merge
         gimp-drawable-mask-intersect
         file-print-gtk
         gimp-item-transform-flip
         gimp-image-is-dirty
         gimp-layer-add-alpha
         plug-in-curve-bend-Iterator
         gimp-patterns-get-list
         gimp-brush-is-editable
         gimp-brush-get-hardness
         gimp-drawable-mask-bounds
         script-fu-button00
         plug-in-whirl-pinch
         gimp-brushes-get-brush-data
         script-fu-selection-to-pattern
         gimp-vectors-stroke-rotate
         gimp-gradient-segment-range-blend-colors
         plug-in-randomize-slur
         plug-in-colortoalpha
         plug-in-sinus
         file-svg-load-thumb
         gimp-image-add-vguide
         gimp-transform-2d
         gimp-file-load-layers
         gimp-edit-stroke
         gimp-context-set-foreground
         gimp-floating-sel-rigor
         script-fu-chrome-logo
         plug-in-gauss-iir
         gimp-palette-duplicate
         gimp-palette-delete-entry
         script-fu-beveled-pattern-arrow
         gimp-text-layer-get-font-size
         gimp-image-select-ellipse
         gimp-image-base-type
         gimp-file-load
         script-fu-neon-logo
         gimp-text-layer-get-indent
         gimp-desaturate
         gimp-vectors-bezier-stroke-new-moveto
         gimp-image-crop
         file-xwd-save
         gimp-drawable-is-gray
         file-pat-save
         gimp-path-get-point-at-dist
         gimp-image-undo-group-start
         gimp-gradient-new
         gimp-image-merge-visible-layers
         gimp-text-layer-set-hinting
         gimp-help-using-fileformats
         gimp-image-delete
         gimp-image-select-contiguous-color
         gimp-context-get-brush-angle
         gimp-image-set-tattoo-state
         gimp-online-plug-in-web-site
         gimp-image-get-channel-by-tattoo
         gimp-item-is-drawable
         gimp-item-is-group
         file-png-save2
         gimp-item-is-text-layer
         gimp-pattern-get-info
         file-gz-load
         gimp-image-undo-disable
         gimp-item-is-selection
         script-fu-i26-gunya2
         gimp-brush-get-pixels
         script-fu-comic-logo-alpha
         gimp-image-set-component-visible
         gimp-layer-get-mask
         script-fu-frosty-logo-alpha
         gimp-palette-export-python
         plug-in-flarefx
         gimp-drawable-offsets
         gimp-image-insert-vectors
         file-tga-load
         gimp-gimprc-query
         plug-in-gradmap
         gimp-image-add-hguide
         gimp-vectors-to-selection
         gimp-palette-get-columns
         gimp-vectors-remove-stroke
         gimp-context-set-sample-merged
         gimp-image-get-parasite-list
         gimp-context-set-ink-blob-aspect-ratio
         gimp-histogram
         file-pbm-save
         gimp-brushes-close-popup
         gimp-brushes-get-list
         gimp-text-layer-set-language
         gimp-dynamics-refresh
         gimp-context-get-font
         file-colorxhtml-save
         gimp-context-get-transform-resize
         gimp-image-add-layer
         gimp-layer-resize-to-image-size
         gimp-text-layer-set-color
         script-fu-chip-away-logo-alpha
         gimp-layer-create-mask
         script-fu-set-cmap
         gimp-palettes-set-popup
         plug-in-dbbrowser
         gimp-image-set-unit
         plug-in-sobel
         script-fu-swirl-tile
         plug-in-decompose-registered
         gimp-item-get-parasite
         gimp-image-select-color
         file-xpm-save
         RamServer
         script-fu-guides-remove
         gimp-image-convert-indexed
         plug-in-icc-profile-apply
         file-jpeg-load
         gimp-channel-get-opacity
         gimp-context-get-ink-angle
         gimp-help
         gimp-layer-set-offsets
         file-wmf-load
         gimp-buffers-get-list
         gimp-context-get-ink-tilt-sensitivity
         gimp-channel-combine-masks
         gimp-version
         gimp-image-flatten
         gimp-eraser-default
         gimp-plugin-get-pdb-error-handler
         plug-in-exchange
         plug-in-icc-profile-file-info
         gimp-context-set-brush-angle
         file-glob
         gimp-floating-sel-remove
         file-xmc-load-thumb
         gimp-patterns-refresh
         file-psd-save
         plug-in-blinds
         gimp-context-get-gradient
         gimp-image-get-filename
         script-fu-frosty-logo
         plug-in-nlfilt
         gimp-image-convert-rgb
         script-fu-land
         script-fu-guides-from-selection
         gimp-text-layer-get-antialias
         file-pdf-save
         gimp-vectors-new
         gimp-display-delete
         gimp-brush-rename
         gimp-context-get-background
         gimp-path-set-current
         gimp-image-grid-get-foreground-color
         file-openraster-load
         gimp-item-is-layer-mask
         gimp-edit-blend
         plug-in-oilify-enhanced
         file-gif-load-thumb
         plug-in-metadata-set
         gimp-brushes-get-spacing
         gimp-gradients-get-list
         plug-in-fractalexplorer
         plug-in-illusion
         gimp-selection-translate
         script-fu-round-button
         file-ico-save
         gimp-unit-get-number-of-built-in-units
         gimp-levels-stretch
         file-png-set-defaults
         gimp-item-get-lock-content
         gimp-selection-save
         gimp-vectors-stroke-flip-free
         script-fu-alien-glow-button
         gimp-palette-export-css
         gimp-register-thumbnail-loader
         gimp-context-set-ink-tilt-sensitivity
         plug-in-metadata-encode-xmp
         gimp-gradient-segment-get-left-color
         plug-in-blur
         gimp-eraser
         plug-in-retinex
         script-fu-tube-subbutton-label-gimp-org
         gimp-vectors-get-strokes
         file-pdf-load-thumb
         gimp-help-using-web
         gimp-image-set-filename
         plug-in-plug-in-details
         file-eps-load
         gimp-paintbrush-default
         gimp-context-set-brush-default-size
         plug-in-script-fu-eval
         gimp-patterns-get-pattern-data
         gimp-drawable-has-alpha
         plug-in-small-tiles
         file-ps-load
         gimp-image-grid-get-style
         plug-in-solid-noise
         gimp-context-get-brush
         gimp-brush-delete
         gimp-gradient-segment-get-blending-function
         gimp-brushes-popup
         script-fu-beveled-pattern-bullet
         gimp-image-find-next-guide
         gimp-image-width
         plug-in-icc-profile-set
         plug-in-plasma
         gimp-layer-group-new
         gimp-online-main-web-site
         gimp-gradient-get-custom-samples
         plug-in-applylens
         gimp-layer-set-edit-mask
         gimp-image-grid-set-spacing
         gimp-gradient-segment-range-split-uniform
         file-raw-load
         gimp-gradient-segment-range-flip
         gimp-image-set-resolution
         plug-in-max-rgb
         plug-in-dog
         file-gih-load
         plug-in-newsprint
         script-fu-basic2-logo
         gimp-plugin-menu-branch-register
         script-fu-cool-metal-logo-alpha
         gimp-message-set-handler
         gimp-image-get-active-layer
         plug-in-erode
         file-dicom-save
         gimp-message
         plug-in-autocrop
         file-pnm-save
         script-fu-glossy-logo-alpha
         gimp-channel-set-color
         gimp-vectors-bezier-stroke-lineto
         gimp-brushes-set-popup
         gimp-patterns-get-pattern
         gimp-brushes-refresh
         plug-in-pagecurl
         gimp-gradient-segment-range-replicate
         gimp-context-set-pattern
         plug-in-maze
         gimp-pattern-get-pixels
         gimp-selection-border
         plug-in-randomize-pick
         gimp-quit
         script-fu-paste-as-brush
         gimp-plugin-menu-register
         plug-in-hsv-noise
         plug-in-alienmap2
         script-fu-chip-away-logo
         gimp-drawable-transform-rotate
         gimp-invert
         gimp-image-scale
         gimp-gradients-close-popup
         gimp-context-get-foreground
         plug-in-diffraction
         file-xmc-load
         gimp-vectors-stroke-interpolate
         script-fu-selection-to-brush
         plug-in-gauss-iir2
         plug-in-scatter-hsv
         script-fu-basic2-logo-alpha
         script-fu-make-brush-elliptical-feathered
         file-desktop-link-load
         script-fu-lava
         gimp-item-set-lock-content
         gimp-text-layer-set-indent
         gimp-brightness-contrast
         gimp-register-save-handler
         gimp-image-get-tattoo-state
         gimp-file-save
         file-pix-load
         gimp-context-set-transform-resize
         plug-in-video
         gimp-dodgeburn-default
         script-fu-reverse-layers
         plug-in-gauss-rle2
         gimp-path-get-locked
         plug-in-nova
         gimp-image-pick-color
         plug-in-cartoon
         gimp-path-list
         gimp-text-fontname
         gimp-channel-new-from-component
         gimp-drawable-height
         gimp-image-detach-parasite
         gimp-context-get-ink-size-sensitivity
         file-fli-load
         gimp-fonts-refresh
         script-fu-big-header-gimp-org
         plug-in-rotate
         plug-in-deinterlace
         script-fu-alien-neon-logo-alpha
         gimp-unit-get-deletion-flag
         gimp-text-layer-get-font
         gimp-drawable-transform-perspective-default
         plug-in-grid
         gimp-layer-get-mode
         file-gz-save
         gimp-buffer-get-width
         plug-in-gauss
         gimp-help-2-concepts-paths
         script-fu-glowing-logo-alpha
         script-fu-small-header-gimp-org
         gimp-getpid
         gimp-edit-cut
         gimp-ellipse-select
         gimp-text-layer-set-text
         gimp-context-get-antialias
         file-tga-save
         gimp-image-get-uri
         plug-in-dilate
         gimp-context-set-sample-threshold
         gimp-palettes-popup
         gimp-drawable-get-pixel
         gimp-text-layer-resize
         extension-script-fu
         gimp-layer-set-apply-mask
         plug-in-imagemap
         gimp-drawable-type
         plug-in-borderaverage
         gimp-image-get-active-drawable
         gimp-procedural-db-set-data
         gimp-palette-export-java
         gimp-gradient-duplicate
         script-fu-neon-logo-alpha
         gimp-image-grid-get-background-color
         plug-in-cubism
         gimp-image-grid-get-offset
         gimp-register-file-handler-mime
         gimp-display-new
         gimp-palette-entry-set-name
         gimp-image-grid-set-foreground-color
         plug-in-script-fu-server
         file-ps-load-thumb
         gimp-image-remove-layer-mask
         script-fu-truchet
         gimp-rotate
         gimp-get-theme-dir
         gimp-context-set-ink-size-sensitivity
         gimp-image-set-active-layer
         script-fu-camo-pattern
         file-uri-load
         gimp-hue-saturation
         gimp-layer-new
         script-fu-gradient-bevel-logo
         plug-in-bump-map-tiled
         gimp-brush-set-spacing
         gimp-image-remove-vectors
         plug-in-displace-polar
         gimp-plugin-help-register
         gimp-item-set-visible
         gimp-file-save-thumbnail
         script-fu-drop-shadow
         script-fu-erase-rows
         gimp-gradient-segment-get-right-color
         file-jpeg-save
         gimp-plugins-query
         gimp-procedural-db-query
         gimp-text-layer-get-language
         file-bmp-load
         gimp-message-get-handler
         gimp-unit-get-singular
         plug-in-oilify
         file-ps-load-setargs
         gimp-drawable-transform-2d-default
         plug-in-mosaic
         gimp-item-get-parasite-list
         gimp-drawable-offset
         plug-in-filter-pack
         plug-in-guillotine
         plug-in-hot
         gimp-image-rotate
         file-xbm-load
         plug-in-warp
         gimp-buffer-get-image-type
         plug-in-screenshot
         gimp-context-set-brush-size
         gimp-gradient-segment-range-blend-opacity
         gimp-image-reorder-item
         plug-in-jigsaw
         gimp-drawable-width
         gimp-drawable-transform-matrix
         file-gif-load
         gimp-edit-stroke-vectors
         gimp-context-set-ink-angle
         script-fu-clothify
         gimp-image-get-component-active
         file-pdf-save-multi
         plug-in-metadata-get-simple
         script-fu-ripply-anim
         file-openraster-save
         gimp-displays-reconnect
         gimp-curves-explicit
         gimp-selection-none
         gimp-floating-sel-anchor
         script-fu-glossy-logo
         script-fu-sota-chrome-logo
         plug-in-vinvert
         gimp-brush-get-spikes
         gimp-context-set-opacity
         plug-in-the-slimy-egg
         plug-in-checkerboard
         gimp-image-get-name
         plug-in-lic
         gimp-channel-new
         gimp-procedural-db-dump
         gimp-image-get-exported-uri
         gimp-gradient-segment-range-move
         script-fu-old-photo
         gimp-vectors-stroke-get-length
         plug-in-metadata-decode-xmp
         gimp-drawable-transform-scale-default
         gimp-gradient-segment-get-coloring-type
         gimp-progress-set-text
         gimp-gradients-popup
         gimp-context-get-transform-direction
         gimp-unit-new
         script-fu-cool-metal-logo
         script-fu-waves-anim
         plug-in-icc-profile-set-rgb
         plug-in-red-eye-removal
         gimp-help-2-using-docks
         gimp-scale
         gimp-context-get-palette
         gimp-drawable-transform-flip-default
         gimp-text-layer-set-antialias
         plug-in-normalize
         gimp-layer-copy
         plug-in-lighting
         script-fu-burn-in-anim
         gimp-layer-set-opacity
         gimp-context-get-ink-speed-sensitivity
         gimp-image-insert-layer
         gimp-file-load-thumbnail
         gimp-procedural-db-temp-name
         gimp-get-path-by-tattoo
         file-eps-save
         gimp-edit-bucket-fill
         script-fu-alien-glow-horizontal-ruler
         gimp-channel-get-name
         gimp-gradients-get-active
         gimp-channel-set-tattoo
         gimp-palette-get-foreground
         gimp-image-get-cmap
         gimp-layer-set-linked
         gimp-drawable-parasite-find
         gimp-brushes-list
         gimp-image-lower-channel
         gimp-patterns-set-pattern
         gimp-convert-grayscale
         gimp-drawable-parasite-detach
         gimp-drawable-set-name
         gimp-image-lower-vectors-to-bottom
         gimp-parasite-attach
         gimp-vectors-set-tattoo
         gimp-drawable-set-linked
         gimp-channel-ops-duplicate
         gimp-vectors-get-linked
         gimp-vectors-get-tattoo
         gimp-vectors-parasite-attach
         gimp-image-active-drawable
         gimp-convert-indexed
         gimp-drawable-get-name
         gimp-layer-get-visible
         gimp-palettes-set-palette
         gimp-crop
         gimp-drawable-get-image
         gimp-drawable-is-layer
         gimp-palette-get-background
         gimp-undo-push-group-start
         gimp-palette-swap-colors
         gimp-layer-mask
         gimp-layer-get-preserve-trans
         gimp-drawable-parasite-list
         gimp-image-get-channel-position
         gimp-drawable-is-valid
         gimp-gradients-set-active
         gimp-channel-delete
         gimp-vectors-get-visible
         gimp-layer-get-tattoo
         gimp-image-floating-selection
         gimp-vectors-get-image
         gimp-image-raise-layer
         gimp-brushes-set-paint-mode
         gimp-drawable-parasite-attach
         gimp-blend
         gimp-gradients-set-gradient
         gimp-selection-clear
         gimp-channel-set-visible
         gimp-temp-PDB-name
         gimp-image-get-vectors-position
         gimp-palette-set-foreground
         gimp-image-parasite-list
         gimp-layer-set-tattoo
         gimp-drawable-set-visible
         gimp-drawable-get-tattoo
         gimp-image-raise-channel
         gimp-image-lower-layer
         gimp-brushes-set-opacity
         gimp-vectors-set-visible
         gimp-palette-set-default-colors
         gimp-convert-rgb
         gimp-channel-get-visible
         gimp-image-raise-vectors-to-top
         gimp-vectors-is-valid
         gimp-drawable-get-visible
         gimp-image-lower-vectors
         gimp-vectors-parasite-find
         gimp-brushes-get-opacity
         gimp-image-parasite-find
         gimp-drawable-is-text-layer
         gimp-image-raise-layer-to-top
         gimp-layer-set-name
         gimp-layer-set-preserve-trans
         gimp-layer-delete
         gimp-channel-ops-offset
         gimp-parasite-list
         gimp-channel-get-tattoo
         gimp-vectors-set-linked
         gimp-vectors-parasite-list
         gimp-layer-get-linked
         gimp-undo-push-group-end
         gimp-drawable-set-tattoo
         gimp-image-raise-vectors
         gimp-brushes-set-brush
         gimp-channel-set-name
         gimp-palette-set-background
         gimp-gradients-get-gradient
         gimp-bucket-fill
         gimp-image-get-layer-position
         gimp-layer-get-name
         gimp-drawable-is-layer-mask
         gimp-parasite-detach
         gimp-drawable-get-linked
         gimp-image-parasite-attach
         gimp-image-lower-layer-to-bottom
         gimp-palette-refresh
         gimp-drawable-is-channel
         gimp-vectors-set-name
         gimp-vectors-parasite-detach
         gimp-parasite-find
         gimp-patterns-list
         gimp-drawable-delete
         gimp-image-set-cmap
         gimp-color-picker
         gimp-drawable-bytes
         gimp-image-parasite-detach
         gimp-vectors-get-name
         gimp-brushes-get-paint-mode
         gimp-layer-set-visible
  )
(define gimp (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                            "/edu/grinnell/cs/glimmer/gimp"
                            "edu.grinnell.cs.glimmer.pdb"))
(define loudbus-helper
  (lambda (fun)
    (lambda args
      (apply loudbus-call (cons gimp (cons fun args))))))
(define script-fu-round-corners (loudbus-helper 'script_fu_round_corners))
(define file-xjt-load (loudbus-helper 'file_xjt_load))
(define gimp-gradient-segment-set-right-color (loudbus-helper 'gimp_gradient_segment_set_right_color))
(define plug-in-emboss (loudbus-helper 'plug_in_emboss))
(define file-ps-save (loudbus-helper 'file_ps_save))
(define file-png-get-defaults (loudbus-helper 'file_png_get_defaults))
(define gimp-layer-is-floating-sel (loudbus-helper 'gimp_layer_is_floating_sel))
(define gimp-layer-resize (loudbus-helper 'gimp_layer_resize))
(define plug-in-colormap-remap (loudbus-helper 'plug_in_colormap_remap))
(define gimp-pencil (loudbus-helper 'gimp_pencil))
(define file-tiff-save2 (loudbus-helper 'file_tiff_save2))
(define gimp-item-get-linked (loudbus-helper 'gimp_item_get_linked))
(define gimp-text-layer-set-kerning (loudbus-helper 'gimp_text_layer_set_kerning))
(define gimp-vectors-new-from-text-layer (loudbus-helper 'gimp_vectors_new_from_text_layer))
(define gimp-gimprc-set (loudbus-helper 'gimp_gimprc_set))
(define plug-in-vpropagate (loudbus-helper 'plug_in_vpropagate))
(define plug-in-smooth-palette (loudbus-helper 'plug_in_smooth_palette))
(define gimp-selection-bounds (loudbus-helper 'gimp_selection_bounds))
(define script-fu-speed-text (loudbus-helper 'script_fu_speed_text))
(define plug-in-softglow (loudbus-helper 'plug_in_softglow))
(define gimp-image-floating-sel-attached-to (loudbus-helper 'gimp_image_floating_sel_attached_to))
(define gimp-context-get-sample-merged (loudbus-helper 'gimp_context_get_sample_merged))
(define gimp-context-set-sample-criterion (loudbus-helper 'gimp_context_set_sample_criterion))
(define python-fu-palette-offset (loudbus-helper 'python_fu_palette_offset))
(define file-png-load (loudbus-helper 'file_png_load))
(define gimp-context-get-sample-threshold (loudbus-helper 'gimp_context_get_sample_threshold))
(define gimp-help-2-using-photography (loudbus-helper 'gimp_help_2_using_photography))
(define gimp-selection-all (loudbus-helper 'gimp_selection_all))
(define gimp-image-get-resolution (loudbus-helper 'gimp_image_get_resolution))
(define plug-in-curve-bend (loudbus-helper 'plug_in_curve_bend))
(define file-raw-save (loudbus-helper 'file_raw_save))
(define gimp-context-set-default-colors (loudbus-helper 'gimp_context_set_default_colors))
(define gimp-context-set-feather (loudbus-helper 'gimp_context_set_feather))
(define gimp-layer-flatten (loudbus-helper 'gimp_layer_flatten))
(define gimp-item-detach-parasite (loudbus-helper 'gimp_item_detach_parasite))
(define gimp-drawable-free-shadow (loudbus-helper 'gimp_drawable_free_shadow))
(define gimp-drawable-type-with-alpha (loudbus-helper 'gimp_drawable_type_with_alpha))
(define gimp-edit-named-copy (loudbus-helper 'gimp_edit_named_copy))
(define plug-in-despeckle (loudbus-helper 'plug_in_despeckle))
(define file-faxg3-load (loudbus-helper 'file_faxg3_load))
(define file-gih-save (loudbus-helper 'file_gih_save))
(define gimp-image-free-shadow (loudbus-helper 'gimp_image_free_shadow))
(define gimp-drawable-transform-scale (loudbus-helper 'gimp_drawable_transform_scale))
(define gimp-procedural-db-proc-val (loudbus-helper 'gimp_procedural_db_proc_val))
(define plug-in-zealouscrop (loudbus-helper 'plug_in_zealouscrop))
(define gimp-brush-get-spacing (loudbus-helper 'gimp_brush_get_spacing))
(define gimp-edit-named-cut (loudbus-helper 'gimp_edit_named_cut))
(define gimp-image-get-layer-by-name (loudbus-helper 'gimp_image_get_layer_by_name))
(define gimp-help-2-concepts-usage (loudbus-helper 'gimp_help_2_concepts_usage))
(define gimp-airbrush (loudbus-helper 'gimp_airbrush))
(define file-openraster-load-thumb (loudbus-helper 'file_openraster_load_thumb))
(define gimp-drawable-update (loudbus-helper 'gimp_drawable_update))
(define gimp-patterns-set-popup (loudbus-helper 'gimp_patterns_set_popup))
(define gimp-image-get-vectors (loudbus-helper 'gimp_image_get_vectors))
(define plug-in-laplace (loudbus-helper 'plug_in_laplace))
(define gimp-layer-from-mask (loudbus-helper 'gimp_layer_from_mask))
(define gimp-progress-uninstall (loudbus-helper 'gimp_progress_uninstall))
(define gimp-item-get-visible (loudbus-helper 'gimp_item_get_visible))
(define gimp-edit-fill (loudbus-helper 'gimp_edit_fill))
(define gimp-image-lower-item-to-bottom (loudbus-helper 'gimp_image_lower_item_to_bottom))
(define script-fu-blend-anim (loudbus-helper 'script_fu_blend_anim))
(define plug-in-sample-colorize (loudbus-helper 'plug_in_sample_colorize))
(define plug-in-shift (loudbus-helper 'plug_in_shift))
(define gimp-dodgeburn (loudbus-helper 'gimp_dodgeburn))
(define file-pgm-save (loudbus-helper 'file_pgm_save))
(define gimp-edit-named-paste (loudbus-helper 'gimp_edit_named_paste))
(define script-fu-refresh (loudbus-helper 'script_fu_refresh))
(define gimp-vectors-export-to-string (loudbus-helper 'gimp_vectors_export_to_string))
(define gimp-context-pop (loudbus-helper 'gimp_context_pop))
(define gimp-vectors-bezier-stroke-conicto (loudbus-helper 'gimp_vectors_bezier_stroke_conicto))
(define gimp-floating-sel-to-layer (loudbus-helper 'gimp_floating_sel_to_layer))
(define script-fu-title-header (loudbus-helper 'script_fu_title_header))
(define plug-in-color-enhance (loudbus-helper 'plug_in_color_enhance))
(define plug-in-displace (loudbus-helper 'plug_in_displace))
(define file-xmc-save (loudbus-helper 'file_xmc_save))
(define gimp-text-layer-get-hinting (loudbus-helper 'gimp_text_layer_get_hinting))
(define gimp-shear (loudbus-helper 'gimp_shear))
(define gimp-brush-get-info (loudbus-helper 'gimp_brush_get_info))
(define gimp-vectors-import-from-string (loudbus-helper 'gimp_vectors_import_from_string))
(define gimp-get-color-configuration (loudbus-helper 'gimp_get_color_configuration))
(define gimp-text-get-extents (loudbus-helper 'gimp_text_get_extents))
(define gimp-paintbrush (loudbus-helper 'gimp_paintbrush))
(define plug-in-decompose (loudbus-helper 'plug_in_decompose))
(define gimp-item-is-vectors (loudbus-helper 'gimp_item_is_vectors))
(define gimp-palette-add-entry (loudbus-helper 'gimp_palette_add_entry))
(define gimp-help-2-using-simpleobjects (loudbus-helper 'gimp_help_2_using_simpleobjects))
(define script-fu-spyrogimp (loudbus-helper 'script_fu_spyrogimp))
(define script-fu-textured-logo (loudbus-helper 'script_fu_textured_logo))
(define gimp-image-grid-set-background-color (loudbus-helper 'gimp_image_grid_set_background_color))
(define gimp-path-set-locked (loudbus-helper 'gimp_path_set_locked))
(define file-pix-save (loudbus-helper 'file_pix_save))
(define gimp-palettes-close-popup (loudbus-helper 'gimp_palettes_close_popup))
(define plug-in-waves (loudbus-helper 'plug_in_waves))
(define script-fu-3d-outline-logo-alpha (loudbus-helper 'script_fu_3d_outline_logo_alpha))
(define file-bz2-load (loudbus-helper 'file_bz2_load))
(define gimp-channel-copy (loudbus-helper 'gimp_channel_copy))
(define gimp-context-set-sample-transparent (loudbus-helper 'gimp_context_set_sample_transparent))
(define gimp-brush-get-angle (loudbus-helper 'gimp_brush_get_angle))
(define gimp-layer-new-from-drawable (loudbus-helper 'gimp_layer_new_from_drawable))
(define gimp-edit-paste (loudbus-helper 'gimp_edit_paste))
(define gimp-text-layer-set-line-spacing (loudbus-helper 'gimp_text_layer_set_line_spacing))
(define plug-in-spheredesigner (loudbus-helper 'plug_in_spheredesigner))
(define gimp-palette-rename (loudbus-helper 'gimp_palette_rename))
(define gimp-gradient-segment-get-middle-pos (loudbus-helper 'gimp_gradient_segment_get_middle_pos))
(define gimp-context-get-opacity (loudbus-helper 'gimp_context_get_opacity))
(define gimp-buffer-rename (loudbus-helper 'gimp_buffer_rename))
(define gimp-vectors-import-from-file (loudbus-helper 'gimp_vectors_import_from_file))
(define plug-in-rgb-noise (loudbus-helper 'plug_in_rgb_noise))
(define gimp-palettes-get-palette-entry (loudbus-helper 'gimp_palettes_get_palette_entry))
(define gimp-procedural-db-proc-arg (loudbus-helper 'gimp_procedural_db_proc_arg))
(define gimp-image-add-vectors (loudbus-helper 'gimp_image_add_vectors))
(define file-fli-save (loudbus-helper 'file_fli_save))
(define gimp-item-transform-2d (loudbus-helper 'gimp_item_transform_2d))
(define python-fu-console (loudbus-helper 'python_fu_console))
(define gimp-brush-duplicate (loudbus-helper 'gimp_brush_duplicate))
(define file-ico-load-thumb (loudbus-helper 'file_ico_load_thumb))
(define gimp-image-get-item-position (loudbus-helper 'gimp_image_get_item_position))
(define gimp-context-set-font (loudbus-helper 'gimp_context_set_font))
(define gimp-edit-copy-visible (loudbus-helper 'gimp_edit_copy_visible))
(define gimp-image-unset-active-channel (loudbus-helper 'gimp_image_unset_active_channel))
(define script-fu-textured-logo-alpha (loudbus-helper 'script_fu_textured_logo_alpha))
(define gimp-online-developer-web-site (loudbus-helper 'gimp_online_developer_web_site))
(define gimp-unit-get-abbreviation (loudbus-helper 'gimp_unit_get_abbreviation))
(define file-pcx-load (loudbus-helper 'file_pcx_load))
(define gimp-smudge (loudbus-helper 'gimp_smudge))
(define plug-in-the-old-egg (loudbus-helper 'plug_in_the_old_egg))
(define gimp-context-set-brush-aspect-ratio (loudbus-helper 'gimp_context_set_brush_aspect_ratio))
(define plug-in-edge (loudbus-helper 'plug_in_edge))
(define gimp-drawable-transform-shear (loudbus-helper 'gimp_drawable_transform_shear))
(define gimp-layer-get-opacity (loudbus-helper 'gimp_layer_get_opacity))
(define script-fu-glowing-logo (loudbus-helper 'script_fu_glowing_logo))
(define gimp-text-layer-get-letter-spacing (loudbus-helper 'gimp_text_layer_get_letter_spacing))
(define gimp-palettes-get-palette (loudbus-helper 'gimp_palettes_get_palette))
(define gimp-context-set-dynamics (loudbus-helper 'gimp_context_set_dynamics))
(define gimp-text-layer-get-color (loudbus-helper 'gimp_text_layer_get_color))
(define file-tiff-load (loudbus-helper 'file_tiff_load))
(define plug-in-sparkle (loudbus-helper 'plug_in_sparkle))
(define script-fu-sota-chrome-it (loudbus-helper 'script_fu_sota_chrome_it))
(define python-fu-gradient-save-as-css (loudbus-helper 'python_fu_gradient_save_as_css))
(define gimp-layer-get-apply-mask (loudbus-helper 'gimp_layer_get_apply_mask))
(define gimp-channel-set-show-masked (loudbus-helper 'gimp_channel_set_show_masked))
(define gimp-context-get-ink-blob-angle (loudbus-helper 'gimp_context_get_ink_blob_angle))
(define script-fu-comic-logo (loudbus-helper 'script_fu_comic_logo))
(define gimp-context-get-transform-recursion (loudbus-helper 'gimp_context_get_transform_recursion))
(define script-fu-chrome-logo-alpha (loudbus-helper 'script_fu_chrome_logo_alpha))
(define gimp-image-get-active-vectors (loudbus-helper 'gimp_image_get_active_vectors))
(define gimp-item-get-name (loudbus-helper 'gimp_item_get_name))
(define gimp-progress-get-window-handle (loudbus-helper 'gimp_progress_get_window_handle))
(define gimp-image-grid-set-offset (loudbus-helper 'gimp_image_grid_set_offset))
(define gimp-vectors-export-to-file (loudbus-helper 'gimp_vectors_export_to_file))
(define script-fu-newsprint-text (loudbus-helper 'script_fu_newsprint_text))
(define gimp-image-attach-parasite (loudbus-helper 'gimp_image_attach_parasite))
(define gimp-flip (loudbus-helper 'gimp_flip))
(define python-fu-palette-to-gradient-repeating (loudbus-helper 'python_fu_palette_to_gradient_repeating))
(define gimp-context-get-sample-transparent (loudbus-helper 'gimp_context_get_sample_transparent))
(define gimp-palette-new (loudbus-helper 'gimp_palette_new))
(define gimp-convolve-default (loudbus-helper 'gimp_convolve_default))
(define script-fu-labels-gimp-org (loudbus-helper 'script_fu_labels_gimp_org))
(define plug-in-align-layers (loudbus-helper 'plug_in_align_layers))
(define gimp-edit-clear (loudbus-helper 'gimp_edit_clear))
(define gimp-image-set-component-active (loudbus-helper 'gimp_image_set_component_active))
(define gimp-context-set-antialias (loudbus-helper 'gimp_context_set_antialias))
(define gimp-text-layer-get-kerning (loudbus-helper 'gimp_text_layer_get_kerning))
(define gimp-gradient-segment-range-set-blending-function (loudbus-helper 'gimp_gradient_segment_range_set_blending_function))
(define gimp-procedural-db-proc-exists (loudbus-helper 'gimp_procedural_db_proc_exists))
(define gimp-palette-delete (loudbus-helper 'gimp_palette_delete))
(define script-fu-alien-glow-right-arrow (loudbus-helper 'script_fu_alien_glow_right_arrow))
(define plug-in-wind (loudbus-helper 'plug_in_wind))
(define file-uri-save (loudbus-helper 'file_uri_save))
(define gimp-image-get-component-visible (loudbus-helper 'gimp_image_get_component_visible))
(define gimp-buffer-delete (loudbus-helper 'gimp_buffer_delete))
(define gimp-path-get-points (loudbus-helper 'gimp_path_get_points))
(define script-fu-grid-system (loudbus-helper 'script_fu_grid_system))
(define script-fu-slide (loudbus-helper 'script_fu_slide))
(define gimp-get-monitor-resolution (loudbus-helper 'gimp_get_monitor_resolution))
(define gimp-context-swap-colors (loudbus-helper 'gimp_context_swap_colors))
(define gimp-gradient-segment-set-right-pos (loudbus-helper 'gimp_gradient_segment_set_right_pos))
(define plug-in-iwarp (loudbus-helper 'plug_in_iwarp))
(define gimp-image-select-round-rectangle (loudbus-helper 'gimp_image_select_round_rectangle))
(define file-header-save (loudbus-helper 'file_header_save))
(define gimp-help-concepts-paths (loudbus-helper 'gimp_help_concepts_paths))
(define gimp-image-pick-correlate-layer (loudbus-helper 'gimp_image_pick_correlate_layer))
(define gimp-context-get-brush-aspect-ratio (loudbus-helper 'gimp_context_get_brush_aspect_ratio))
(define script-fu-selection-to-image (loudbus-helper 'script_fu_selection_to_image))
(define file-ppm-save (loudbus-helper 'file_ppm_save))
(define script-fu-basic1-logo-alpha (loudbus-helper 'script_fu_basic1_logo_alpha))
(define gimp-text-layer-new (loudbus-helper 'gimp_text_layer_new))
(define gimp-context-get-sample-criterion (loudbus-helper 'gimp_context_get_sample_criterion))
(define plug-in-photocopy (loudbus-helper 'plug_in_photocopy))
(define gimp-image-get-colormap (loudbus-helper 'gimp_image_get_colormap))
(define file-bmp-save (loudbus-helper 'file_bmp_save))
(define plug-in-colormap-swap (loudbus-helper 'plug_in_colormap_swap))
(define script-fu-tile-blur (loudbus-helper 'script_fu_tile_blur))
(define gimp-path-get-current (loudbus-helper 'gimp_path_get_current))
(define gimp-fuzzy-select-full (loudbus-helper 'gimp_fuzzy_select_full))
(define gimp-edit-paste-as-new (loudbus-helper 'gimp_edit_paste_as_new))
(define gimp-item-transform-rotate-simple (loudbus-helper 'gimp_item_transform_rotate_simple))
(define plug-in-autocrop-layer (loudbus-helper 'plug_in_autocrop_layer))
(define gimp-attach-parasite (loudbus-helper 'gimp_attach_parasite))
(define gimp-selection-combine (loudbus-helper 'gimp_selection_combine))
(define gimp-context-get-brush-size (loudbus-helper 'gimp_context_get_brush_size))
(define file-xbm-save (loudbus-helper 'file_xbm_save))
(define gimp-path-get-tattoo (loudbus-helper 'gimp_path_get_tattoo))
(define gimp-gradient-rename (loudbus-helper 'gimp_gradient_rename))
(define gimp-buffer-get-bytes (loudbus-helper 'gimp_buffer_get_bytes))
(define gimp-vectors-bezier-stroke-cubicto (loudbus-helper 'gimp_vectors_bezier_stroke_cubicto))
(define gimp-item-get-tattoo (loudbus-helper 'gimp_item_get_tattoo))
(define gimp-item-is-layer (loudbus-helper 'gimp_item_is_layer))
(define gimp-text-layer-set-letter-spacing (loudbus-helper 'gimp_text_layer_set_letter_spacing))
(define gimp-image-flip (loudbus-helper 'gimp_image_flip))
(define plug-in-pixelize2 (loudbus-helper 'plug_in_pixelize2))
(define file-gif-save (loudbus-helper 'file_gif_save))
(define plug-in-metadata-set-simple (loudbus-helper 'plug_in_metadata_set_simple))
(define gimp-item-transform-rotate (loudbus-helper 'gimp_item_transform_rotate))
(define gimp-context-set-ink-blob-angle (loudbus-helper 'gimp_context_set_ink_blob_angle))
(define gimp-gradients-sample-uniform (loudbus-helper 'gimp_gradients_sample_uniform))
(define file-cel-load (loudbus-helper 'file_cel_load))
(define script-fu-fuzzy-border (loudbus-helper 'script_fu_fuzzy_border))
(define gimp-image-set-active-vectors (loudbus-helper 'gimp_image_set_active_vectors))
(define gimp-plugin-set-pdb-error-handler (loudbus-helper 'gimp_plugin_set_pdb_error_handler))
(define gimp-help-using-photography (loudbus-helper 'gimp_help_using_photography))
(define gimp-brush-set-spikes (loudbus-helper 'gimp_brush_set_spikes))
(define script-fu-gradient-example (loudbus-helper 'script_fu_gradient_example))
(define script-fu-starscape-logo (loudbus-helper 'script_fu_starscape_logo))
(define script-fu-alien-glow-logo-alpha (loudbus-helper 'script_fu_alien_glow_logo_alpha))
(define file-sunras-load (loudbus-helper 'file_sunras_load))
(define gimp-gradients-get-gradient-data (loudbus-helper 'gimp_gradients_get_gradient_data))
(define gimp-palette-set-columns (loudbus-helper 'gimp_palette_set_columns))
(define script-fu-difference-clouds (loudbus-helper 'script_fu_difference_clouds))
(define gimp-heal-default (loudbus-helper 'gimp_heal_default))
(define gimp-gradient-get-number-of-segments (loudbus-helper 'gimp_gradient_get_number_of_segments))
(define plug-in-drawable-compose (loudbus-helper 'plug_in_drawable_compose))
(define script-fu-beveled-pattern-button (loudbus-helper 'script_fu_beveled_pattern_button))
(define gimp-context-set-paint-mode (loudbus-helper 'gimp_context_set_paint_mode))
(define script-fu-blended-logo (loudbus-helper 'script_fu_blended_logo))
(define gimp-procedural-db-proc-info (loudbus-helper 'gimp_procedural_db_proc_info))
(define gimp-display-is-valid (loudbus-helper 'gimp_display_is_valid))
(define gimp-item-get-parent (loudbus-helper 'gimp_item_get_parent))
(define plug-in-recompose (loudbus-helper 'plug_in_recompose))
(define gimp-clone (loudbus-helper 'gimp_clone))
(define gimp-selection-grow (loudbus-helper 'gimp_selection_grow))
(define gimp-image-set-colormap (loudbus-helper 'gimp_image_set_colormap))
(define gimp-gradient-segment-get-right-pos (loudbus-helper 'gimp_gradient_segment_get_right_pos))
(define gimp-layer-set-show-mask (loudbus-helper 'gimp_layer_set_show_mask))
(define script-fu-tube-button-label-gimp-org (loudbus-helper 'script_fu_tube_button_label_gimp_org))
(define plug-in-papertile (loudbus-helper 'plug_in_papertile))
(define gimp-context-list-paint-methods (loudbus-helper 'gimp_context_list_paint_methods))
(define script-fu-flatland (loudbus-helper 'script_fu_flatland))
(define gimp-image-raise-item (loudbus-helper 'gimp_image_raise_item))
(define script-fu-t-o-p-logo-alpha (loudbus-helper 'script_fu_t_o_p_logo_alpha))
(define gimp-image-get-layer-by-tattoo (loudbus-helper 'gimp_image_get_layer_by_tattoo))
(define plug-in-sel-gauss (loudbus-helper 'plug_in_sel_gauss))
(define plug-in-ifscompose (loudbus-helper 'plug_in_ifscompose))
(define gimp-progress-end (loudbus-helper 'gimp_progress_end))
(define gimp-image-remove-channel (loudbus-helper 'gimp_image_remove_channel))
(define gimp-detach-parasite (loudbus-helper 'gimp_detach_parasite))
(define gimp-help-using-docks (loudbus-helper 'gimp_help_using_docks))
(define gimp-selection-sharpen (loudbus-helper 'gimp_selection_sharpen))
(define gimp-image-get-unit (loudbus-helper 'gimp_image_get_unit))
(define plug-in-autostretch-hsv (loudbus-helper 'plug_in_autostretch_hsv))
(define plug-in-icc-profile-apply-rgb (loudbus-helper 'plug_in_icc_profile_apply_rgb))
(define gimp-image-get-guide-orientation (loudbus-helper 'gimp_image_get_guide_orientation))
(define gimp-layer-new-from-visible (loudbus-helper 'gimp_layer_new_from_visible))
(define plug-in-ccanalyze (loudbus-helper 'plug_in_ccanalyze))
(define gimp-gradient-delete (loudbus-helper 'gimp_gradient_delete))
(define gimp-layer-scale-full (loudbus-helper 'gimp_layer_scale_full))
(define plug-in-tile (loudbus-helper 'plug_in_tile))
(define plug-in-unit-editor (loudbus-helper 'plug_in_unit_editor))
(define gimp-channel-set-opacity (loudbus-helper 'gimp_channel_set_opacity))
(define gimp-image-clean-all (loudbus-helper 'gimp_image_clean_all))
(define gimp-selection-is-empty (loudbus-helper 'gimp_selection_is_empty))
(define file-xjt-save (loudbus-helper 'file_xjt_save))
(define gimp-image-remove-layer (loudbus-helper 'gimp_image_remove_layer))
(define gimp-vectors-stroke-scale (loudbus-helper 'gimp_vectors_stroke_scale))
(define gimp-context-get-ink-size (loudbus-helper 'gimp_context_get_ink_size))
(define plug-in-rotate-colormap (loudbus-helper 'plug_in_rotate_colormap))
(define gimp-image-grid-get-spacing (loudbus-helper 'gimp_image_grid_get_spacing))
(define gimp-gradient-segment-range-redistribute-handles (loudbus-helper 'gimp_gradient_segment_range_redistribute_handles))
(define gimp-item-set-linked (loudbus-helper 'gimp_item_set_linked))
(define extension-gimp-help-browser (loudbus-helper 'extension_gimp_help_browser))
(define gimp-image-get-xcf-uri (loudbus-helper 'gimp_image_get_xcf_uri))
(define gimp-vectors-bezier-stroke-new-ellipse (loudbus-helper 'gimp_vectors_bezier_stroke_new_ellipse))
(define gimp-dynamics-get-list (loudbus-helper 'gimp_dynamics_get_list))
(define gimp-drawable-transform-shear-default (loudbus-helper 'gimp_drawable_transform_shear_default))
(define gimp-fonts-set-popup (loudbus-helper 'gimp_fonts_set_popup))
(define script-fu-copy-visible (loudbus-helper 'script_fu_copy_visible))
(define gimp-get-parasite-list (loudbus-helper 'gimp_get_parasite_list))
(define gimp-image-convert-set-dither-matrix (loudbus-helper 'gimp_image_convert_set_dither_matrix))
(define file-csource-save (loudbus-helper 'file_csource_save))
(define gimp-context-push (loudbus-helper 'gimp_context_push))
(define gimp-image-scale-full (loudbus-helper 'gimp_image_scale_full))
(define gimp-image-select-rectangle (loudbus-helper 'gimp_image_select_rectangle))
(define file-png-save (loudbus-helper 'file_png_save))
(define gimp-by-color-select-full (loudbus-helper 'gimp_by_color_select_full))
(define gimp-free-select (loudbus-helper 'gimp_free_select))
(define gimp-context-set-sample-threshold-int (loudbus-helper 'gimp_context_set_sample_threshold_int))
(define plug-in-palettemap (loudbus-helper 'plug_in_palettemap))
(define gimp-gradient-segment-set-left-pos (loudbus-helper 'gimp_gradient_segment_set_left_pos))
(define gimp-gradient-segment-range-delete (loudbus-helper 'gimp_gradient_segment_range_delete))
(define gimp-drawable-transform-2d (loudbus-helper 'gimp_drawable_transform_2d))
(define plug-in-threshold-alpha (loudbus-helper 'plug_in_threshold_alpha))
(define gimp-register-magic-load-handler (loudbus-helper 'gimp_register_magic_load_handler))
(define gimp-channel-get-color (loudbus-helper 'gimp_channel_get_color))
(define gimp-context-get-pattern (loudbus-helper 'gimp_context_get_pattern))
(define gimp-drawable-merge-shadow (loudbus-helper 'gimp_drawable_merge_shadow))
(define gimp-unit-get-identifier (loudbus-helper 'gimp_unit_get_identifier))
(define script-fu-add-bevel (loudbus-helper 'script_fu_add_bevel))
(define plug-in-gflare (loudbus-helper 'plug_in_gflare))
(define plug-in-compose (loudbus-helper 'plug_in_compose))
(define gimp-context-set-ink-speed-sensitivity (loudbus-helper 'gimp_context_set_ink_speed_sensitivity))
(define script-fu-line-nova (loudbus-helper 'script_fu_line_nova))
(define script-fu-spinning-globe (loudbus-helper 'script_fu_spinning_globe))
(define gimp-image-thumbnail (loudbus-helper 'gimp_image_thumbnail))
(define script-fu-font-map (loudbus-helper 'script_fu_font_map))
(define gimp-layer-get-show-mask (loudbus-helper 'gimp_layer_get_show_mask))
(define gimp-palette-is-editable (loudbus-helper 'gimp_palette_is_editable))
(define script-fu-weave (loudbus-helper 'script_fu_weave))
(define gimp-gradients-refresh (loudbus-helper 'gimp_gradients_refresh))
(define gimp-brushes-set-spacing (loudbus-helper 'gimp_brushes_set_spacing))
(define gimp-levels-auto (loudbus-helper 'gimp_levels_auto))
(define gimp-help-using-simpleobjects (loudbus-helper 'gimp_help_using_simpleobjects))
(define gimp-image-insert-channel (loudbus-helper 'gimp_image_insert_channel))
(define plug-in-qbist (loudbus-helper 'plug_in_qbist))
(define gimp-drawable-transform-flip (loudbus-helper 'gimp_drawable_transform_flip))
(define gimp-help-using-selections (loudbus-helper 'gimp_help_using_selections))
(define plug-in-sharpen (loudbus-helper 'plug_in_sharpen))
(define gimp-context-get-feather-radius (loudbus-helper 'gimp_context_get_feather_radius))
(define gimp-convolve (loudbus-helper 'gimp_convolve))
(define gimp-text-layer-set-hint-style (loudbus-helper 'gimp_text_layer_set_hint_style))
(define gimp-context-set-ink-size (loudbus-helper 'gimp_context_set_ink_size))
(define gimp-image-get-guide-position (loudbus-helper 'gimp_image_get_guide_position))
(define file-gbr-load (loudbus-helper 'file_gbr_load))
(define gimp-unit-get-number-of-units (loudbus-helper 'gimp_unit_get_number_of_units))
(define plug-in-metadata-import (loudbus-helper 'plug_in_metadata_import))
(define file-gtm-save (loudbus-helper 'file_gtm_save))
(define gimp-image-undo-enable (loudbus-helper 'gimp_image_undo_enable))
(define gimp-image-delete-guide (loudbus-helper 'gimp_image_delete_guide))
(define gimp-item-attach-parasite (loudbus-helper 'gimp_item_attach_parasite))
(define gimp-unit-get-digits (loudbus-helper 'gimp_unit_get_digits))
(define gimp-get-default-comment (loudbus-helper 'gimp_get_default_comment))
(define gimp-path-stroke-current (loudbus-helper 'gimp_path_stroke_current))
(define gimp-text-layer-set-justification (loudbus-helper 'gimp_text_layer_set_justification))
(define gimp-gradient-segment-range-split-midpoint (loudbus-helper 'gimp_gradient_segment_range_split_midpoint))
(define plug-in-engrave (loudbus-helper 'plug_in_engrave))
(define file-svg-load (loudbus-helper 'file_svg_load))
(define gimp-text-layer-get-line-spacing (loudbus-helper 'gimp_text_layer_get_line_spacing))
(define script-fu-alien-glow-bullet (loudbus-helper 'script_fu_alien_glow_bullet))
(define plug-in-cml-explorer (loudbus-helper 'plug_in_cml_explorer))
(define plug-in-animationoptimize (loudbus-helper 'plug_in_animationoptimize))
(define gimp-gradient-segment-range-set-coloring-type (loudbus-helper 'gimp_gradient_segment_range_set_coloring_type))
(define gimp-selection-value (loudbus-helper 'gimp_selection_value))
(define gimp-text-layer-set-font (loudbus-helper 'gimp_text_layer_set_font))
(define gimp-brush-set-hardness (loudbus-helper 'gimp_brush_set_hardness))
(define gimp-brush-set-angle (loudbus-helper 'gimp_brush_set_angle))
(define gimp-item-transform-matrix (loudbus-helper 'gimp_item_transform_matrix))
(define file-sgi-load (loudbus-helper 'file_sgi_load))
(define script-fu-carved-logo (loudbus-helper 'script_fu_carved_logo))
(define script-fu-bovinated-logo (loudbus-helper 'script_fu_bovinated_logo))
(define gimp-drawable-is-rgb (loudbus-helper 'gimp_drawable_is_rgb))
(define gimp-layer-remove-mask (loudbus-helper 'gimp_layer_remove_mask))
(define gimp-image-undo-group-end (loudbus-helper 'gimp_image_undo_group_end))
(define gimp-layer-set-mode (loudbus-helper 'gimp_layer_set_mode))
(define gimp-image-grid-set-style (loudbus-helper 'gimp_image_grid_set_style))
(define gimp-image-get-selection (loudbus-helper 'gimp_image_get_selection))
(define gimp-xcf-load (loudbus-helper 'gimp_xcf_load))
(define gimp-context-set-brush (loudbus-helper 'gimp_context_set_brush))
(define gimp-image-resize-to-layers (loudbus-helper 'gimp_image_resize_to_layers))
(define plug-in-metadata-get (loudbus-helper 'plug_in_metadata_get))
(define gimp-context-get-paint-method (loudbus-helper 'gimp_context_get_paint_method))
(define plug-in-script-fu-text-console (loudbus-helper 'plug_in_script_fu_text_console))
(define gimp-layer-set-lock-alpha (loudbus-helper 'gimp_layer_set_lock_alpha))
(define gimp-palette-export-text (loudbus-helper 'gimp_palette_export_text))
(define file-bz2-save (loudbus-helper 'file_bz2_save))
(define gimp-brush-get-radius (loudbus-helper 'gimp_brush_get_radius))
(define gimp-item-transform-perspective (loudbus-helper 'gimp_item_transform_perspective))
(define script-fu-perspective-shadow (loudbus-helper 'script_fu_perspective_shadow))
(define gimp-image-get-imported-uri (loudbus-helper 'gimp_image_get_imported_uri))
(define gimp-image-get-vectors-by-tattoo (loudbus-helper 'gimp_image_get_vectors_by_tattoo))
(define gimp-image-height (loudbus-helper 'gimp_image_height))
(define gimp-image-undo-thaw (loudbus-helper 'gimp_image_undo_thaw))
(define gimp-text-layer-get-markup (loudbus-helper 'gimp_text_layer_get_markup))
(define gimp-image-is-valid (loudbus-helper 'gimp_image_is_valid))
(define gimp-image-convert-grayscale (loudbus-helper 'gimp_image_convert_grayscale))
(define gimp-palette-entry-get-color (loudbus-helper 'gimp_palette_entry_get_color))
(define plug-in-mblur-inward (loudbus-helper 'plug_in_mblur_inward))
(define gimp-help-concepts-usage (loudbus-helper 'gimp_help_concepts_usage))
(define plug-in-lens-distortion (loudbus-helper 'plug_in_lens_distortion))
(define script-fu-bovinated-logo-alpha (loudbus-helper 'script_fu_bovinated_logo_alpha))
(define file-fits-load (loudbus-helper 'file_fits_load))
(define gimp-image-merge-down (loudbus-helper 'gimp_image_merge_down))
(define script-fu-coffee-stain (loudbus-helper 'script_fu_coffee_stain))
(define extension-gimp-help (loudbus-helper 'extension_gimp_help))
(define gimp-register-load-handler (loudbus-helper 'gimp_register_load_handler))
(define gimp-drawable-transform-perspective (loudbus-helper 'gimp_drawable_transform_perspective))
(define gimp-vectors-stroke-new-from-points (loudbus-helper 'gimp_vectors_stroke_new_from_points))
(define script-fu-addborder (loudbus-helper 'script_fu_addborder))
(define gimp-text-layer-get-base-direction (loudbus-helper 'gimp_text_layer_get_base_direction))
(define plug-in-map-object (loudbus-helper 'plug_in_map_object))
(define gimp-brush-get-shape (loudbus-helper 'gimp_brush_get_shape))
(define gimp-context-set-ink-blob-type (loudbus-helper 'gimp_context_set_ink_blob_type))
(define gimp-image-lower-item (loudbus-helper 'gimp_image_lower_item))
(define plug-in-flame (loudbus-helper 'plug_in_flame))
(define plug-in-apply-canvas (loudbus-helper 'plug_in_apply_canvas))
(define gimp-image-get-layers (loudbus-helper 'gimp_image_get_layers))
(define gimp-equalize (loudbus-helper 'gimp_equalize))
(define python-fu-foggify (loudbus-helper 'python_fu_foggify))
(define gimp-drawable-set-image (loudbus-helper 'gimp_drawable_set_image))
(define file-pcx-save (loudbus-helper 'file_pcx_save))
(define gimp-display-get-window-handle (loudbus-helper 'gimp_display_get_window_handle))
(define gimp-text-layer-get-justification (loudbus-helper 'gimp_text_layer_get_justification))
(define gimp-get-default-unit (loudbus-helper 'gimp_get_default_unit))
(define gimp-context-set-feather-radius (loudbus-helper 'gimp_context_set_feather_radius))
(define script-fu-predator (loudbus-helper 'script_fu_predator))
(define gimp-rect-select (loudbus-helper 'gimp_rect_select))
(define gimp-displays-flush (loudbus-helper 'gimp_displays_flush))
(define plug-in-convmatrix (loudbus-helper 'plug_in_convmatrix))
(define gimp-posterize (loudbus-helper 'gimp_posterize))
(define gimp-context-set-defaults (loudbus-helper 'gimp_context_set_defaults))
(define file-tiff-save (loudbus-helper 'file_tiff_save))
(define gimp-progress-cancel (loudbus-helper 'gimp_progress_cancel))
(define script-fu-circuit (loudbus-helper 'script_fu_circuit))
(define plug-in-animationunoptimize (loudbus-helper 'plug_in_animationunoptimize))
(define gimp-floating-sel-attach (loudbus-helper 'gimp_floating_sel_attach))
(define plug-in-c-astretch (loudbus-helper 'plug_in_c_astretch))
(define script-fu-paste-as-pattern (loudbus-helper 'script_fu_paste_as_pattern))
(define gimp-channel-get-show-masked (loudbus-helper 'gimp_channel_get_show_masked))
(define plug-in-unsharp-mask (loudbus-helper 'plug_in_unsharp_mask))
(define gimp-context-get-ink-blob-type (loudbus-helper 'gimp_context_get_ink_blob_type))
(define gimp-plugin-domain-register (loudbus-helper 'gimp_plugin_domain_register))
(define gimp-layer-scale (loudbus-helper 'gimp_layer_scale))
(define python-fu-eval (loudbus-helper 'python_fu_eval))
(define gimp-get-module-load-inhibit (loudbus-helper 'gimp_get_module_load_inhibit))
(define plug-in-semiflatten (loudbus-helper 'plug_in_semiflatten))
(define script-fu-basic1-logo (loudbus-helper 'script_fu_basic1_logo))
(define gimp-progress-install (loudbus-helper 'gimp_progress_install))
(define file-xwd-load (loudbus-helper 'file_xwd_load))
(define script-fu-selection-round (loudbus-helper 'script_fu_selection_round))
(define gimp-context-get-ink-blob-aspect-ratio (loudbus-helper 'gimp_context_get_ink_blob_aspect_ratio))
(define file-pat-load (loudbus-helper 'file_pat_load))
(define script-fu-blended-logo-alpha (loudbus-helper 'script_fu_blended_logo_alpha))
(define gimp-vectors-copy (loudbus-helper 'gimp_vectors_copy))
(define gimp-path-set-points (loudbus-helper 'gimp_path_set_points))
(define gimp-drawable-sub-thumbnail (loudbus-helper 'gimp_drawable_sub_thumbnail))
(define gimp-text (loudbus-helper 'gimp_text))
(define gimp-progress-init (loudbus-helper 'gimp_progress_init))
(define plug-in-destripe (loudbus-helper 'plug_in_destripe))
(define script-fu-guide-new-percent (loudbus-helper 'script_fu_guide_new_percent))
(define gimp-item-is-channel (loudbus-helper 'gimp_item_is_channel))
(define gimp-patterns-popup (loudbus-helper 'gimp_patterns_popup))
(define file-mng-save (loudbus-helper 'file_mng_save))
(define gimp-drawable-is-indexed (loudbus-helper 'gimp_drawable_is_indexed))
(define gimp-context-get-dynamics (loudbus-helper 'gimp_context_get_dynamics))
(define gimp-image-undo-is-enabled (loudbus-helper 'gimp_image_undo_is_enabled))
(define gimp-path-import (loudbus-helper 'gimp_path_import))
(define gimp-smudge-default (loudbus-helper 'gimp_smudge_default))
(define file-wmf-load-thumb (loudbus-helper 'file_wmf_load_thumb))
(define gimp-brush-get-aspect-ratio (loudbus-helper 'gimp_brush_get_aspect_ratio))
(define gimp-progress-pulse (loudbus-helper 'gimp_progress_pulse))
(define gimp-context-set-transform-recursion (loudbus-helper 'gimp_context_set_transform_recursion))
(define plug-in-noisify (loudbus-helper 'plug_in_noisify))
(define gimp-text-get-extents-fontname (loudbus-helper 'gimp_text_get_extents_fontname))
(define gimp-text-layer-set-base-direction (loudbus-helper 'gimp_text_layer_set_base_direction))
(define plug-in-metadata-export (loudbus-helper 'plug_in_metadata_export))
(define gimp-fonts-get-list (loudbus-helper 'gimp_fonts_get_list))
(define gimp-brush-is-generated (loudbus-helper 'gimp_brush_is_generated))
(define gimp-gradient-get-uniform-samples (loudbus-helper 'gimp_gradient_get_uniform_samples))
(define plug-in-neon (loudbus-helper 'plug_in_neon))
(define plug-in-animationplay (loudbus-helper 'plug_in_animationplay))
(define gimp-path-set-tattoo (loudbus-helper 'gimp_path_set_tattoo))
(define gimp-image-add-channel (loudbus-helper 'gimp_image_add_channel))
(define script-fu-swirly-pattern (loudbus-helper 'script_fu_swirly_pattern))
(define script-fu-tube-subsubbutton-label-gimp-org (loudbus-helper 'script_fu_tube_subsubbutton_label_gimp_org))
(define gimp-item-set-tattoo (loudbus-helper 'gimp_item_set_tattoo))
(define plug-in-mail-image (loudbus-helper 'plug_in_mail_image))
(define gimp-drawable-transform-matrix-default (loudbus-helper 'gimp_drawable_transform_matrix_default))
(define gimp-image-get-floating-sel (loudbus-helper 'gimp_image_get_floating_sel))
(define plug-in-gimpressionist (loudbus-helper 'plug_in_gimpressionist))
(define gimp-selection-invert (loudbus-helper 'gimp_selection_invert))
(define script-fu-t-o-p-logo (loudbus-helper 'script_fu_t_o_p_logo))
(define plug-in-sel2path (loudbus-helper 'plug_in_sel2path))
(define gimp-file-load-layer (loudbus-helper 'gimp_file_load_layer))
(define gimp-brush-new (loudbus-helper 'gimp_brush_new))
(define gimp-context-set-paint-method (loudbus-helper 'gimp_context_set_paint_method))
(define script-fu-text-circle (loudbus-helper 'script_fu_text_circle))
(define script-fu-alien-neon-logo (loudbus-helper 'script_fu_alien_neon_logo))
(define gimp-buffer-get-height (loudbus-helper 'gimp_buffer_get_height))
(define file-cel-save (loudbus-helper 'file_cel_save))
(define plug-in-sel2path-advanced (loudbus-helper 'plug_in_sel2path_advanced))
(define plug-in-web-browser (loudbus-helper 'plug_in_web_browser))
(define gimp-levels (loudbus-helper 'gimp_levels))
(define gimp-brushes-get-brush (loudbus-helper 'gimp_brushes_get_brush))
(define gimp-palettes-get-list (loudbus-helper 'gimp_palettes_get_list))
(define gimp-threshold (loudbus-helper 'gimp_threshold))
(define gimp-drawable-set-pixel (loudbus-helper 'gimp_drawable_set_pixel))
(define file-sunras-save (loudbus-helper 'file_sunras_save))
(define python-fu-slice (loudbus-helper 'python_fu_slice))
(define plug-in-mblur (loudbus-helper 'plug_in_mblur))
(define gimp-text-layer-get-text (loudbus-helper 'gimp_text_layer_get_text))
(define gimp-item-get-children (loudbus-helper 'gimp_item_get_children))
(define plug-in-script-fu-console (loudbus-helper 'plug_in_script_fu_console))
(define gimp-context-get-paint-mode (loudbus-helper 'gimp_context_get_paint_mode))
(define gimp-context-set-gradient (loudbus-helper 'gimp_context_set_gradient))
(define gimp-procedural-db-get-data-size (loudbus-helper 'gimp_procedural_db_get_data_size))
(define gimp-image-add-layer-mask (loudbus-helper 'gimp_image_add_layer_mask))
(define gimp-curves-spline (loudbus-helper 'gimp_curves_spline))
(define gimp-image-get-active-channel (loudbus-helper 'gimp_image_get_active_channel))
(define gimp-help-2-using-selections (loudbus-helper 'gimp_help_2_using_selections))
(define gimp-selection-feather (loudbus-helper 'gimp_selection_feather))
(define gimp-colorize (loudbus-helper 'gimp_colorize))
(define plug-in-animationoptimize-diff (loudbus-helper 'plug_in_animationoptimize_diff))
(define script-fu-beveled-pattern-heading (loudbus-helper 'script_fu_beveled_pattern_heading))
(define file-xpm-load (loudbus-helper 'file_xpm_load))
(define script-fu-3d-outline-logo (loudbus-helper 'script_fu_3d_outline_logo))
(define gimp-item-transform-flip-simple (loudbus-helper 'gimp_item_transform_flip_simple))
(define gimp-help-2-using-fileformats (loudbus-helper 'gimp_help_2_using_fileformats))
(define gimp-layer-add-mask (loudbus-helper 'gimp_layer_add_mask))
(define plug-in-gauss-rle (loudbus-helper 'plug_in_gauss_rle))
(define plug-in-polar-coords (loudbus-helper 'plug_in_polar_coords))
(define script-fu-unsharp-mask (loudbus-helper 'script_fu_unsharp_mask))
(define gimp-item-transform-scale (loudbus-helper 'gimp_item_transform_scale))
(define script-fu-alien-glow-logo (loudbus-helper 'script_fu_alien_glow_logo))
(define gimp-edit-copy (loudbus-helper 'gimp_edit_copy))
(define gimp-procedural-db-get-data (loudbus-helper 'gimp_procedural_db_get_data))
(define python-fu-brush-from-text (loudbus-helper 'python_fu_brush_from_text))
(define script-fu-carve-it (loudbus-helper 'script_fu_carve_it))
(define gimp-palette-get-info (loudbus-helper 'gimp_palette_get_info))
(define gimp-palette-entry-get-name (loudbus-helper 'gimp_palette_entry_get_name))
(define gimp-drawable-transform-flip-simple (loudbus-helper 'gimp_drawable_transform_flip_simple))
(define script-fu-make-brush-elliptical (loudbus-helper 'script_fu_make_brush_elliptical))
(define script-fu-make-brush-rectangular-feathered (loudbus-helper 'script_fu_make_brush_rectangular_feathered))
(define plug-in-gfig (loudbus-helper 'plug_in_gfig))
(define plug-in-pixelize (loudbus-helper 'plug_in_pixelize))
(define gimp-drawable-transform-rotate-default (loudbus-helper 'gimp_drawable_transform_rotate_default))
(define gimp-image-select-polygon (loudbus-helper 'gimp_image_select_polygon))
(define gimp-drawable-transform-rotate-simple (loudbus-helper 'gimp_drawable_transform_rotate_simple))
(define gimp-drawable-bpp (loudbus-helper 'gimp_drawable_bpp))
(define gimp-clone-default (loudbus-helper 'gimp_clone_default))
(define file-psd-load (loudbus-helper 'file_psd_load))
(define gimp-vectors-stroke-flip (loudbus-helper 'gimp_vectors_stroke_flip))
(define gimp-image-select-item (loudbus-helper 'gimp_image_select_item))
(define plug-in-randomize-hurl (loudbus-helper 'plug_in_randomize_hurl))
(define gimp-by-color-select (loudbus-helper 'gimp_by_color_select))
(define plug-in-glasstile (loudbus-helper 'plug_in_glasstile))
(define gimp-item-get-image (loudbus-helper 'gimp_item_get_image))
(define script-fu-chalk-logo-alpha (loudbus-helper 'script_fu_chalk_logo_alpha))
(define gimp-temp-name (loudbus-helper 'gimp_temp_name))
(define python-fu-palette-to-gradient (loudbus-helper 'python_fu_palette_to_gradient))
(define gimp-context-get-feather (loudbus-helper 'gimp_context_get_feather))
(define gimp-round-rect-select (loudbus-helper 'gimp_round_rect_select))
(define file-pdf-load (loudbus-helper 'file_pdf_load))
(define script-fu-xach-effect (loudbus-helper 'script_fu_xach_effect))
(define script-fu-beveled-pattern-hrule (loudbus-helper 'script_fu_beveled_pattern_hrule))
(define gimp-vectors-stroke-get-points (loudbus-helper 'gimp_vectors_stroke_get_points))
(define gimp-image-new (loudbus-helper 'gimp_image_new))
(define file-psd-load-thumb (loudbus-helper 'file_psd_load_thumb))
(define gimp-gradient-is-editable (loudbus-helper 'gimp_gradient_is_editable))
(define plug-in-spread (loudbus-helper 'plug_in_spread))
(define gimp-edit-named-copy-visible (loudbus-helper 'gimp_edit_named_copy_visible))
(define gimp-context-set-background (loudbus-helper 'gimp_context_set_background))
(define gimp-item-is-valid (loudbus-helper 'gimp_item_is_valid))
(define gimp-online-docs-web-site (loudbus-helper 'gimp_online_docs_web_site))
(define file-png-save-defaults (loudbus-helper 'file_png_save_defaults))
(define gimp-unit-get-factor (loudbus-helper 'gimp_unit_get_factor))
(define gimp-context-set-interpolation (loudbus-helper 'gimp_context_set_interpolation))
(define gimp-patterns-close-popup (loudbus-helper 'gimp_patterns_close_popup))
(define script-fu-gradient-bevel-logo-alpha (loudbus-helper 'script_fu_gradient_bevel_logo_alpha))
(define gimp-plugin-icon-register (loudbus-helper 'gimp_plugin_icon_register))
(define gimp-gradients-set-popup (loudbus-helper 'gimp_gradients_set_popup))
(define gimp-get-parasite (loudbus-helper 'gimp_get_parasite))
(define gimp-desaturate-full (loudbus-helper 'gimp_desaturate_full))
(define gimp-unit-set-deletion-flag (loudbus-helper 'gimp_unit_set_deletion_flag))
(define gimp-unit-get-plural (loudbus-helper 'gimp_unit_get_plural))
(define file-ico-load (loudbus-helper 'file_ico_load))
(define plug-in-film (loudbus-helper 'plug_in_film))
(define gimp-image-raise-item-to-top (loudbus-helper 'gimp_image_raise_item_to_top))
(define gimp-layer-translate (loudbus-helper 'gimp_layer_translate))
(define gimp-unit-get-symbol (loudbus-helper 'gimp_unit_get_symbol))
(define gimp-selection-load (loudbus-helper 'gimp_selection_load))
(define gimp-gradients-sample-custom (loudbus-helper 'gimp_gradients_sample_custom))
(define gimp-brush-set-aspect-ratio (loudbus-helper 'gimp_brush_set_aspect_ratio))
(define gimp-image-resize (loudbus-helper 'gimp_image_resize))
(define gimp-path-to-selection (loudbus-helper 'gimp_path_to_selection))
(define gimp-palette-export-php (loudbus-helper 'gimp_palette_export_php))
(define gimp-path-delete (loudbus-helper 'gimp_path_delete))
(define script-fu-make-brush-rectangular (loudbus-helper 'script_fu_make_brush_rectangular))
(define gimp-image-list (loudbus-helper 'gimp_image_list))
(define gimp-edit-bucket-fill-full (loudbus-helper 'gimp_edit_bucket_fill_full))
(define gimp-vectors-stroke-get-point-at-dist (loudbus-helper 'gimp_vectors_stroke_get_point_at_dist))
(define gimp-image-set-active-channel (loudbus-helper 'gimp_image_set_active_channel))
(define script-fu-guide-new (loudbus-helper 'script_fu_guide_new))
(define gimp-image-get-parasite (loudbus-helper 'gimp_image_get_parasite))
(define plug-in-antialias (loudbus-helper 'plug_in_antialias))
(define gimp-airbrush-default (loudbus-helper 'gimp_airbrush_default))
(define gimp-gradient-segment-set-left-color (loudbus-helper 'gimp_gradient_segment_set_left_color))
(define gimp-heal (loudbus-helper 'gimp_heal))
(define gimp-item-delete (loudbus-helper 'gimp_item_delete))
(define gimp-drawable-thumbnail (loudbus-helper 'gimp_drawable_thumbnail))
(define script-fu-3dtruchet (loudbus-helper 'script_fu_3dtruchet))
(define file-aa-save (loudbus-helper 'file_aa_save))
(define gimp-selection-shrink (loudbus-helper 'gimp_selection_shrink))
(define gimp-drawable-foreground-extract (loudbus-helper 'gimp_drawable_foreground_extract))
(define plug-in-colors-channel-mixer (loudbus-helper 'plug_in_colors_channel_mixer))
(define gimp-text-layer-get-hint-style (loudbus-helper 'gimp_text_layer_get_hint_style))
(define gimp-image-undo-freeze (loudbus-helper 'gimp_image_undo_freeze))
(define gimp-palettes-refresh (loudbus-helper 'gimp_palettes_refresh))
(define gimp-image-duplicate (loudbus-helper 'gimp_image_duplicate))
(define gimp-layer-get-edit-mask (loudbus-helper 'gimp_layer_get_edit_mask))
(define gimp-image-get-channels (loudbus-helper 'gimp_image_get_channels))
(define python-fu-palette-sort (loudbus-helper 'python_fu_palette_sort))
(define file-gbr-save (loudbus-helper 'file_gbr_save))
(define script-fu-distress-selection (loudbus-helper 'script_fu_distress_selection))
(define plug-in-make-seamless (loudbus-helper 'plug_in_make_seamless))
(define gimp-perspective (loudbus-helper 'gimp_perspective))
(define gimp-progress-update (loudbus-helper 'gimp_progress_update))
(define gimp-context-set-palette (loudbus-helper 'gimp_context_set_palette))
(define plug-in-ripple (loudbus-helper 'plug_in_ripple))
(define gimp-help-2-using-web (loudbus-helper 'gimp_help_2_using_web))
(define plug-in-fractal-trace (loudbus-helper 'plug_in_fractal_trace))
(define gimp-gradient-segment-get-left-pos (loudbus-helper 'gimp_gradient_segment_get_left_pos))
(define gimp-fuzzy-select (loudbus-helper 'gimp_fuzzy_select))
(define script-fu-chalk-logo (loudbus-helper 'script_fu_chalk_logo))
(define file-fli-info (loudbus-helper 'file_fli_info))
(define gimp-drawable-fill (loudbus-helper 'gimp_drawable_fill))
(define gimp-gradient-segment-set-middle-pos (loudbus-helper 'gimp_gradient_segment_set_middle_pos))
(define gimp-context-get-sample-threshold-int (loudbus-helper 'gimp_context_get_sample_threshold_int))
(define gimp-item-transform-shear (loudbus-helper 'gimp_item_transform_shear))
(define gimp-context-get-interpolation (loudbus-helper 'gimp_context_get_interpolation))
(define script-fu-selection-rounded-rectangle (loudbus-helper 'script_fu_selection_rounded_rectangle))
(define gimp-floating-sel-relax (loudbus-helper 'gimp_floating_sel_relax))
(define gimp-image-get-channel-by-name (loudbus-helper 'gimp_image_get_channel_by_name))
(define gimp-text-layer-set-font-size (loudbus-helper 'gimp_text_layer_set_font_size))
(define plug-in-bump-map (loudbus-helper 'plug_in_bump_map))
(define gimp-fonts-popup (loudbus-helper 'gimp_fonts_popup))
(define plug-in-icc-profile-info (loudbus-helper 'plug_in_icc_profile_info))
(define file-sgi-save (loudbus-helper 'file_sgi_save))
(define gimp-layer-get-lock-alpha (loudbus-helper 'gimp_layer_get_lock_alpha))
(define gimp-edit-named-paste-as-new (loudbus-helper 'gimp_edit_named_paste_as_new))
(define gimp-brush-set-radius (loudbus-helper 'gimp_brush_set_radius))
(define gimp-vectors-stroke-translate (loudbus-helper 'gimp_vectors_stroke_translate))
(define gimp-xcf-save (loudbus-helper 'gimp_xcf_save))
(define gimp-selection-layer-alpha (loudbus-helper 'gimp_selection_layer_alpha))
(define gimp-palette-entry-set-color (loudbus-helper 'gimp_palette_entry_set_color))
(define gimp-image-get-vectors-by-name (loudbus-helper 'gimp_image_get_vectors_by_name))
(define gimp-color-balance (loudbus-helper 'gimp_color_balance))
(define gimp-vectors-stroke-close (loudbus-helper 'gimp_vectors_stroke_close))
(define plug-in-colorify (loudbus-helper 'plug_in_colorify))
(define script-fu-crystal-logo (loudbus-helper 'script_fu_crystal_logo))
(define gimp-brush-set-shape (loudbus-helper 'gimp_brush_set_shape))
(define script-fu-render-map (loudbus-helper 'script_fu_render_map))
(define gimp-selection-float (loudbus-helper 'gimp_selection_float))
(define file-psp-load (loudbus-helper 'file_psp_load))
(define gimp-context-set-transform-direction (loudbus-helper 'gimp_context_set_transform_direction))
(define file-dicom-load (loudbus-helper 'file_dicom_load))
(define gimp-fonts-close-popup (loudbus-helper 'gimp_fonts_close_popup))
(define file-fits-save (loudbus-helper 'file_fits_save))
(define gimp-item-set-name (loudbus-helper 'gimp_item_set_name))
(define gimp-palette-get-colors (loudbus-helper 'gimp_palette_get_colors))
(define file-pnm-load (loudbus-helper 'file_pnm_load))
(define plug-in-depth-merge (loudbus-helper 'plug_in_depth_merge))
(define gimp-drawable-mask-intersect (loudbus-helper 'gimp_drawable_mask_intersect))
(define file-print-gtk (loudbus-helper 'file_print_gtk))
(define gimp-item-transform-flip (loudbus-helper 'gimp_item_transform_flip))
(define gimp-image-is-dirty (loudbus-helper 'gimp_image_is_dirty))
(define gimp-layer-add-alpha (loudbus-helper 'gimp_layer_add_alpha))
(define plug-in-curve-bend-Iterator (loudbus-helper 'plug_in_curve_bend_Iterator))
(define gimp-patterns-get-list (loudbus-helper 'gimp_patterns_get_list))
(define gimp-brush-is-editable (loudbus-helper 'gimp_brush_is_editable))
(define gimp-brush-get-hardness (loudbus-helper 'gimp_brush_get_hardness))
(define gimp-drawable-mask-bounds (loudbus-helper 'gimp_drawable_mask_bounds))
(define script-fu-button00 (loudbus-helper 'script_fu_button00))
(define plug-in-whirl-pinch (loudbus-helper 'plug_in_whirl_pinch))
(define gimp-brushes-get-brush-data (loudbus-helper 'gimp_brushes_get_brush_data))
(define script-fu-selection-to-pattern (loudbus-helper 'script_fu_selection_to_pattern))
(define gimp-vectors-stroke-rotate (loudbus-helper 'gimp_vectors_stroke_rotate))
(define gimp-gradient-segment-range-blend-colors (loudbus-helper 'gimp_gradient_segment_range_blend_colors))
(define plug-in-randomize-slur (loudbus-helper 'plug_in_randomize_slur))
(define plug-in-colortoalpha (loudbus-helper 'plug_in_colortoalpha))
(define plug-in-sinus (loudbus-helper 'plug_in_sinus))
(define file-svg-load-thumb (loudbus-helper 'file_svg_load_thumb))
(define gimp-image-add-vguide (loudbus-helper 'gimp_image_add_vguide))
(define gimp-transform-2d (loudbus-helper 'gimp_transform_2d))
(define gimp-file-load-layers (loudbus-helper 'gimp_file_load_layers))
(define gimp-edit-stroke (loudbus-helper 'gimp_edit_stroke))
(define gimp-context-set-foreground (loudbus-helper 'gimp_context_set_foreground))
(define gimp-floating-sel-rigor (loudbus-helper 'gimp_floating_sel_rigor))
(define script-fu-chrome-logo (loudbus-helper 'script_fu_chrome_logo))
(define plug-in-gauss-iir (loudbus-helper 'plug_in_gauss_iir))
(define gimp-palette-duplicate (loudbus-helper 'gimp_palette_duplicate))
(define gimp-palette-delete-entry (loudbus-helper 'gimp_palette_delete_entry))
(define script-fu-beveled-pattern-arrow (loudbus-helper 'script_fu_beveled_pattern_arrow))
(define gimp-text-layer-get-font-size (loudbus-helper 'gimp_text_layer_get_font_size))
(define gimp-image-select-ellipse (loudbus-helper 'gimp_image_select_ellipse))
(define gimp-image-base-type (loudbus-helper 'gimp_image_base_type))
(define gimp-file-load (loudbus-helper 'gimp_file_load))
(define script-fu-neon-logo (loudbus-helper 'script_fu_neon_logo))
(define gimp-text-layer-get-indent (loudbus-helper 'gimp_text_layer_get_indent))
(define gimp-desaturate (loudbus-helper 'gimp_desaturate))
(define gimp-vectors-bezier-stroke-new-moveto (loudbus-helper 'gimp_vectors_bezier_stroke_new_moveto))
(define gimp-image-crop (loudbus-helper 'gimp_image_crop))
(define file-xwd-save (loudbus-helper 'file_xwd_save))
(define gimp-drawable-is-gray (loudbus-helper 'gimp_drawable_is_gray))
(define file-pat-save (loudbus-helper 'file_pat_save))
(define gimp-path-get-point-at-dist (loudbus-helper 'gimp_path_get_point_at_dist))
(define gimp-image-undo-group-start (loudbus-helper 'gimp_image_undo_group_start))
(define gimp-gradient-new (loudbus-helper 'gimp_gradient_new))
(define gimp-image-merge-visible-layers (loudbus-helper 'gimp_image_merge_visible_layers))
(define gimp-text-layer-set-hinting (loudbus-helper 'gimp_text_layer_set_hinting))
(define gimp-help-using-fileformats (loudbus-helper 'gimp_help_using_fileformats))
(define gimp-image-delete (loudbus-helper 'gimp_image_delete))
(define gimp-image-select-contiguous-color (loudbus-helper 'gimp_image_select_contiguous_color))
(define gimp-context-get-brush-angle (loudbus-helper 'gimp_context_get_brush_angle))
(define gimp-image-set-tattoo-state (loudbus-helper 'gimp_image_set_tattoo_state))
(define gimp-online-plug-in-web-site (loudbus-helper 'gimp_online_plug_in_web_site))
(define gimp-image-get-channel-by-tattoo (loudbus-helper 'gimp_image_get_channel_by_tattoo))
(define gimp-item-is-drawable (loudbus-helper 'gimp_item_is_drawable))
(define gimp-item-is-group (loudbus-helper 'gimp_item_is_group))
(define file-png-save2 (loudbus-helper 'file_png_save2))
(define gimp-item-is-text-layer (loudbus-helper 'gimp_item_is_text_layer))
(define gimp-pattern-get-info (loudbus-helper 'gimp_pattern_get_info))
(define file-gz-load (loudbus-helper 'file_gz_load))
(define gimp-image-undo-disable (loudbus-helper 'gimp_image_undo_disable))
(define gimp-item-is-selection (loudbus-helper 'gimp_item_is_selection))
(define script-fu-i26-gunya2 (loudbus-helper 'script_fu_i26_gunya2))
(define gimp-brush-get-pixels (loudbus-helper 'gimp_brush_get_pixels))
(define script-fu-comic-logo-alpha (loudbus-helper 'script_fu_comic_logo_alpha))
(define gimp-image-set-component-visible (loudbus-helper 'gimp_image_set_component_visible))
(define gimp-layer-get-mask (loudbus-helper 'gimp_layer_get_mask))
(define script-fu-frosty-logo-alpha (loudbus-helper 'script_fu_frosty_logo_alpha))
(define gimp-palette-export-python (loudbus-helper 'gimp_palette_export_python))
(define plug-in-flarefx (loudbus-helper 'plug_in_flarefx))
(define gimp-drawable-offsets (loudbus-helper 'gimp_drawable_offsets))
(define gimp-image-insert-vectors (loudbus-helper 'gimp_image_insert_vectors))
(define file-tga-load (loudbus-helper 'file_tga_load))
(define gimp-gimprc-query (loudbus-helper 'gimp_gimprc_query))
(define plug-in-gradmap (loudbus-helper 'plug_in_gradmap))
(define gimp-image-add-hguide (loudbus-helper 'gimp_image_add_hguide))
(define gimp-vectors-to-selection (loudbus-helper 'gimp_vectors_to_selection))
(define gimp-palette-get-columns (loudbus-helper 'gimp_palette_get_columns))
(define gimp-vectors-remove-stroke (loudbus-helper 'gimp_vectors_remove_stroke))
(define gimp-context-set-sample-merged (loudbus-helper 'gimp_context_set_sample_merged))
(define gimp-image-get-parasite-list (loudbus-helper 'gimp_image_get_parasite_list))
(define gimp-context-set-ink-blob-aspect-ratio (loudbus-helper 'gimp_context_set_ink_blob_aspect_ratio))
(define gimp-histogram (loudbus-helper 'gimp_histogram))
(define file-pbm-save (loudbus-helper 'file_pbm_save))
(define gimp-brushes-close-popup (loudbus-helper 'gimp_brushes_close_popup))
(define gimp-brushes-get-list (loudbus-helper 'gimp_brushes_get_list))
(define gimp-text-layer-set-language (loudbus-helper 'gimp_text_layer_set_language))
(define gimp-dynamics-refresh (loudbus-helper 'gimp_dynamics_refresh))
(define gimp-context-get-font (loudbus-helper 'gimp_context_get_font))
(define file-colorxhtml-save (loudbus-helper 'file_colorxhtml_save))
(define gimp-context-get-transform-resize (loudbus-helper 'gimp_context_get_transform_resize))
(define gimp-image-add-layer (loudbus-helper 'gimp_image_add_layer))
(define gimp-layer-resize-to-image-size (loudbus-helper 'gimp_layer_resize_to_image_size))
(define gimp-text-layer-set-color (loudbus-helper 'gimp_text_layer_set_color))
(define script-fu-chip-away-logo-alpha (loudbus-helper 'script_fu_chip_away_logo_alpha))
(define gimp-layer-create-mask (loudbus-helper 'gimp_layer_create_mask))
(define script-fu-set-cmap (loudbus-helper 'script_fu_set_cmap))
(define gimp-palettes-set-popup (loudbus-helper 'gimp_palettes_set_popup))
(define plug-in-dbbrowser (loudbus-helper 'plug_in_dbbrowser))
(define gimp-image-set-unit (loudbus-helper 'gimp_image_set_unit))
(define plug-in-sobel (loudbus-helper 'plug_in_sobel))
(define script-fu-swirl-tile (loudbus-helper 'script_fu_swirl_tile))
(define plug-in-decompose-registered (loudbus-helper 'plug_in_decompose_registered))
(define gimp-item-get-parasite (loudbus-helper 'gimp_item_get_parasite))
(define gimp-image-select-color (loudbus-helper 'gimp_image_select_color))
(define file-xpm-save (loudbus-helper 'file_xpm_save))
(define RamServer (loudbus-helper 'RamServer))
(define script-fu-guides-remove (loudbus-helper 'script_fu_guides_remove))
(define gimp-image-convert-indexed (loudbus-helper 'gimp_image_convert_indexed))
(define plug-in-icc-profile-apply (loudbus-helper 'plug_in_icc_profile_apply))
(define file-jpeg-load (loudbus-helper 'file_jpeg_load))
(define gimp-channel-get-opacity (loudbus-helper 'gimp_channel_get_opacity))
(define gimp-context-get-ink-angle (loudbus-helper 'gimp_context_get_ink_angle))
(define gimp-help (loudbus-helper 'gimp_help))
(define gimp-layer-set-offsets (loudbus-helper 'gimp_layer_set_offsets))
(define file-wmf-load (loudbus-helper 'file_wmf_load))
(define gimp-buffers-get-list (loudbus-helper 'gimp_buffers_get_list))
(define gimp-context-get-ink-tilt-sensitivity (loudbus-helper 'gimp_context_get_ink_tilt_sensitivity))
(define gimp-channel-combine-masks (loudbus-helper 'gimp_channel_combine_masks))
(define gimp-version (loudbus-helper 'gimp_version))
(define gimp-image-flatten (loudbus-helper 'gimp_image_flatten))
(define gimp-eraser-default (loudbus-helper 'gimp_eraser_default))
(define gimp-plugin-get-pdb-error-handler (loudbus-helper 'gimp_plugin_get_pdb_error_handler))
(define plug-in-exchange (loudbus-helper 'plug_in_exchange))
(define plug-in-icc-profile-file-info (loudbus-helper 'plug_in_icc_profile_file_info))
(define gimp-context-set-brush-angle (loudbus-helper 'gimp_context_set_brush_angle))
(define file-glob (loudbus-helper 'file_glob))
(define gimp-floating-sel-remove (loudbus-helper 'gimp_floating_sel_remove))
(define file-xmc-load-thumb (loudbus-helper 'file_xmc_load_thumb))
(define gimp-patterns-refresh (loudbus-helper 'gimp_patterns_refresh))
(define file-psd-save (loudbus-helper 'file_psd_save))
(define plug-in-blinds (loudbus-helper 'plug_in_blinds))
(define gimp-context-get-gradient (loudbus-helper 'gimp_context_get_gradient))
(define gimp-image-get-filename (loudbus-helper 'gimp_image_get_filename))
(define script-fu-frosty-logo (loudbus-helper 'script_fu_frosty_logo))
(define plug-in-nlfilt (loudbus-helper 'plug_in_nlfilt))
(define gimp-image-convert-rgb (loudbus-helper 'gimp_image_convert_rgb))
(define script-fu-land (loudbus-helper 'script_fu_land))
(define script-fu-guides-from-selection (loudbus-helper 'script_fu_guides_from_selection))
(define gimp-text-layer-get-antialias (loudbus-helper 'gimp_text_layer_get_antialias))
(define file-pdf-save (loudbus-helper 'file_pdf_save))
(define gimp-vectors-new (loudbus-helper 'gimp_vectors_new))
(define gimp-display-delete (loudbus-helper 'gimp_display_delete))
(define gimp-brush-rename (loudbus-helper 'gimp_brush_rename))
(define gimp-context-get-background (loudbus-helper 'gimp_context_get_background))
(define gimp-path-set-current (loudbus-helper 'gimp_path_set_current))
(define gimp-image-grid-get-foreground-color (loudbus-helper 'gimp_image_grid_get_foreground_color))
(define file-openraster-load (loudbus-helper 'file_openraster_load))
(define gimp-item-is-layer-mask (loudbus-helper 'gimp_item_is_layer_mask))
(define gimp-edit-blend (loudbus-helper 'gimp_edit_blend))
(define plug-in-oilify-enhanced (loudbus-helper 'plug_in_oilify_enhanced))
(define file-gif-load-thumb (loudbus-helper 'file_gif_load_thumb))
(define plug-in-metadata-set (loudbus-helper 'plug_in_metadata_set))
(define gimp-brushes-get-spacing (loudbus-helper 'gimp_brushes_get_spacing))
(define gimp-gradients-get-list (loudbus-helper 'gimp_gradients_get_list))
(define plug-in-fractalexplorer (loudbus-helper 'plug_in_fractalexplorer))
(define plug-in-illusion (loudbus-helper 'plug_in_illusion))
(define gimp-selection-translate (loudbus-helper 'gimp_selection_translate))
(define script-fu-round-button (loudbus-helper 'script_fu_round_button))
(define file-ico-save (loudbus-helper 'file_ico_save))
(define gimp-unit-get-number-of-built-in-units (loudbus-helper 'gimp_unit_get_number_of_built_in_units))
(define gimp-levels-stretch (loudbus-helper 'gimp_levels_stretch))
(define file-png-set-defaults (loudbus-helper 'file_png_set_defaults))
(define gimp-item-get-lock-content (loudbus-helper 'gimp_item_get_lock_content))
(define gimp-selection-save (loudbus-helper 'gimp_selection_save))
(define gimp-vectors-stroke-flip-free (loudbus-helper 'gimp_vectors_stroke_flip_free))
(define script-fu-alien-glow-button (loudbus-helper 'script_fu_alien_glow_button))
(define gimp-palette-export-css (loudbus-helper 'gimp_palette_export_css))
(define gimp-register-thumbnail-loader (loudbus-helper 'gimp_register_thumbnail_loader))
(define gimp-context-set-ink-tilt-sensitivity (loudbus-helper 'gimp_context_set_ink_tilt_sensitivity))
(define plug-in-metadata-encode-xmp (loudbus-helper 'plug_in_metadata_encode_xmp))
(define gimp-gradient-segment-get-left-color (loudbus-helper 'gimp_gradient_segment_get_left_color))
(define plug-in-blur (loudbus-helper 'plug_in_blur))
(define gimp-eraser (loudbus-helper 'gimp_eraser))
(define plug-in-retinex (loudbus-helper 'plug_in_retinex))
(define script-fu-tube-subbutton-label-gimp-org (loudbus-helper 'script_fu_tube_subbutton_label_gimp_org))
(define gimp-vectors-get-strokes (loudbus-helper 'gimp_vectors_get_strokes))
(define file-pdf-load-thumb (loudbus-helper 'file_pdf_load_thumb))
(define gimp-help-using-web (loudbus-helper 'gimp_help_using_web))
(define gimp-image-set-filename (loudbus-helper 'gimp_image_set_filename))
(define plug-in-plug-in-details (loudbus-helper 'plug_in_plug_in_details))
(define file-eps-load (loudbus-helper 'file_eps_load))
(define gimp-paintbrush-default (loudbus-helper 'gimp_paintbrush_default))
(define gimp-context-set-brush-default-size (loudbus-helper 'gimp_context_set_brush_default_size))
(define plug-in-script-fu-eval (loudbus-helper 'plug_in_script_fu_eval))
(define gimp-patterns-get-pattern-data (loudbus-helper 'gimp_patterns_get_pattern_data))
(define gimp-drawable-has-alpha (loudbus-helper 'gimp_drawable_has_alpha))
(define plug-in-small-tiles (loudbus-helper 'plug_in_small_tiles))
(define file-ps-load (loudbus-helper 'file_ps_load))
(define gimp-image-grid-get-style (loudbus-helper 'gimp_image_grid_get_style))
(define plug-in-solid-noise (loudbus-helper 'plug_in_solid_noise))
(define gimp-context-get-brush (loudbus-helper 'gimp_context_get_brush))
(define gimp-brush-delete (loudbus-helper 'gimp_brush_delete))
(define gimp-gradient-segment-get-blending-function (loudbus-helper 'gimp_gradient_segment_get_blending_function))
(define gimp-brushes-popup (loudbus-helper 'gimp_brushes_popup))
(define script-fu-beveled-pattern-bullet (loudbus-helper 'script_fu_beveled_pattern_bullet))
(define gimp-image-find-next-guide (loudbus-helper 'gimp_image_find_next_guide))
(define gimp-image-width (loudbus-helper 'gimp_image_width))
(define plug-in-icc-profile-set (loudbus-helper 'plug_in_icc_profile_set))
(define plug-in-plasma (loudbus-helper 'plug_in_plasma))
(define gimp-layer-group-new (loudbus-helper 'gimp_layer_group_new))
(define gimp-online-main-web-site (loudbus-helper 'gimp_online_main_web_site))
(define gimp-gradient-get-custom-samples (loudbus-helper 'gimp_gradient_get_custom_samples))
(define plug-in-applylens (loudbus-helper 'plug_in_applylens))
(define gimp-layer-set-edit-mask (loudbus-helper 'gimp_layer_set_edit_mask))
(define gimp-image-grid-set-spacing (loudbus-helper 'gimp_image_grid_set_spacing))
(define gimp-gradient-segment-range-split-uniform (loudbus-helper 'gimp_gradient_segment_range_split_uniform))
(define file-raw-load (loudbus-helper 'file_raw_load))
(define gimp-gradient-segment-range-flip (loudbus-helper 'gimp_gradient_segment_range_flip))
(define gimp-image-set-resolution (loudbus-helper 'gimp_image_set_resolution))
(define plug-in-max-rgb (loudbus-helper 'plug_in_max_rgb))
(define plug-in-dog (loudbus-helper 'plug_in_dog))
(define file-gih-load (loudbus-helper 'file_gih_load))
(define plug-in-newsprint (loudbus-helper 'plug_in_newsprint))
(define script-fu-basic2-logo (loudbus-helper 'script_fu_basic2_logo))
(define gimp-plugin-menu-branch-register (loudbus-helper 'gimp_plugin_menu_branch_register))
(define script-fu-cool-metal-logo-alpha (loudbus-helper 'script_fu_cool_metal_logo_alpha))
(define gimp-message-set-handler (loudbus-helper 'gimp_message_set_handler))
(define gimp-image-get-active-layer (loudbus-helper 'gimp_image_get_active_layer))
(define plug-in-erode (loudbus-helper 'plug_in_erode))
(define file-dicom-save (loudbus-helper 'file_dicom_save))
(define gimp-message (loudbus-helper 'gimp_message))
(define plug-in-autocrop (loudbus-helper 'plug_in_autocrop))
(define file-pnm-save (loudbus-helper 'file_pnm_save))
(define script-fu-glossy-logo-alpha (loudbus-helper 'script_fu_glossy_logo_alpha))
(define gimp-channel-set-color (loudbus-helper 'gimp_channel_set_color))
(define gimp-vectors-bezier-stroke-lineto (loudbus-helper 'gimp_vectors_bezier_stroke_lineto))
(define gimp-brushes-set-popup (loudbus-helper 'gimp_brushes_set_popup))
(define gimp-patterns-get-pattern (loudbus-helper 'gimp_patterns_get_pattern))
(define gimp-brushes-refresh (loudbus-helper 'gimp_brushes_refresh))
(define plug-in-pagecurl (loudbus-helper 'plug_in_pagecurl))
(define gimp-gradient-segment-range-replicate (loudbus-helper 'gimp_gradient_segment_range_replicate))
(define gimp-context-set-pattern (loudbus-helper 'gimp_context_set_pattern))
(define plug-in-maze (loudbus-helper 'plug_in_maze))
(define gimp-pattern-get-pixels (loudbus-helper 'gimp_pattern_get_pixels))
(define gimp-selection-border (loudbus-helper 'gimp_selection_border))
(define plug-in-randomize-pick (loudbus-helper 'plug_in_randomize_pick))
(define gimp-quit (loudbus-helper 'gimp_quit))
(define script-fu-paste-as-brush (loudbus-helper 'script_fu_paste_as_brush))
(define gimp-plugin-menu-register (loudbus-helper 'gimp_plugin_menu_register))
(define plug-in-hsv-noise (loudbus-helper 'plug_in_hsv_noise))
(define plug-in-alienmap2 (loudbus-helper 'plug_in_alienmap2))
(define script-fu-chip-away-logo (loudbus-helper 'script_fu_chip_away_logo))
(define gimp-drawable-transform-rotate (loudbus-helper 'gimp_drawable_transform_rotate))
(define gimp-invert (loudbus-helper 'gimp_invert))
(define gimp-image-scale (loudbus-helper 'gimp_image_scale))
(define gimp-gradients-close-popup (loudbus-helper 'gimp_gradients_close_popup))
(define gimp-context-get-foreground (loudbus-helper 'gimp_context_get_foreground))
(define plug-in-diffraction (loudbus-helper 'plug_in_diffraction))
(define file-xmc-load (loudbus-helper 'file_xmc_load))
(define gimp-vectors-stroke-interpolate (loudbus-helper 'gimp_vectors_stroke_interpolate))
(define script-fu-selection-to-brush (loudbus-helper 'script_fu_selection_to_brush))
(define plug-in-gauss-iir2 (loudbus-helper 'plug_in_gauss_iir2))
(define plug-in-scatter-hsv (loudbus-helper 'plug_in_scatter_hsv))
(define script-fu-basic2-logo-alpha (loudbus-helper 'script_fu_basic2_logo_alpha))
(define script-fu-make-brush-elliptical-feathered (loudbus-helper 'script_fu_make_brush_elliptical_feathered))
(define file-desktop-link-load (loudbus-helper 'file_desktop_link_load))
(define script-fu-lava (loudbus-helper 'script_fu_lava))
(define gimp-item-set-lock-content (loudbus-helper 'gimp_item_set_lock_content))
(define gimp-text-layer-set-indent (loudbus-helper 'gimp_text_layer_set_indent))
(define gimp-brightness-contrast (loudbus-helper 'gimp_brightness_contrast))
(define gimp-register-save-handler (loudbus-helper 'gimp_register_save_handler))
(define gimp-image-get-tattoo-state (loudbus-helper 'gimp_image_get_tattoo_state))
(define gimp-file-save (loudbus-helper 'gimp_file_save))
(define file-pix-load (loudbus-helper 'file_pix_load))
(define gimp-context-set-transform-resize (loudbus-helper 'gimp_context_set_transform_resize))
(define plug-in-video (loudbus-helper 'plug_in_video))
(define gimp-dodgeburn-default (loudbus-helper 'gimp_dodgeburn_default))
(define script-fu-reverse-layers (loudbus-helper 'script_fu_reverse_layers))
(define plug-in-gauss-rle2 (loudbus-helper 'plug_in_gauss_rle2))
(define gimp-path-get-locked (loudbus-helper 'gimp_path_get_locked))
(define plug-in-nova (loudbus-helper 'plug_in_nova))
(define gimp-image-pick-color (loudbus-helper 'gimp_image_pick_color))
(define plug-in-cartoon (loudbus-helper 'plug_in_cartoon))
(define gimp-path-list (loudbus-helper 'gimp_path_list))
(define gimp-text-fontname (loudbus-helper 'gimp_text_fontname))
(define gimp-channel-new-from-component (loudbus-helper 'gimp_channel_new_from_component))
(define gimp-drawable-height (loudbus-helper 'gimp_drawable_height))
(define gimp-image-detach-parasite (loudbus-helper 'gimp_image_detach_parasite))
(define gimp-context-get-ink-size-sensitivity (loudbus-helper 'gimp_context_get_ink_size_sensitivity))
(define file-fli-load (loudbus-helper 'file_fli_load))
(define gimp-fonts-refresh (loudbus-helper 'gimp_fonts_refresh))
(define script-fu-big-header-gimp-org (loudbus-helper 'script_fu_big_header_gimp_org))
(define plug-in-rotate (loudbus-helper 'plug_in_rotate))
(define plug-in-deinterlace (loudbus-helper 'plug_in_deinterlace))
(define script-fu-alien-neon-logo-alpha (loudbus-helper 'script_fu_alien_neon_logo_alpha))
(define gimp-unit-get-deletion-flag (loudbus-helper 'gimp_unit_get_deletion_flag))
(define gimp-text-layer-get-font (loudbus-helper 'gimp_text_layer_get_font))
(define gimp-drawable-transform-perspective-default (loudbus-helper 'gimp_drawable_transform_perspective_default))
(define plug-in-grid (loudbus-helper 'plug_in_grid))
(define gimp-layer-get-mode (loudbus-helper 'gimp_layer_get_mode))
(define file-gz-save (loudbus-helper 'file_gz_save))
(define gimp-buffer-get-width (loudbus-helper 'gimp_buffer_get_width))
(define plug-in-gauss (loudbus-helper 'plug_in_gauss))
(define gimp-help-2-concepts-paths (loudbus-helper 'gimp_help_2_concepts_paths))
(define script-fu-glowing-logo-alpha (loudbus-helper 'script_fu_glowing_logo_alpha))
(define script-fu-small-header-gimp-org (loudbus-helper 'script_fu_small_header_gimp_org))
(define gimp-getpid (loudbus-helper 'gimp_getpid))
(define gimp-edit-cut (loudbus-helper 'gimp_edit_cut))
(define gimp-ellipse-select (loudbus-helper 'gimp_ellipse_select))
(define gimp-text-layer-set-text (loudbus-helper 'gimp_text_layer_set_text))
(define gimp-context-get-antialias (loudbus-helper 'gimp_context_get_antialias))
(define file-tga-save (loudbus-helper 'file_tga_save))
(define gimp-image-get-uri (loudbus-helper 'gimp_image_get_uri))
(define plug-in-dilate (loudbus-helper 'plug_in_dilate))
(define gimp-context-set-sample-threshold (loudbus-helper 'gimp_context_set_sample_threshold))
(define gimp-palettes-popup (loudbus-helper 'gimp_palettes_popup))
(define gimp-drawable-get-pixel (loudbus-helper 'gimp_drawable_get_pixel))
(define gimp-text-layer-resize (loudbus-helper 'gimp_text_layer_resize))
(define extension-script-fu (loudbus-helper 'extension_script_fu))
(define gimp-layer-set-apply-mask (loudbus-helper 'gimp_layer_set_apply_mask))
(define plug-in-imagemap (loudbus-helper 'plug_in_imagemap))
(define gimp-drawable-type (loudbus-helper 'gimp_drawable_type))
(define plug-in-borderaverage (loudbus-helper 'plug_in_borderaverage))
(define gimp-image-get-active-drawable (loudbus-helper 'gimp_image_get_active_drawable))
(define gimp-procedural-db-set-data (loudbus-helper 'gimp_procedural_db_set_data))
(define gimp-palette-export-java (loudbus-helper 'gimp_palette_export_java))
(define gimp-gradient-duplicate (loudbus-helper 'gimp_gradient_duplicate))
(define script-fu-neon-logo-alpha (loudbus-helper 'script_fu_neon_logo_alpha))
(define gimp-image-grid-get-background-color (loudbus-helper 'gimp_image_grid_get_background_color))
(define plug-in-cubism (loudbus-helper 'plug_in_cubism))
(define gimp-image-grid-get-offset (loudbus-helper 'gimp_image_grid_get_offset))
(define gimp-register-file-handler-mime (loudbus-helper 'gimp_register_file_handler_mime))
(define gimp-display-new (loudbus-helper 'gimp_display_new))
(define gimp-palette-entry-set-name (loudbus-helper 'gimp_palette_entry_set_name))
(define gimp-image-grid-set-foreground-color (loudbus-helper 'gimp_image_grid_set_foreground_color))
(define plug-in-script-fu-server (loudbus-helper 'plug_in_script_fu_server))
(define file-ps-load-thumb (loudbus-helper 'file_ps_load_thumb))
(define gimp-image-remove-layer-mask (loudbus-helper 'gimp_image_remove_layer_mask))
(define script-fu-truchet (loudbus-helper 'script_fu_truchet))
(define gimp-rotate (loudbus-helper 'gimp_rotate))
(define gimp-get-theme-dir (loudbus-helper 'gimp_get_theme_dir))
(define gimp-context-set-ink-size-sensitivity (loudbus-helper 'gimp_context_set_ink_size_sensitivity))
(define gimp-image-set-active-layer (loudbus-helper 'gimp_image_set_active_layer))
(define script-fu-camo-pattern (loudbus-helper 'script_fu_camo_pattern))
(define file-uri-load (loudbus-helper 'file_uri_load))
(define gimp-hue-saturation (loudbus-helper 'gimp_hue_saturation))
(define gimp-layer-new (loudbus-helper 'gimp_layer_new))
(define script-fu-gradient-bevel-logo (loudbus-helper 'script_fu_gradient_bevel_logo))
(define plug-in-bump-map-tiled (loudbus-helper 'plug_in_bump_map_tiled))
(define gimp-brush-set-spacing (loudbus-helper 'gimp_brush_set_spacing))
(define gimp-image-remove-vectors (loudbus-helper 'gimp_image_remove_vectors))
(define plug-in-displace-polar (loudbus-helper 'plug_in_displace_polar))
(define gimp-plugin-help-register (loudbus-helper 'gimp_plugin_help_register))
(define gimp-item-set-visible (loudbus-helper 'gimp_item_set_visible))
(define gimp-file-save-thumbnail (loudbus-helper 'gimp_file_save_thumbnail))
(define script-fu-drop-shadow (loudbus-helper 'script_fu_drop_shadow))
(define script-fu-erase-rows (loudbus-helper 'script_fu_erase_rows))
(define gimp-gradient-segment-get-right-color (loudbus-helper 'gimp_gradient_segment_get_right_color))
(define file-jpeg-save (loudbus-helper 'file_jpeg_save))
(define gimp-plugins-query (loudbus-helper 'gimp_plugins_query))
(define gimp-procedural-db-query (loudbus-helper 'gimp_procedural_db_query))
(define gimp-text-layer-get-language (loudbus-helper 'gimp_text_layer_get_language))
(define file-bmp-load (loudbus-helper 'file_bmp_load))
(define gimp-message-get-handler (loudbus-helper 'gimp_message_get_handler))
(define gimp-unit-get-singular (loudbus-helper 'gimp_unit_get_singular))
(define plug-in-oilify (loudbus-helper 'plug_in_oilify))
(define file-ps-load-setargs (loudbus-helper 'file_ps_load_setargs))
(define gimp-drawable-transform-2d-default (loudbus-helper 'gimp_drawable_transform_2d_default))
(define plug-in-mosaic (loudbus-helper 'plug_in_mosaic))
(define gimp-item-get-parasite-list (loudbus-helper 'gimp_item_get_parasite_list))
(define gimp-drawable-offset (loudbus-helper 'gimp_drawable_offset))
(define plug-in-filter-pack (loudbus-helper 'plug_in_filter_pack))
(define plug-in-guillotine (loudbus-helper 'plug_in_guillotine))
(define plug-in-hot (loudbus-helper 'plug_in_hot))
(define gimp-image-rotate (loudbus-helper 'gimp_image_rotate))
(define file-xbm-load (loudbus-helper 'file_xbm_load))
(define plug-in-warp (loudbus-helper 'plug_in_warp))
(define gimp-buffer-get-image-type (loudbus-helper 'gimp_buffer_get_image_type))
(define plug-in-screenshot (loudbus-helper 'plug_in_screenshot))
(define gimp-context-set-brush-size (loudbus-helper 'gimp_context_set_brush_size))
(define gimp-gradient-segment-range-blend-opacity (loudbus-helper 'gimp_gradient_segment_range_blend_opacity))
(define gimp-image-reorder-item (loudbus-helper 'gimp_image_reorder_item))
(define plug-in-jigsaw (loudbus-helper 'plug_in_jigsaw))
(define gimp-drawable-width (loudbus-helper 'gimp_drawable_width))
(define gimp-drawable-transform-matrix (loudbus-helper 'gimp_drawable_transform_matrix))
(define file-gif-load (loudbus-helper 'file_gif_load))
(define gimp-edit-stroke-vectors (loudbus-helper 'gimp_edit_stroke_vectors))
(define gimp-context-set-ink-angle (loudbus-helper 'gimp_context_set_ink_angle))
(define script-fu-clothify (loudbus-helper 'script_fu_clothify))
(define gimp-image-get-component-active (loudbus-helper 'gimp_image_get_component_active))
(define file-pdf-save-multi (loudbus-helper 'file_pdf_save_multi))
(define plug-in-metadata-get-simple (loudbus-helper 'plug_in_metadata_get_simple))
(define script-fu-ripply-anim (loudbus-helper 'script_fu_ripply_anim))
(define file-openraster-save (loudbus-helper 'file_openraster_save))
(define gimp-displays-reconnect (loudbus-helper 'gimp_displays_reconnect))
(define gimp-curves-explicit (loudbus-helper 'gimp_curves_explicit))
(define gimp-selection-none (loudbus-helper 'gimp_selection_none))
(define gimp-floating-sel-anchor (loudbus-helper 'gimp_floating_sel_anchor))
(define script-fu-glossy-logo (loudbus-helper 'script_fu_glossy_logo))
(define script-fu-sota-chrome-logo (loudbus-helper 'script_fu_sota_chrome_logo))
(define plug-in-vinvert (loudbus-helper 'plug_in_vinvert))
(define gimp-brush-get-spikes (loudbus-helper 'gimp_brush_get_spikes))
(define gimp-context-set-opacity (loudbus-helper 'gimp_context_set_opacity))
(define plug-in-the-slimy-egg (loudbus-helper 'plug_in_the_slimy_egg))
(define plug-in-checkerboard (loudbus-helper 'plug_in_checkerboard))
(define gimp-image-get-name (loudbus-helper 'gimp_image_get_name))
(define plug-in-lic (loudbus-helper 'plug_in_lic))
(define gimp-channel-new (loudbus-helper 'gimp_channel_new))
(define gimp-procedural-db-dump (loudbus-helper 'gimp_procedural_db_dump))
(define gimp-image-get-exported-uri (loudbus-helper 'gimp_image_get_exported_uri))
(define gimp-gradient-segment-range-move (loudbus-helper 'gimp_gradient_segment_range_move))
(define script-fu-old-photo (loudbus-helper 'script_fu_old_photo))
(define gimp-vectors-stroke-get-length (loudbus-helper 'gimp_vectors_stroke_get_length))
(define plug-in-metadata-decode-xmp (loudbus-helper 'plug_in_metadata_decode_xmp))
(define gimp-drawable-transform-scale-default (loudbus-helper 'gimp_drawable_transform_scale_default))
(define gimp-gradient-segment-get-coloring-type (loudbus-helper 'gimp_gradient_segment_get_coloring_type))
(define gimp-progress-set-text (loudbus-helper 'gimp_progress_set_text))
(define gimp-gradients-popup (loudbus-helper 'gimp_gradients_popup))
(define gimp-context-get-transform-direction (loudbus-helper 'gimp_context_get_transform_direction))
(define gimp-unit-new (loudbus-helper 'gimp_unit_new))
(define script-fu-cool-metal-logo (loudbus-helper 'script_fu_cool_metal_logo))
(define script-fu-waves-anim (loudbus-helper 'script_fu_waves_anim))
(define plug-in-icc-profile-set-rgb (loudbus-helper 'plug_in_icc_profile_set_rgb))
(define plug-in-red-eye-removal (loudbus-helper 'plug_in_red_eye_removal))
(define gimp-help-2-using-docks (loudbus-helper 'gimp_help_2_using_docks))
(define gimp-scale (loudbus-helper 'gimp_scale))
(define gimp-context-get-palette (loudbus-helper 'gimp_context_get_palette))
(define gimp-drawable-transform-flip-default (loudbus-helper 'gimp_drawable_transform_flip_default))
(define gimp-text-layer-set-antialias (loudbus-helper 'gimp_text_layer_set_antialias))
(define plug-in-normalize (loudbus-helper 'plug_in_normalize))
(define gimp-layer-copy (loudbus-helper 'gimp_layer_copy))
(define plug-in-lighting (loudbus-helper 'plug_in_lighting))
(define script-fu-burn-in-anim (loudbus-helper 'script_fu_burn_in_anim))
(define gimp-layer-set-opacity (loudbus-helper 'gimp_layer_set_opacity))
(define gimp-context-get-ink-speed-sensitivity (loudbus-helper 'gimp_context_get_ink_speed_sensitivity))
(define gimp-image-insert-layer (loudbus-helper 'gimp_image_insert_layer))
(define gimp-file-load-thumbnail (loudbus-helper 'gimp_file_load_thumbnail))
(define gimp-procedural-db-temp-name (loudbus-helper 'gimp_procedural_db_temp_name))
(define gimp-get-path-by-tattoo (loudbus-helper 'gimp_get_path_by_tattoo))
(define file-eps-save (loudbus-helper 'file_eps_save))
(define gimp-edit-bucket-fill (loudbus-helper 'gimp_edit_bucket_fill))
(define script-fu-alien-glow-horizontal-ruler (loudbus-helper 'script_fu_alien_glow_horizontal_ruler))
(define gimp-channel-get-name (loudbus-helper 'gimp_channel_get_name))
(define gimp-gradients-get-active (loudbus-helper 'gimp_gradients_get_active))
(define gimp-channel-set-tattoo (loudbus-helper 'gimp_channel_set_tattoo))
(define gimp-palette-get-foreground (loudbus-helper 'gimp_palette_get_foreground))
(define gimp-image-get-cmap (loudbus-helper 'gimp_image_get_cmap))
(define gimp-layer-set-linked (loudbus-helper 'gimp_layer_set_linked))
(define gimp-drawable-parasite-find (loudbus-helper 'gimp_drawable_parasite_find))
(define gimp-brushes-list (loudbus-helper 'gimp_brushes_list))
(define gimp-image-lower-channel (loudbus-helper 'gimp_image_lower_channel))
(define gimp-patterns-set-pattern (loudbus-helper 'gimp_patterns_set_pattern))
(define gimp-convert-grayscale (loudbus-helper 'gimp_convert_grayscale))
(define gimp-drawable-parasite-detach (loudbus-helper 'gimp_drawable_parasite_detach))
(define gimp-drawable-set-name (loudbus-helper 'gimp_drawable_set_name))
(define gimp-image-lower-vectors-to-bottom (loudbus-helper 'gimp_image_lower_vectors_to_bottom))
(define gimp-parasite-attach (loudbus-helper 'gimp_parasite_attach))
(define gimp-vectors-set-tattoo (loudbus-helper 'gimp_vectors_set_tattoo))
(define gimp-drawable-set-linked (loudbus-helper 'gimp_drawable_set_linked))
(define gimp-channel-ops-duplicate (loudbus-helper 'gimp_channel_ops_duplicate))
(define gimp-vectors-get-linked (loudbus-helper 'gimp_vectors_get_linked))
(define gimp-vectors-get-tattoo (loudbus-helper 'gimp_vectors_get_tattoo))
(define gimp-vectors-parasite-attach (loudbus-helper 'gimp_vectors_parasite_attach))
(define gimp-image-active-drawable (loudbus-helper 'gimp_image_active_drawable))
(define gimp-convert-indexed (loudbus-helper 'gimp_convert_indexed))
(define gimp-drawable-get-name (loudbus-helper 'gimp_drawable_get_name))
(define gimp-layer-get-visible (loudbus-helper 'gimp_layer_get_visible))
(define gimp-palettes-set-palette (loudbus-helper 'gimp_palettes_set_palette))
(define gimp-crop (loudbus-helper 'gimp_crop))
(define gimp-drawable-get-image (loudbus-helper 'gimp_drawable_get_image))
(define gimp-drawable-is-layer (loudbus-helper 'gimp_drawable_is_layer))
(define gimp-palette-get-background (loudbus-helper 'gimp_palette_get_background))
(define gimp-undo-push-group-start (loudbus-helper 'gimp_undo_push_group_start))
(define gimp-palette-swap-colors (loudbus-helper 'gimp_palette_swap_colors))
(define gimp-layer-mask (loudbus-helper 'gimp_layer_mask))
(define gimp-layer-get-preserve-trans (loudbus-helper 'gimp_layer_get_preserve_trans))
(define gimp-drawable-parasite-list (loudbus-helper 'gimp_drawable_parasite_list))
(define gimp-image-get-channel-position (loudbus-helper 'gimp_image_get_channel_position))
(define gimp-drawable-is-valid (loudbus-helper 'gimp_drawable_is_valid))
(define gimp-gradients-set-active (loudbus-helper 'gimp_gradients_set_active))
(define gimp-channel-delete (loudbus-helper 'gimp_channel_delete))
(define gimp-vectors-get-visible (loudbus-helper 'gimp_vectors_get_visible))
(define gimp-layer-get-tattoo (loudbus-helper 'gimp_layer_get_tattoo))
(define gimp-image-floating-selection (loudbus-helper 'gimp_image_floating_selection))
(define gimp-vectors-get-image (loudbus-helper 'gimp_vectors_get_image))
(define gimp-image-raise-layer (loudbus-helper 'gimp_image_raise_layer))
(define gimp-brushes-set-paint-mode (loudbus-helper 'gimp_brushes_set_paint_mode))
(define gimp-drawable-parasite-attach (loudbus-helper 'gimp_drawable_parasite_attach))
(define gimp-blend (loudbus-helper 'gimp_blend))
(define gimp-gradients-set-gradient (loudbus-helper 'gimp_gradients_set_gradient))
(define gimp-selection-clear (loudbus-helper 'gimp_selection_clear))
(define gimp-channel-set-visible (loudbus-helper 'gimp_channel_set_visible))
(define gimp-temp-PDB-name (loudbus-helper 'gimp_temp_PDB_name))
(define gimp-image-get-vectors-position (loudbus-helper 'gimp_image_get_vectors_position))
(define gimp-palette-set-foreground (loudbus-helper 'gimp_palette_set_foreground))
(define gimp-image-parasite-list (loudbus-helper 'gimp_image_parasite_list))
(define gimp-layer-set-tattoo (loudbus-helper 'gimp_layer_set_tattoo))
(define gimp-drawable-set-visible (loudbus-helper 'gimp_drawable_set_visible))
(define gimp-drawable-get-tattoo (loudbus-helper 'gimp_drawable_get_tattoo))
(define gimp-image-raise-channel (loudbus-helper 'gimp_image_raise_channel))
(define gimp-image-lower-layer (loudbus-helper 'gimp_image_lower_layer))
(define gimp-brushes-set-opacity (loudbus-helper 'gimp_brushes_set_opacity))
(define gimp-vectors-set-visible (loudbus-helper 'gimp_vectors_set_visible))
(define gimp-palette-set-default-colors (loudbus-helper 'gimp_palette_set_default_colors))
(define gimp-convert-rgb (loudbus-helper 'gimp_convert_rgb))
(define gimp-channel-get-visible (loudbus-helper 'gimp_channel_get_visible))
(define gimp-image-raise-vectors-to-top (loudbus-helper 'gimp_image_raise_vectors_to_top))
(define gimp-vectors-is-valid (loudbus-helper 'gimp_vectors_is_valid))
(define gimp-drawable-get-visible (loudbus-helper 'gimp_drawable_get_visible))
(define gimp-image-lower-vectors (loudbus-helper 'gimp_image_lower_vectors))
(define gimp-vectors-parasite-find (loudbus-helper 'gimp_vectors_parasite_find))
(define gimp-brushes-get-opacity (loudbus-helper 'gimp_brushes_get_opacity))
(define gimp-image-parasite-find (loudbus-helper 'gimp_image_parasite_find))
(define gimp-drawable-is-text-layer (loudbus-helper 'gimp_drawable_is_text_layer))
(define gimp-image-raise-layer-to-top (loudbus-helper 'gimp_image_raise_layer_to_top))
(define gimp-layer-set-name (loudbus-helper 'gimp_layer_set_name))
(define gimp-layer-set-preserve-trans (loudbus-helper 'gimp_layer_set_preserve_trans))
(define gimp-layer-delete (loudbus-helper 'gimp_layer_delete))
(define gimp-channel-ops-offset (loudbus-helper 'gimp_channel_ops_offset))
(define gimp-parasite-list (loudbus-helper 'gimp_parasite_list))
(define gimp-channel-get-tattoo (loudbus-helper 'gimp_channel_get_tattoo))
(define gimp-vectors-set-linked (loudbus-helper 'gimp_vectors_set_linked))
(define gimp-vectors-parasite-list (loudbus-helper 'gimp_vectors_parasite_list))
(define gimp-layer-get-linked (loudbus-helper 'gimp_layer_get_linked))
(define gimp-undo-push-group-end (loudbus-helper 'gimp_undo_push_group_end))
(define gimp-drawable-set-tattoo (loudbus-helper 'gimp_drawable_set_tattoo))
(define gimp-image-raise-vectors (loudbus-helper 'gimp_image_raise_vectors))
(define gimp-brushes-set-brush (loudbus-helper 'gimp_brushes_set_brush))
(define gimp-channel-set-name (loudbus-helper 'gimp_channel_set_name))
(define gimp-palette-set-background (loudbus-helper 'gimp_palette_set_background))
(define gimp-gradients-get-gradient (loudbus-helper 'gimp_gradients_get_gradient))
(define gimp-bucket-fill (loudbus-helper 'gimp_bucket_fill))
(define gimp-image-get-layer-position (loudbus-helper 'gimp_image_get_layer_position))
(define gimp-layer-get-name (loudbus-helper 'gimp_layer_get_name))
(define gimp-drawable-is-layer-mask (loudbus-helper 'gimp_drawable_is_layer_mask))
(define gimp-parasite-detach (loudbus-helper 'gimp_parasite_detach))
(define gimp-drawable-get-linked (loudbus-helper 'gimp_drawable_get_linked))
(define gimp-image-parasite-attach (loudbus-helper 'gimp_image_parasite_attach))
(define gimp-image-lower-layer-to-bottom (loudbus-helper 'gimp_image_lower_layer_to_bottom))
(define gimp-palette-refresh (loudbus-helper 'gimp_palette_refresh))
(define gimp-drawable-is-channel (loudbus-helper 'gimp_drawable_is_channel))
(define gimp-vectors-set-name (loudbus-helper 'gimp_vectors_set_name))
(define gimp-vectors-parasite-detach (loudbus-helper 'gimp_vectors_parasite_detach))
(define gimp-parasite-find (loudbus-helper 'gimp_parasite_find))
(define gimp-patterns-list (loudbus-helper 'gimp_patterns_list))
(define gimp-drawable-delete (loudbus-helper 'gimp_drawable_delete))
(define gimp-image-set-cmap (loudbus-helper 'gimp_image_set_cmap))
(define gimp-color-picker (loudbus-helper 'gimp_color_picker))
(define gimp-drawable-bytes (loudbus-helper 'gimp_drawable_bytes))
(define gimp-image-parasite-detach (loudbus-helper 'gimp_image_parasite_detach))
(define gimp-vectors-get-name (loudbus-helper 'gimp_vectors_get_name))
(define gimp-brushes-get-paint-mode (loudbus-helper 'gimp_brushes_get_paint_mode))
(define gimp-layer-set-visible (loudbus-helper 'gimp_layer_set_visible))
