
%%% DRUM STYLE SETUP %%%

  % Define two sets of hand aliases: ll/rr and P/p
  drumPitchNames.rr = #'right 
  drumPitchNames.ll = #'left
  drumPitchNames.P  = #'right
  drumPitchNames.p  = #'left

  % Defines a style table that puts left under the line, and right above it
  #(define pipe-band-snare '((left default #f -1) (right default #f 1)))

%%%% Welcome to Pipe Band Drumming with Lilypond

% This file assumes that drum notes are being created with 'left and 'right
% types as per the drum style setup above.

%%% FIX THE LAYOUT %%%

  \layout {
    % No indent on the first line
    indent = #0
    \context {
      \Score
      
          $(add-grace-property 'Voice 'Stem 'font-size -4) 
    $(add-grace-property 'Voice 'NoteHead 'font-size -4) 

      % Fixes grace note spacing in drags and roughs
      \consists "Grace_spacing_engraver"
      \override GraceSpacing.shortest-duration-space = 0.1
      \override GraceSpacing.spacing-increment = 0.1
      % All stems down
      \override Stem.direction = #down
      % Flatten beams
      \override Beam #'positions = #'(-4 . -4)
      
      % 1 Line in the staff
      \override StaffSymbol.line-count =  #1      
      drumStyleTable = #(alist->hash-table pipe-band-snare)

      % Remove this line to enable bar numbers
      \omit BarNumber
      % Dont show a clef
      \omit Clef
      
  
      \dynamicUp    
    }
  }
  
  
%%% SUPPORT FUNCTIONS %%%

%% (if (equal? 'right (ly:music-property inHand 'drum-type))
  #(define (hands inHand)
  (if (equal? 'right (find-first-pitch inHand))
    (cons 'right 'left)
    (cons 'left 'right))
  )

  #(define (find-first-pitch music)
  (if (eq? #t (ly:music-property music 'drum-type #t))
    (ly:music-property (car (ly:music-property (ly:music-property music 'element) 'elements)) 'drum-type)
    (ly:music-property music 'drum-type)
  ))

%%% DRUM EMBELLISHMENTS %%%

  flam =
  #(define-music-function (parser location baseNote) (ly:music?)
  (let* ((h (hands baseNote))
        (offHand (cdr h))
        (gracey (make-music 'NoteEvent 'drum-type offHand 'duration (ly:make-duration 3)) )
        )

  #{
  \slashedGrace {
      \once \override Stem #'direction = #up
      $gracey
  }
  $baseNote
  #}
  ))

  drag =
  #(define-music-function (parser location baseNote) (ly:music?)
  (let* ((h (hands baseNote))
        (offHand (cdr h))
        (gracey (make-music 'NoteEvent 'drum-type offHand 'duration (ly:make-duration 4)) )
        )
  #{
  \slashedGrace {
      \once \override Stem #'direction = #up
      \once \override Beam #'positions = #'(2 . 2.25)
      $gracey $gracey
  }
  $baseNote
  #}
  ))
  
  ruff =
  #(define-music-function (parser location baseNote) (ly:music?)
  (let* ((h (hands baseNote))
        (offHand (cdr h))
        (onHand (car h))
        (gracey1 (make-music 'NoteEvent 'drum-type onHand 'duration (ly:make-duration 4)) )
        (gracey2 (make-music 'NoteEvent 'drum-type offHand 'duration (ly:make-duration 4)) )
        )
  #{
  \slashedGrace {
      \once \override Stem #'direction = #up
      \once \override Beam #'positions = #'(2 . 2.25)
      \once \override Beam #'positions = #'(2 . 3)

      $gracey1 $gracey2 $gracey2
  }
  $baseNote
  #}
  ))

  triplet =
  #(define-music-function
       (parser location padding)
       (ly:music?)
     #{
       \tuplet 3/2 #padding
     #})
