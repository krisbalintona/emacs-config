;;; krisb-oblique-strategies.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: text, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple implementation of Oblique Strategies prompts.  See
;; https://github.com/zzkt/oblique-strategies?tab=readme-ov-file and
;; https://github.com/zzkt/oblique-strategies?tab=readme-ov-file for more
;; information on its potential use and function.

;; The list of strategies was taken from https://oblique.ookb.co/list.html.

;;; Code:

(defvar krisb-oblique-strategies-prompts
  (list "(Organic) machinery."
        "A line has two sides."
        "A very small object - Its centre."
        "Abandon desire."
        "Abandon normal instructions."
        "Accept advice."
        "Accretion."
        "Adding on."
        "Allow an easement (an easement is the abandonment of a stricture)."
        "Always give yourself credit for having more than personality."
        "Always the first steps."
        "Animal noises."
        "Are there sections? Consider transitions."
        "Ask a computer program to repeat your last action."
        "Ask people to work against their better judgement."
        "Ask your body."
        "Assemble some of the elements in a group and treat the group."
        "Back up a few steps. What else could you have done?"
        "Balance the consistency principle with the inconsistency principle."
        "Be dirty."
        "Be extravagant."
        "Be less critical more often."
        "Breathe more deeply."
        "Build bridges."
        "Burn bridges."
        "Call your mother and ask her what to do."
        "Cascades."
        "Change ambiguities to specifics."
        "Change specifics to ambiguities."
        "Change instrument roles."
        "Change nothing and continue with immaculate consistency."
        "Children’s voices speaking."
        "Children’s voices singing."
        "Cluster analysis."
        "Consider different fading systems."
        "Consider transitions."
        "Consult other promising sources."
        "Consult other unpromising sources."
        "Convert a melodic element into a rhythmic element."
        "Courage!"
        "Cut a vital connection."
        "Cut a virtual connection."
        "Decorate, decorate."
        "Define an area as “safe” and use it as an anchor."
        "Describe the landscape in which this belongs."
        "Destroy nothing."
        "Destroy the most important thing."
        "Discard an axiom."
        "Disciplined self-indulgence."
        "Disconnect from desire."
        "Discover the recipes you are using and abandon them."
        "Discover your formulas and abandon them."
        "Display your talent."
        "Distorting time."
        "Do nothing for as long as possible."
        "Do something boring."
        "Do something sudden, destructive and unpredictable."
        "Do the last thing first."
        "Do the washing up."
        "Do the words need changing?"
        "Do we need holes?"
        "Don’t avoid what is easy."
        "Don’t be afraid of things because they’re easy to do."
        "Don’t be frightened of cliches."
        "Don’t be frightened to display your talents."
        "Don’t break the silence."
        "Don’t stress one thing more than another."
        "Emphasize differences."
        "Emphasize repetitions."
        "Emphasize the flaws."
        "Faced with a choice, do both!"
        "Feed the recording back out of the medium."
        "Feedback recordings into an acoustic situation."
        "Fill every beat with something."
        "First work alone, then work in unusual pairs."
        "From nothing to more than nothing."
        "Get your neck massaged."
        "Ghost echoes."
        "Give the game away."
        "Give way to your worst impulse."
        "Go outside. Shut the door."
        "Go slowly all the way round the outside."
        "Go to an extreme, move back to a more comfortable place."
        "How would someone else do it?"
        "How would you explain this to your parents?"
        "How would you have done it?"
        "Humanize something that is free of error."
        "Idiot glee."
        "Imagine the music as a moving chain or caterpillar."
        "Imagine the music as a series of disconnected events."
        "In total darkness."
        "In a very large room, very quietly."
        "Infinitesimal gradations."
        "Instead of changing the thing, change the world around it."
        "Credibility of intentions."
        "Nobility of intentions."
        "Humility of intentions."
        "Is it finished?"
        "Is something missing?"
        "Is the intonation correct?"
        "Is the style right?"
        "Is the tuning appropriate?"
        "It is quite possible (after all)."
        "It is simply a matter or work."
        "Just carry on."
        "Left channel, right channel, centre channel."
        "List the qualities it has. List those you’d like."
        "Listen in total darkness."
        "Listen in a very large room, very quietly."
        "Listen to the quiet voice"
        "Look at a very small object, look at its centre."
        "Look at the order in which you do things."
        "Look closely at the most embarrassing details and amplify."
        "Lost in useless territory."
        "Lowest common denominator check: single beat; single note; single riff."
        "Magnify the most difficult details."
        "Make a blank valuable by putting it in an excquisite frame."
        "Make a sudden, destructive unpredictable action. Incorporate."
        "Make an exhaustive list of everything you might do and do the last thing on the list."
        "Make it more sensual."
        "Make it more banal."
        "Make what’s perfect more human."
        "Mechanize something idiosyncratic."
        "Move towards the impossible."
        "Move towards the unimportant."
        "Mute and continue."
        "Not building a wall but making a brick."
        "Once the search is in progress, something will be found."
        "Only a part, not the whole."
        "Only one element of each kind."
        "Overtly resist change."
        "Pae White’s non-blank graphic metacard."
        "Pay attention to distractions."
        "Picture of a man spotlighted."
        "Put in earplugs."
        "Question the heroic approach."
        "Rearrange."
        "Remember those quiet evenings."
        "Remove a restriction."
        "Remove ambiguities and convert to specifics."
        "Remove specifics and convert to ambiguities."
        "Remove the middle, extend the edges."
        "Repetition is a form of change."
        "Retrace your steps."
        "Revaluation (a warm feeling)."
        "Reverse."
        "Short circuit (example; a man eating peas with the idea that they will improve his virility shovels straight into his lap)."
        "Shut the door and listen from outside."
        "Simple subtraction."
        "Simply a matter of work."
        "Slow preparation, fast execution."
        "Spectrum analysis."
        "State the problem in words as simply as possible."
        "Steal a solution."
        "Take a break."
        "Take away as much mystery as possible. What is left?"
        "Take away the elements in order of apparent non-importance."
        "Take away the important parts."
        "Tape your mouth."
        "The inconsistency principle."
        "The most important thing is the thing most easily forgotten."
        "The tape is now the music."
        "Think inside the work."
        "Think outside the work."
        "Think of the radio."
        "Tidy up."
        "Towards the insignificant."
        "Trust in the you of now."
        "Try faking it."
        "Turn it upside down."
        "Twist the spine."
        "Use “unqualified” people."
        "Use an old idea."
        "Use an unacceptable color."
        "Use cliches."
        "Use fewer notes."
        "Use filters."
        "Use something nearby as a model."
        "Use your own ideas."
        "Voice your suspicions."
        "Water."
        "Fire."
        "Earth."
        "Wind."
        "Heart."
        "What are the sections sections of? (Imagine a caterpillar moving)."
        "What context would look right?"
        "What do you do? Now, what do you do best?"
        "What else is this like?"
        "What is the reality of the situation?"
        "What is the simplest solution?"
        "What mistakes did you make last time?"
        "What most recently impressed you? How is it similar? What can you learn from it? What could you take from it?"
        "What to increase? What to reduce? What to maintain?"
        "What were the branch points in the evolution of this entity?"
        "What were you really thinking about just now? Incorporate."
        "What would make this really successful?"
        "What would your closest friend do?"
        "What wouldn’t you do? Do that."
        "When is it for?"
        "When is it for? Who is it for?"
        "Who is it for?"
        "Where is the edge?"
        "Which parts can be grouped?"
        "Who would make this really successful?"
        "Work at a different speed."
        "Would anyone want it?"
        "You are an engineer."
        "You can only make one dot at a time."
        "You don’t have to be ashamed of using your own ideas."
        "Your mistake was a hidden intention.")
  "List of Oblique Strategies prompts.")

(defun krisb-oblique-strategies--random (&optional n)
  "Return a random Oblique Strategies prompt.
If N is non-nil, return a list of that many prompts.  If N is nil,
default to 5."
  (let (prompts)
    (dotimes (i (or n 5))
      (push (seq-random-elt krisb-oblique-strategies-prompts) prompts))
    prompts))

;;;###autoload
(defun krisb-oblique-strategies-select (&optional n)
  "Select a random Oblique Strategies prompt.
The number of prompts available is N, which defaults to 5 if nil."
  (interactive "P")
  (let* ((n (or n 5))
         (prompts (krisb-oblique-strategies--random n)))
    (completing-read "Choose a prompt: " prompts nil t)))

;;;###autoload
(defun krisb-oblique-strategies-insert (&optional select n)
  "Insert into the current buffer a random Oblique Strategies prompt.
If SELECT is non-nil, use `krisb-oblique-strategies-select' to choose.
The number of prompts available is N, which defaults to 5 if nil."
  (interactive "P")
  (let ((prompt (if select
                    (krisb-oblique-strategies-select n)
                  (car (krisb-oblique-strategies--random 1)))))
    (insert prompt)))

;;; Provide
(provide 'krisb-oblique-strategies)
;;; krisb-oblique-strategies.el ends here
