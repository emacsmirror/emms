;;; emms-filters.el --- Filters for Emms          -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.
;;
;; Author:  Erica Lina Qi <EricaLinaQi@proton.me>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This code allows you to filter and search the metadata cache.
;; This manages the search and filter functionalities of emms-browser.

;; Usage
;; -------------------------------------------------------------------
;; Use filters as before with <> keys to cycle the filter ring in the browser buffer.
;; Search-by something to create a new cache, or emms-filters-push to get started
;; building a filter.
;;
;; Use 'emms-filters-status-print' to watch the stacks and filters in effect.

;; Use 'emms-filters-show-filter-menu' to see a list of all filters known
;; organized by factory.
;;
;; Apply a filter with the functions
;;    emms-filters-push, emms-filters-or, emms-filters-and, emms-filters-and-not
;;    emms-filters-smash and emms-filters-one-shot.
;;
;; manipulate the stack with the functions:
;; emms-filters-push, emms-filters-pop, emms-filters-clear and emms-filters-squash, swap and swap-pop.
;;
;; Interactively create and use new filters by choosing 'new filter'
;; in the filter selection lists.
;;
;; The function `emms-filters-current-meta-filter' gives the multi-filter data source
;; for the the current filter.
;;
;; A filter can be 'kept'. The function 'emms-filters-keep will create and register
;; a multi-filter of the current filter, adding it to the multi-filter menu.
;; This only lasts until the current Emacs session ends.
;; If emms-filters-multi-filter-save-file is set, a usable multi-filter definition will also be
;; appended to the file.
;;
;; Manage the search cache with emms-filters-hard-filter, emms-filters-one-shot, emms-filters-quick-one-shot,
;; emms-filters-search-by, emms-filters-pop-cache, emms-filters-squash-caches,
;; emms-filters-clear-caches, emms-filters-push-cache
;;
;; Caches can be stashed for the session and pushed back to the stack
;; at any time. The Emms-cache-DB is the default.
;;
;; Switch the active ring filter with <> which correspond to
;; emms-filters-next-ring-filter and emms-filters-previous-ring-filter.
;;
;; The filter stack can be cleared with emms-filters-clear, the caches
;; with emms-filters-clear-caches and the ring with emms-filters-clear-ring-filter.
;;
;; All stacks and filters can be cleared with 'emms-filters-clear-all


;; Some Definitions:
;; -------------------------------------------------------------------
;;    Filtering:  Displaying the narrowed results from looking for matches
;;                in a list of items.
;;    Search:
;;       The saving of the narrowed results created from filtering a list of items,
;;       such that future filtering and searching will have a smaller list of items.
;;
;;    Filter or filter cons,  a cons of the form (name . function)
;;         Registration takes care of this.
;;         Once a filter is properly constructed it will be a cons
;;         (name . filter-function) the functions are created with one of the
;;         filter factories.
;;
;;    Filter function - a function that takes a track as its argument
;;      and returns true or False.
;;
;;    Filter Factory: A function which creates a filter function given the
;;      the desired parameters.
;;
;;    Multi-filter:  A filter factory which is other filters combined
;;                   using Or, And as well as And-Not.
;;
;;    Meta-filter: A multi-filter data definition.
;;       The filter stack uses meta-filters in a cons
;;       like this; (name . meta-filter).
;;       Filter names for meta-filters can be easily constructed.
;;
;;       This meta-filter uses 4 filters by name:
;;
;;            '(("Vals" "Milonga")
;;              ("1900-1929" "1929-1937"))
;;
;;       This filter will
;;       Match on genre of vals OR
;;                         milonga AND
;;                             any year between
;;                         1900-1929 OR
;;                         1929-1937.
;;
;;       Making one or more multi-filter is easy.
;;       (emms-filters-make-filters
;;        '(("Multi-filter"
;;           "Vals | milonga - 1900-1937"
;;           (("Vals" "Milonga")
;;            ("1900-1929" "1929-1937")))));

;;    Meta-filter-stack:  An interactive stack of meta-filters which allow
;;      the creation, combination and use of all filters.
;;
;;    Filter-ring: A ring of filter names, which can be easily selected with
;;       next and previous controls. All filters created through
;;       'emms-browser-make-filter are added here by default.
;;
;;       The filter ring replaces the functionality of emms-browser-filters.
;;       The easiest way to make the filter ring is with a list of filters.
;;       (emms-filters-make-filter-ring '("Tango" "Vals" "Milonga"))


;; Backward compatibility:
;; -------------------------------------------------------------------
;; This code replaces both emms-browser filters and search-by.
;; emms-browser-make-filter and search-by use emms-filters for their
;; current functionality.
;;
;; Emms-browser-filter functions are specified to return an inverted value.
;; emms-browser-make-filter is a slightly different mechanism from emms-filters.el.
;; but has been modified to pass its filters to emms-filters.
;; Those filters will be properly inverted and added to emms-filters-filters and to the
;; emms-filters-filter-ring. This should provide a seamless experience for previous users
;; of emms-browser filtering. As the emms-filters-filter-ring is functionally equivalent.
;;
;; Search-by was just one filter factory, 'fields-search', and searches are
;; not inverted. The only real difference between a filter and a search was
;; that a filter was rendered and a search was saved for subsequent filtering.
;; The equivalent to the emms-browser search-by is just a one shot
;; interactive new fields-search factory filter that saves a cache.
;;
;; Filters are slightly different when coded for emms-filters.
;; 1. They should return true if they match the tracks
;; 2. The factory should wrap the lambda in a let with lexical-binding t.
;; 3. The factory and the filters must both be registered with emms-filters.
;;    This provides a higher level of interaction with the filters.
;; 4. There is no difference between a search function and a filter function.


;; The moving parts.
;; -------------------------------------------------------------------
;; Emms-filters consists of a few different mechanisms.
;; There are factories to make filters. There is the filter stack
;; to manage the creation and use of filters.
;;
;; There is the cache stack to handle the saving of a current filtered results
;; into a reduced database cache for subsequent searches.
;;
;; There is the filter ring for quickly switching between commonly used filters.
;;
;; - Filter Factories - To make filters, which are lambda functions.
;;                      Factories are frequently made from other factories.
;; - Filters - To be used by the meta-filter stack to create more filters.
;;             filters are represeed simply as data, and are very easy to define.
;; - Filter menu - A customizable ring of factories and their rings of filters.
;; - Multi-filter - A filter factory to create Meta-filters, filters made of filters.
;; - Meta-filter - A multi-filter data definition. Also data, and easy to define.
;; - The filter stack - A meta-filter manipulator and multi-filter creator.
;; - The cache stack - A stack of database caches.
;; - The filter ring. - A subset of convenient to use filters.
;;                      For backward compatibility and convenience.


;;; Filter factories
;; -------------------------------------------------------------------
;; Filter factories make filters which are simply test functions which
;; take a track and return true or false.
;;
;; Factories are registered with the Emms-filter system so that they
;; have names that can be referenced later. Additionally, registration
;; includes a prompt and parameter definition. This allows the emms-filters
;; prompting system to provide an interactive interface to any filter factory.
;;
;; The prompting system allows the creation of any filter interactively at
;; any time.
;;
;; Here is the Genre Factory which is actually made from the
;; field-compare factory. This is a common pattern to create
;; a simpler factory from a more complex one. It is simply
;; a partial that is registered with a different set of prompts.
;; In this case Genre: is the prompt and it is expected to be a string.
;;
;; (emms-filters-register-filter-factory
;;  "Genre"
;;  (apply-partially 'emms-filters-make-filter-field-compare
;;                   'string-equal-ignore-case 'info-genre)
;;  '(("Genre: " (:string . nil))));;
;;
;; The actual filter factory is the field comparison factory.
;; This single function can be a new factory for any data field
;; using any comparison function we would like.
;;
;; Filter factories depend upon lexical context of their parameters. In
;; order to have data values that stick after function creation there
;; is let using lexical binding to ensure the factory behaves as expected.
;; This transfers the values to local values and uses them as local
;; within the returned #'(lambda (track)...).
;;
;; (defun emms-filters-make-filter-field-compare (operator-func field compare-val)
;;   "Make a filter that compares FIELD to COMPARE-VALUE with OPERATOR-FUNC.
;; Works for number fields and string fields provided the appropriate
;; type match between values and the comparison function. Partials can
;; easily make more specific factory functions from this one."
;;   (let ((local-operator operator-func)
;;                 (local-field field)
;;                 (local-compare-val compare-val))
;;     #'(lambda (track)
;;         (let ((track-val (emms-track-get track local-field)))
;;           (and
;;            track-val
;;            (funcall local-operator local-compare-val track-val))))))

;; The registration for this factory is more complex because of the prompting
;; for all the parameters. By changing just the registration name and the
;; prompts we can create two factories, one for numbers and one for strings.
;; Note the use of the ` and , to force the select lists to resolve.
;;
;; (emms-filters-register-filter-factory "Number field compare"
;;                              'emms-filters-make-filter-field-compare
;;                              ;; prompts
;;                              `(("Compare Function: "
;;                                 (:function . ,emms-filters-number-compare-functions))
;;                                ("Field name: "
;;                                 (:symbol . ,emms-filters-number-field-names))
;;                                ("Compare to: "
;;                                 (:number . nil))))
;;
;;
;; Making a filter from a factory is easy.
;;
;; (emms-filters-make-filter "Genre" "My Genre filter" "Somevalue")
;;
;; Or make a lot of filters at once.
;;
;; (emms-filters-make-filters '(("Genre" "Waltz"      "waltz")
;;                     ("Genre" "Salsa"      "salsa")
;;                     ("Genre" "Blues"      "blues")
;;                     ("Genre" "Jazz"       "jazz")))
;;
;; Or just push a filter onto the stack with emms-filters-push,
;; select 'new filter' and follow the prompts.

;;; Factory Prompts.
;;; Interactive factory prompting for filter building.
;; -------------------------------------------------------------------
;; Registering a factory associates a name, a function and a list of
;; prompt definitions so that we may create filters interactively by name.
;;
;; The factory prompt data is used to interactively create new filters.
;; A prompt is  (prompt (type . select-list)) if there is no
;; select list we read a string and coerce the value to the correct
;; type as needed.  :number, :string, :list :symbol :function
;; are the coercion type choices.
;;
;; Here is a simple factory registration for the Genre filter factory function.
;; Which takes a single string parameter.
;;
;; (emms-filters-register-filter-factory "Genre"
;;                              emms-filters-make-filter-genre
;;                              '(("Genre: " (:string . nil))))
;;
;; Parameters are of the form: '((prompt (type . select-list)) ... )
;;
;; The following prompt will coerce the value it receives into a number.
;;
;; '(("Days: " (:number . nil)))
;;
;; The compare field factory takes a compare function,
;; an :info-field specifier and a string to compare.
;; Note the use of ` and , in order to resolve the selection lists here.
;; It uses the convenience variables which hold the compare functions and
;; string field names.
;;
;; `(("Compare Function:"
;;    (:function . ,emms-filters-string-compare-functions))
;;   ("Field name:"
;;    (:symbol . ,emms-filters-string-field-names))
;;   ("Compare to:"
;;    (:string . nil)));;
;;

;; The Filter stack
;; -------------------------------------------------------------------
;; The filter stack builds more complex filters as you push filters to it.
;; Adding to the filter or replacing it with another push continues to add filters
;; to the filter stack. To return to the previous filter simply pop the stack.
;;
;; To use a filter, emms-filters-push it to create a new current filter on the stack.
;; It will become a meta-filter on the filter stack
;; and the current active filter will be a multi-filter version of it.
;;
;; The filter ring works independently of the filter stack. Each re-filtering of
;; tracks uses the current ring filter and the current filter together.
;;
;; A filter can be 'kept'. The function 'emms-filters-keep will create and register
;; a multi-filter of the current filter, adding it to the multi-filter menu.
;; This only lasts until the current Emacs session ends.
;; If emms-filters-multi-filter-save-file is set, a usable filter definition will be
;; appended to the file.
;;
;; Other commands for manipulating the stack.
;;    Push, pop, squash, clear, swap, swap-pop, smash


;;; The Search Cache Stack
;; --------------------------------------------------
;; The cache stack is a simply a stack of emms-cache-db style hash tables.
;; Each entry is a subset of the master emms-cache-db created through filtering.
;; Their names are constructed from the filters which created them.
;;
;; Filtering and displaying of tracks is done against the top cache on the stack.
;;
;; The function; emms-filters-hard-filter creates a cache from the current filter
;; and cache, and pushes it to the stack.
;;
;; By using emms-filters-one-shot, emms-filters-quick-one-shot
;; also create caches on the stack. These functions allow effective
;; emulation of the previous EMMS-Browser search functionalities.
;;
;; The usual commands exist for manipulating the stack.
;;    Pop, squash, clear, swap, swap-pop, push-cache

;; Additionally there is a stash option. This pops and stashes the current
;; cache to be retrieved later. The stashed cache will become a selection
;; for the push-cache command.
;;
;; A one-shot filter combined with a factory name is 'emms-filters-quick-one-shot.
;;
;; This effectively emulates the former emms-browser search behavior of
;; quickly prompting, filtering and saving a cache by pushing a filter,
;; hard-filter then pop.


;; How it works.
;; -------------------------------------------------------------------
;; To begin simply do an emms-filters-push.  This will present the filter factory ring.
;; Choose a factory, an already existing filter or 'New' and follow the prompts.
;;
;; Filters which are created interactively can be kept for the session
;; with emms-filters-keep.  One shots, (searches), are automatically kept for the session.
;; Keep may also write them to a file for later use.
;;
;; Push a filter to the filter stack with emms-filters-push and then
;; add to it with the emms-filters-or, emms-filters-and, and emms-filters-and-not functions.
;; Each change results in new filter on the stack.
;;
;; Use emms-filters-or to add another filter and choose 'new filter' to
;; interactively create and add a filter to the current filter.
;;
;; Add in an extra layer of quick switch filtering with next and previous
;; filter-ring filters.  The filter ring filters can be accessed with
;; < and >.
;;
;; You may want to keep your results for a while, or you may
;; wish to start with a clear search for a name, either way,
;; a hard-filter will push a cache-db onto the cache stack.
;;
;; Subsequent filtering continues with this new DB cache. A cache can also
;; be pushed to the stack with a one-shot function. One shots
;; make, use, cache, and then pop a filter, leaving a new cache and the filter
;; stack as it was.
;;
;; Create a new factory function, register it in
;; emms-filters-factories along with its parameters and prompts.
;; From this point on filters can be created interactively by selecting
;; to push a new filter, and choosing the new factory.
;;
;; In code use emms-filters-make-filter or emms-filters-make-filters to use the factory by name.

;; Filter stack interaction
;; -------------------------------------------------------------------
;; To interactively create a filter, start with a push.
;; The filter stack itself is the interactive filter factory for multi-filters.
;;
;; Choose a factory, then an existing filter or 'new filter' and follow the prompts.
;;
;; Do an emms-filters-or to add another possible match or emms-filters-and or
;; emms-filters-and-not to add a restriction. Build the filter how you like.
;;
;; When a new filter is pushed, it turns into a meta filter
;; and is pushed on the filter stack. A filter function is made from
;; the entire stack's multi-filter and set to be the current filter,
;; and the browser is asked to re-render the results.
;;
;; Any change to the stack causes a re-render with the new current filter.
;;
;; Use emms-filters-status or the emms-filter-hydra to see the stacks and
;; current filters.

;; Making Filters from factories, in code.
;; -------------------------------------------------------------------
;; Filter factories include the following. Most common filters can be
;; easily constructed from these. The number of available filters is too
;; numerous to list. For instance, a filter already exists for every
;; track type and there many common genres and year range filters.
;;
;; Filter factories like artist, album artist, composer, Names, etc.
;; are all just specialized field compare or fields search factories.
;;
;; Factories
;; ----------
;; Album
;; Album-artist
;; All text fields
;; Artist
;; Artists
;; Artists and composer
;; Composer
;; Directory
;; Duration less
;; Duration more
;; Fields search
;; Genre
;; Greater than Year
;; Less than Year
;; Multi-filter
;; Names
;; Names and titles
;; Not played since
;; Notes
;; Number field compare
;; Orchestra
;; Performer
;; Played since
;; String field compare
;; Title
;; Titles
;; Track type
;; Year range

;; Filters also have names, and are added
;; to their respective factory's filter selection menu.
;; here are some example filter definitions.
;;
;; ;; Filters are easily described as data.
;; ;;        factory      Name        arguments
;;
;; (setq tango-filters
;;       '(("Year range" "1900-1929" 1900 1929)
;;         ("Year range" "1929-1937" 1929 1937)
;;         ("Directory" "tangotunes" "tangotunesflac")
;;
;;         ("Genre" "Vals"    "vals")
;;         ("Genre" "Tango"   "tango")
;;         ("Genre" "Milonga" "milonga")
;;
;;         ("Multi-filter"
;;          "1900-1937"
;;          (("1900-1929" "1929-1937")))
;;
;;         ("Multi-filter"
;;          "Vals | milonga"
;;          (("Vals" "Milonga")))
;;
;;         ("Multi-filter"
;;          "Vals 1900-1929"
;;          (("Vals") ("1900-1929")))
;;
;;         ("Multi-filter"
;;          "Not vals"
;;          ((:not "Vals")))
;;
;;         ("Multi-filter"
;;          "Vals or milonga 1900-1937"
;;          (("Vals" "Milonga")
;;           ("1900-1929" "1929-1937")))
;;         ))
;;
;; (emms-filters-make-filters tango-filters)
;;
;; ;; Add my own filter selection menu with tango filters in it.
;; (emms-filters-add-filter-menu-from-filter-list "Tango" tango-filters)
;;
;; The easiest way to make a filter ring.
;; (emms-filters-make-filter-ring '("Tango" "Vals" "Milonga"))

;;; Code:

(require 'emms-cache)
(require 'ring)
(require 'cl-lib)

(defvar  emms-filters-stack nil
  "A history of multi-filters. Our working stack.")

(defvar emms-filters-search-caches '()
  "The stack of search result caches.")

(defvar  emms-filters-filter-ring nil
  "A ring of filter names for quick access with next and previous.")

(defconst emms-filters-no-filter nil ;; '("no filter" . nil)
  "A filter that turns filtering off, a better initial value than nil.")

(defvar emms-filters-current-ring-filter  emms-filters-no-filter
  "The current ring filter, a filter cons, (name . func).")

(defvar emms-filters-filter-factories '()
  "An alist of filter factory functions and their argument lists.")

(defvar emms-filters-filters '(("no filter" . nil))
  "A list of available filters.")

(defvar emms-filters-automatic-filter-names t
  "Automatically generate filter names when creating filters interactively.")

(defvar emms-filters-current-filter emms-filters-no-filter
  "The current filter function.")

(defvar emms-filters-current-filter-name "no filter"
  "A name string of the filter for everyone to use.")

(defvar emms-filters-filter-menu '("no filter" "new filter")
  "A list of available filters grouped by factory.")

(defgroup emms-filters nil
  "*The Emacs Multimedia System filter system"
  :prefix "emms-filters-"
  :group 'multimedia
  :group 'applications)

;; For backwards compatibility with emms-browser
;; This is really just a mirror of the browser's hook.
(defcustom emms-filters-filter-changed-hook nil
  "Hook to run after the filter has changed."
  :type 'hook)

;; Emms-filters is agnostic about the renderer.
;;
;; These are to be set by the rendererer so that emms-filters
;; can ask for a new render of the results when
;; new a new filter has been created.

(defvar emms-filters-make-and-render-hash-hook nil
  "This function applies the filters, creates a hash,
and then populates and renders a tree of data,
For the Emms-browser this should be emms-browse-by.")

;; emms-filters-expand-render-hook
(defvar emms-filters-expand-render-hook nil
  "To be set by the renderer so that the results tree
can be expanded when a filter or search exists,
For the Emms-Browser this is the emms-browser-expand-all function.")

(defvar emms-filters-multi-filter-save-file nil
  "A file name to write the kept meta-filters from the session to.")

(defvar emms-filters-cache-stash '(("Emms DB" . emms-cache-db))
  "A list of cons (name . cache).")

(defun emms-filters-browser-filter-hook-function (track)
  "A hook function for the browser. Freewill here for TRACK filtering.
First we test the track against the current ring filter if we have one,
then we combine with the result of the emms-filters-current-filter."
  (and (if (cdr emms-filters-current-ring-filter)
           (funcall (cdr emms-filters-current-ring-filter) track)
         t)
       (if (cdr emms-filters-current-filter)
           (funcall (cdr emms-filters-current-filter) track)
         t)))

(defun emms-filters-register-filter (filter-name filter)
  "Put our new FILTER function named FILTER-NAME in our filter list."
  (push (cons filter-name filter) emms-filters-filters))

(defun emms-filters-register-if-missing (filter)
  "Register a cons FILTER if it isn't already in the emms-filters-filters list."
  (when (not (assoc (car filter) emms-filters-filters))
    (push filter emms-filters-filters )))

;; (defun emms-filters-add-filter-menu-item (folder-name name-list)
;;   "Add a list of NAME-LIST, a list of strings,
;; as another FOLDER-NAME in the filter selection menu."
;;   (setq emms-filters-filter-menu
;;         (cons (list folder-name name-list)
;;               emms-filters-filter-menu)))

(defun emms-filters-add-to-filter-menu-from-filter-list (folder filters)
  "Add a FOLDER and FILTERS to the filter select list menu."
  (emms-filters-add-to-filter-menu folder (mapcar 'cadr filters)))

(defun emms-filters-add-to-filter-menu (folder-name filter-or-list)
  "Add to a FOLDER-NAME in the filter select menu creating it as needed.
Adds filter name(s) given in FILTER-OR-LIST to the FOLDER-NAME
of the filter select menu tree."
  (if (listp filter-or-list)
      (mapcar (lambda (filter)
                (emms-filters-add-name-to-filter-menu folder-name filter))
              filter-or-list)
    (emms-filters-add-name-to-filter-menu folder-name filter-or-list)))

(defun emms-filters-add-name-to-filter-menu (folder-name filter-name)
  "Add FILTER-NAME to menu tree of FOLDER-NAME."
  (if (assoc folder-name emms-filters-filter-menu)
      (push filter-name (cadr (assoc folder-name emms-filters-filter-menu)))
    (setq emms-filters-filter-menu
          (cons (list folder-name (list filter-name))
                emms-filters-filter-menu))))

(defun emms-filters-show-filter-menu ()
  "Show the menu tree of filters."
  (interactive)
  (message "%s"
           (mapconcat
            (lambda (menu)
              (if (consp menu)
                  (format "%s : \n%s\n"
                          (car menu)
                          (mapconcat 'identity
                                     (cadr menu) ", "))
                menu))
            emms-filters-filter-menu "\n")))

(defun emms-filters-make-filter-ring (list-of-filter-names)
  "Make a ring of filter names from the LIST-OF-FILTER-NAMES.
Appends the `no filter' filter."
  (setq emms-filters-filter-ring
        (make-ring
         (+ 1 (length list-of-filter-names))))
  (mapcar (lambda (filter-name)
            (ring-insert emms-filters-filter-ring filter-name))
          (cons "no filter" list-of-filter-names)))

(defun emms-filters-append-to-filter-ring (filter-name)
  "Append a single FILTER-NAME to the filter-ring.
This creates the filter ring as needed."
  (if emms-filters-filter-ring
      (ring-insert+extend emms-filters-filter-ring
                          filter-name t)
    (emms-filters-make-filter-ring (list filter-name))))

;; This should allow people to continue using the emms-browser
;; filtering as they always have, reusing the filters they've already made.
(defun emms-filters-register-filter-into-ring (filter)
  "Integrate Emms browser filters into emms-filters-filters.
Register a FILTER to emms-filters-filters if it's name is missing.
Add its name to the filter ring and filter menu in
the `browser-filters' selection menu."
  (emms-filters-register-if-missing filter)
  (let ((name (car filter)))
    (emms-filters-append-to-filter-ring name)
    (emms-filters-add-to-filter-menu "browser-filters" name)))

(defun emms-filters-list-filters ()
  "List the filters in our filter list."
  (mapcar 'car emms-filters-filters))

(defun emms-filters-show-filters ()
  "Show the filters we have."
  (interactive)
  (when emms-filters-filters
    (message "Emf Filters:\n%s"
             (mapconcat 'identity (emms-filters-list-filters) "\n"))))

(defun emms-filters-show-filter-ring ()
  "Show the filters in the filter ring."
  (interactive)
  (message "Ring filters: %s" (ring-elements emms-filters-filter-ring)))

(defun emms-filters-find-filter (name)
  "A nicer way to find NAME in our list of filters."
  (assoc name emms-filters-filters))

(defun emms-filters-find-filter-function (filter-name)
  "Find the Function for FILTER-NAME in emms-filters-filters.
Pass functions through untouched."
  (if (eq filter-name :not)
      :not
    (cdr (assoc filter-name emms-filters-filters))))

(defun emms-filters-format-search (fields value)
  "Create a string format from a list of FIELDS and a compare VALUE."
  (format "%s : %s"
          (mapconcat
           #'(lambda (info)
               (if (symbolp info)
                   (substring (symbol-name info)  5)
                 info))
           fields " | ")
          value))

;; The Filter Factory of factories.
;; making them, using them, keeping them organized.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emms-filters-make--filter (factory factory-args)
  "Make a filter using the FACTORY and FACTORY-ARGS.
If factory is a function it is used directly. Otherwise, it will
look for the function in emms-filters-filter-factories."
  (let ((factory-func (if (functionp factory)
                          factory
                        (cadr (assoc factory emms-filters-filter-factories)))))
    (apply factory-func factory-args)))

(defun emms-filters-make-filter (factory filter-name factory-args)
  "Make a filter named FILTER-NAME using the FACTORY and FACTORY-ARGS.
If factory is a function it is used directly. Otherwise, it will
look for the function in emms-filters-filter-factories."
  (emms-filters-add-to-filter-menu factory filter-name)
  (emms-filters-register-filter
   filter-name
   (emms-filters-make--filter factory factory-args)))

(defun emms-filters-make-filters (filter-list)
  "Make filters in FILTER-LIST into filter functions.
The filter list holds entries specified as
  (factory-name filter-name factory-arguments)."
  (mapcar (lambda (filter)
            (emms-filters-make-filter
             (car filter)
             (cadr filter) (cddr filter)))
          filter-list))

(defun emms-filters-new-filter (&optional factory-name make-filter-name)
  "Build a new filter from a filter factory interactively.
Use FACTORY-NAME instead of prompting if given.
If MAKE-FILTER-NAME or EMMS-FILTERS-AUTOMATIC-FILTER-NAMES is true the name will
be constructed instead of prompted.

Normally prompts for a filter factory and its parameters, prompts for a
filter name and then creates and registers a new filter,then returns its name."
  (interactive)
  (let* ((factory-name (if factory-name factory-name
                         (emms-filters-choose-factory)))
         (make-name (or make-filter-name emms-filters-automatic-filter-names))
         (parameters (emms-filters-get-factory-parameters factory-name))
         (filter-name (if make-name
                          (format "%s : %s" factory-name parameters)
                        (read-string "filter name:"))))

    (message "%s | %s parms %s" factory-name filter-name parameters)

    (emms-filters-make-filter factory-name filter-name parameters)
    filter-name))

(defun emms-filters-register-filter-factory (name func prompt-list)
  "Register FUNC as NAME with PROMPT-LIST into a filter choice.
Give it the shape: (name . (func . prompt-list))."
  (push
   (cons name (cons func prompt-list))
   emms-filters-filter-factories))

(defun emms-filters-list-filter-factories ()
  "List the filters in our factories list."
  (mapcar 'car emms-filters-filter-factories))

(defun emms-filters-show-filter-factories ()
  "Show the filter factories we have."
  (interactive)
  (when emms-filters-filter-factories
    (message "Filter Factories:\n%s"
             (mapconcat #'identity (emms-filters-list-filter-factories) "\n  "))))

;; (message "Emms Cache stack:\n  %s\n"
;;          (mapconcat #'identity (emms-filters-get-search-keys) "\n  "))

(defun emms-filters-clear-filter-factories ()
  "Reset the filter factory list."
  (setq emms-filters-filter-factories nil))


;;; Factory Prompting.
;;
;; This function is a bit brittle for my taste.
;; It needs more use cases.
;; It might be the only one, or we only ever have lists of symbols...
;; This is used for the fields-search factory.
(defun emms-filters-string-field-list-prompt (prompt)
  "Recursively PROMPT for elements of a list.
Prompt must define a select list. The only usage example so
far is the field-search list which is all symbols.
 info-artist, info-genre, `intern-soft' works for those."
  (let* ((prompt-string (car prompt))
         (selections (cdar (cdr prompt)))
         (value
          (completing-read prompt-string
                           (cons "quit" selections) nil t)))
    (if (string= value "quit")
        nil
      (cons (intern-soft value)
            (emms-filters-string-field-list-prompt
             (cons (concat (car prompt) " " value)
                   (cdr prompt)))))))

(defun emms-filters-coerce-prompt-value (prompt value)
  "Coerce VALUE, a string, according to the prompt type inside PROMPT.
PROMPT should be in the form (prompt (type . <select-list>)).
Types are :number, :symbol, :string and :function.
Strings pass through."
  (let ((type (car (cadr prompt))))
    (cond
     ((string= type :number) (string-to-number value))
     ((string= type :symbol) (intern-soft value))
     ((string= type :function) (intern-soft value))
     (t value))))

(defun emms-filters-read-string-or-choose (prompt)
  "Choose the method input using PROMPT.
Do a string read or completing read if PROMPT has a select-list.
Do a recursive completing read with selection-list if a :list type.
A prompt should look like this; (prompt (type . <select-list>))."
  (let* ((prompt-string (car prompt))
         (selections (cdr (cadr prompt)))
         (_ (message "Selections %s" selections))
         (type (car (cadr prompt)))
         (value (cond ((string= type :list) (emms-filters-string-field-list-prompt prompt))
                      (selections
                       (completing-read prompt-string selections nil t))
                      (t (read-string prompt-string)))))
    (emms-filters-coerce-prompt-value prompt value)))

(defun emms-filters-get-factory-parameters (factory-name)
  "Prompt for the parameters needed by a factory identified by FACTORY-NAME.
Coerce their types as indicated and return the list of parameters.

A prompt should be of the form (prompt (type . <list>)) where prompt is a string
and type is :number :function :symbol or :string"
  (interactive)
  (let ((prompts (cddr (assoc factory-name emms-filters-filter-factories))))
    (mapcar (lambda (prompt)
              (emms-filters-read-string-or-choose prompt))
            prompts)))


;;; Factory Functions to make filter functions with.
;; A filter factory is a function that returns a function which
;; returns true if it likes the values from the track it was given.
;;
;; Registering them makes them interactive and invokable
;; by name.

(defun emms-filters-make-filter-directory (dirname)
  "Generate a function to check if a track is in DIRNAME.
If the track is not in DIRNAME, return t.
Uses a regex anchoring dirname to the beginning of the expanded path."
  (let ((re (concat "^" (expand-file-name dirname))))
    #'(lambda (track)
        (string-match re (emms-track-get track 'name)))))

(emms-filters-register-filter-factory "Directory"
                             'emms-filters-make-filter-directory
                             '(("Directory: " (:string . nil))))

;; seconds in a day (* 60 60 24) = 86400
(defun emms-filters-make-filter-played-within (days)
  "Show only tracks played within the last number of DAYS."
  (let ((seconds-to-time (seconds-to-time (* days 86400))))
    #'(lambda (track)
        (let ((min-date (time-subtract
                         (current-time)
                         seconds-to-time))
              last-played)
          (and (setq last-played
                     (emms-track-get track 'last-played nil))
               (time-less-p min-date last-played))))))

(emms-filters-register-filter-factory "Played since"
                             'emms-filters-make-filter-played-within
                             '(("Days: " (:number . nil))))

(defun emms-filters-make-filter-not-played-within (days)
  "Make a not played since DAYS filter."
  (lambda (track)
    (funcall (emms-filters-make-filter-played-within days) track)))

(emms-filters-register-filter-factory "Not played since"
                             'emms-filters-make-filter-not-played-within
                             '(("Days: " (:number . nil))))

;; Getting the year is special. It might be in year or date.
(defun emms-filters-get-year (track)
  "Get the year from a TRACK. Check year and date fields.
Returns a number"
  (let* ((year (emms-track-get track 'info-year))
         (date (emms-track-get track 'info-date))
         (year (or year (emms-format-date-to-year date)))
         (year (and year (string-to-number year))))
    year))

(defun emms-filters-make-filter-year-range (y1 y2)
  "Make a date range filter from Y1 and Y2."
  (let ((local-y1 y1)
        (local-y2 y2))
    #'(lambda (track)
        (let ((year (emms-filters-get-year track)))
          (and
           year
           (<= local-y1 year)
           (>= local-y2 year))))))

(emms-filters-register-filter-factory "Year range"
                             'emms-filters-make-filter-year-range
                             '(("Start year:" (:number . nil))
                               ("End year:" (:number . nil))))

(defun emms-filters-make-filter-year-greater (year)
  "Make a Greater than year filter from YEAR."
  (let ((local-year year))
    #'(lambda (track)
        (let ((year (emms-filters-get-year track)))
          (and
           year
           (<= local-year year))))))

(emms-filters-register-filter-factory "Greater than Year"
                             'emms-filters-make-filter-year-greater
                             '(("Greater than year: " (:number . nil))))

(defun emms-filters-make-filter-year-less (year)
  "Make a Less than year filter from YEAR."
  (let ((local-year year))
    #'(lambda (track)
        (let ((year (emms-filters-get-year track)))
          (and
           year
           (>= local-year year))))))

(emms-filters-register-filter-factory "Less than Year"
                          'emms-filters-make-filter-year-less
                          '(("Less than year: " (:number . nil))))

;; fields-search
;; -------------
;; A replacement filter factory for the emms-browser-fields-search filter.
(defun emms-filters-make-filter-fields-search (fields compare-value)
  "Make a filter to search in a list of track FIELDS for COMPARE-VALUE.
This replaces the original emms-browser search match-p functionality."
  (let ((local-fields fields)
        (local-compare-value compare-value))
    #'(lambda (track)
        (cl-reduce
         (lambda (result field)
           (let ((track-value (emms-track-get track field "")))
             (or result
                 (and track-value
                      (string-match local-compare-value track-value)))))
         local-fields
         :initial-value nil))))

(defvar emms-filters-string-field-names
  '(info-albumartist
    info-artist
    info-composer
    info-performer
    info-title
    info-album
    info-date
    info-originaldate
    info-note
    info-genre)
  "The list of track field names that are strings.")

(emms-filters-register-filter-factory
 "Fields search"
 'emms-filters-make-filter-fields-search
 `(("Choose fields to search : "
    (:list . ,emms-filters-string-field-names))
   ("Search: " (:string . nil))))

;; field-compare
;; -------------
(defvar emms-filters-number-field-names
  '(info-tracknumber
    info-discnumber
    info-year
    info-originalyear
    info-originaldate
    info-playing-time)
  "The list of track field names that are numbers.")

(defvar emms-filters-string-compare-functions
  '(emms-filters-match-string
    string-equal-ignore-case
    string=
    string<
    string>
    string-match)
  "Compare functions for filter creation.")

(defvar emms-filters-number-compare-functions
  '(> >= = <= <)
  "Compare functions for filter creation.")

(defvar emms-filters-track-types
  '(file url stream streamlist playlist)
  "Types of tracks we can have.")

(defun emms-filters-match-string (string1 string2)
  "Check to see if STRING2 is in STRING1.

This is the inverse parameter list of `string-match'.
So we can continue with the language of
`filter track where field contains string'
`filter track where field > value'."
  (string-match string2 string1))

(defun emms-filters-make-filter-field-compare (operator-func field compare-val)
  "Make a filter that compares FIELD to COMPARE-VALUE with OPERATOR-FUNC.
Works for number fields and string fields provided the appropriate
type match between values and the comparison function. Partials can
easily make more specific factory functions from this one."
  (let ((local-operator operator-func)
        (local-field field)
        (local-compare-val compare-val))
    #'(lambda (track)
        (let ((track-val (emms-track-get track local-field)))
          (and
           track-val
           (funcall local-operator local-compare-val track-val))))))

;; not sure anyone will use these directly but you never know.
;; Its a good test for the prompting system.
;; Note the use of ` and , to resolve the selection lists here.
(emms-filters-register-filter-factory "Number field compare"
                             'emms-filters-make-filter-field-compare
                             ;; prompts
                             `(("Compare Function: "
                                (:function . ,emms-filters-number-compare-functions))
                               ("Field name: "
                                (:symbol . ,emms-filters-number-field-names))
                               ("Compare to: "
                                (:number . nil))))

(emms-filters-register-filter-factory "String field compare"
                             'emms-filters-make-filter-field-compare
                             ;; prompts
                             `(("Compare Function: "
                                (:function . ,emms-filters-string-compare-functions ))
                               ("Field name: "
                                (:symbol . ,emms-filters-string-field-names))
                               ("Compare to: "
                                (:string . nil))))

;; Generic field comparison factories.
;; parameter order is good for making partials.
(emms-filters-register-filter-factory
 "Duration less"
 (apply-partially 'emms-filters-make-filter-field-compare
                  '<= 'info-playing-time)
 '(("Duration: " (:number . nil))))

(emms-filters-register-filter-factory
 "Duration more"
 (apply-partially 'emms-filters-make-filter-field-compare
                  '>= 'info-playing-time)
 '(("Duration: " (:number . nil))))

(emms-filters-register-filter-factory
 "Genre"
 (apply-partially 'emms-filters-make-filter-field-compare
                  'string-equal-ignore-case 'info-genre)
 '(("Genre: " (:string . nil))))

(emms-filters-register-filter-factory
 "Track type"
 (apply-partially 'emms-filters-make-filter-field-compare
                  'eq 'type)
 '(("Track type: "
    (:string . '(file url stream streamlist playlist)))))

;; Search fields for text.  Same behavior as emms-browser-search.
;; Replace the emms browser searches with these filter factories.

(emms-filters-register-filter-factory
 "Album-artist"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-albumartist))
 '(("Search album artist: " (:string . nil))))

(emms-filters-register-filter-factory
 "Artist"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-artist))
 '(("Search artist: " (:string . nil))))

(emms-filters-register-filter-factory
 "Artists"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-artist info-albumartist))
 '(("Search artists: " (:string . nil))))

(emms-filters-register-filter-factory
 "Artists and composer"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-artist info-albumartist info-composer))
 '(("Search artists and composer: " (:string . nil))))

(emms-filters-register-filter-factory
 "Album"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-album))
 '(("Search album: " (:string . nil))))

(emms-filters-register-filter-factory
 "Title"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-title))
 '(("Search title: " (:string . nil))))

(emms-filters-register-filter-factory
 "Performer"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-performer))
 '(("Search performer: " (:string . nil))))

(emms-filters-register-filter-factory
 "Orchestra"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-orchestra))
 '(("Search orchestra: " (:string . nil))))

(emms-filters-register-filter-factory
 "Composer"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-composer))
 '(("Search composer: " (:string . nil))))

(emms-filters-register-filter-factory
 "Notes"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-note))
 '(("Search notes: " (:string . nil))))

(emms-filters-register-filter-factory
 "Titles"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-title
                    info-album))
 '(("Search titles: " (:string . nil))))

(emms-filters-register-filter-factory
 "Names"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-albumartist
                    info-name
                    info-artist
                    info-composer
                    info-performer))
 '(("Search names: " (:string . nil))))

(emms-filters-register-filter-factory
 "Names and titles"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-albumartist
                    info-artist
                    info-composer
                    info-performer
                    info-name
                    info-title
                    info-album))
 '(("Search names and titles: " (:string . nil))))

(emms-filters-register-filter-factory
 "All text"
 (apply-partially 'emms-filters-make-filter-fields-search
                  '(info-albumartist
                    info-artist
                    info-composer
                    info-performer
                    info-title
                    info-album
                    info-name
                    info-date
                    info-originaldate
                    info-note
                    info-genre))
 '(("Search all text fields: " (:string . nil))))

;; Multi-filter  - Just another factory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A filter of filters. A list of lists of filter Names and maybe a :not.
;; Each list is Reduced with Or then reduced together with And and Not.
(defun emms-filters-or-group->multi-funcs (filter-name-list)
  "Return a list of functions from emms-filters-filters for a FILTER-NAME-LIST.
Functions already in the list will be passed through."
  (mapcar (lambda (filter-name)
            (emms-filters-find-filter-function filter-name))
          filter-name-list))

(defun emms-filters-meta-filter->multi-funcs (meta-filter)
  "Return a list of functions from emms-filters-filters for a META-FILTER."
  (mapcar (lambda (or-group)
            (emms-filters-or-group->multi-funcs or-group))
          meta-filter))

(defun emms-filters-reduce-or-group (or-group track)
  "Reduce OR-GROUP for TRACK."
  (cl-reduce
   (lambda (result filter-func)
     (or result
         (funcall filter-func track)))
   or-group
   :initial-value nil))

(defun emms-filters-reduce-invert-or-group (or-group track)
  "Call an OR-GROUP list of filters with TRACK and reduce result with OR.
If the first item is :not then invert the result from the reduction."
  (let* ((invert (eq (car or-group) :not))
         (group (if invert
                    (cdr or-group)
                  or-group))
         (result (emms-filters-reduce-or-group group track)))
    (if invert (not result) result)))

(defun emms-filters-make-multi-filter (meta-filter)
  "Make a track filter function from META-FILTER.
The function will take a track as a parameter and return t if the track
does not match the filters.
A multi-filter is a list of lists of filter names.
The track is checked against each filter, each list of filters is
reduced with or. The lists are reduced with and.
Returns True if the track should be filtered out."
  (let ((local-multi-funcs
         (emms-filters-meta-filter->multi-funcs meta-filter)))
    #'(lambda (track)
        (cl-reduce
         (lambda (result funclist)
           (and result
                (emms-filters-reduce-invert-or-group funclist track)))
         local-multi-funcs
         :initial-value t))))

(emms-filters-register-filter-factory "Multi-filter"
                             'emms-filters-make-multi-filter
                             '(nil))

;;; Some filters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A simple not a filter, So we have a default of no filters to choose/return to.
(emms-filters-register-filter "No filter" nil)

;; The variables are simply organizational so they can
;; be created and added to the filter ring.

;;             factory      name        factory arg
(defvar emms-filters-decade-filters
  '(("Year range" "1900s"     1900 1909)
    ("Year range" "1910s"     1910 1919)
    ("Year range" "1920s"     1920 1929)
    ("Year range" "1930s"     1930 1939)
    ("Year range" "1940s"     1940 1949)
    ("Year range" "1950s"     1950 1959)
    ("Year range" "1960s"     1960 1969)
    ("Year range" "1970s"     1970 1979)
    ("Year range" "1980s"     1980 1989)
    ("Year range" "1990s"     1990 1999)
    ("Year range" "2000s"     2000 2009)
    ("Year range" "2010s"     2010 2019)
    ("Year range" "2020s"     2020 2029))
  "filter tracks by decade")

(defvar emms-filters-genre-filters
  '(("Genre" "Waltz"      "waltz")
    ("Genre" "Vals"       "vals")
    ("Genre" "Tango"      "tango")
    ("Genre" "Milonga"    "milonga")
    ("Genre" "Condombe"   "condombe")
    ("Genre" "Salsa"      "salsa")
    ("Genre" "Blues"      "blues")
    ("Genre" "Rock"       "rock")
    ("Genre" "Swing"      "swing")
    ("Genre" "Pop"        "pop")
    ("Genre" "Rap"        "rap")
    ("Genre" "Hip hop"    "hip hop")
    ("Genre" "Classical"  "classical")
    ("Genre" "Baroque"    "baroque")
    ("Genre" "Chamber"    "chamber")
    ("Genre" "Reggae"     "reggae")
    ("Genre" "Folk"       "folk")
    ("Genre" "World"      "world")
    ("Genre" "Metal"      "metal")
    ("Genre" "Fusion"     "fusion")
    ("Genre" "Jazz"       "jazz"))
  "Some filters for a the track genre")

(defvar emms-filters-last-played-filters
  '(("Played since" "Played in the last month" 30)
    ("Not played since" "Not played since a year" 365))
  "filters for the last time a track was played")

(defvar emms-filters-track-type-filters
  '(("Track type" "File" file)
    ("Track type" "Url" url)
    ("Track type" "Stream" stream)
    ("Track type" "Stream list" streamlist)
    ("Track type" "Play list" playlist))
  "filters for track types")

(defvar emms-filters-duration-filters
  '(("Duration less" "Duration <1 min"  60)
    ("Duration less" "Duration <5 min"  300)
    ("Duration more" "Duration >5 min"  300)
    ("Duration more" "Duration >10 min" 600))
  "filters for the duration of a track.")

(defun emms-filters-make-default-filters()
  "Make some default filters anyone would not mind having."
  (emms-filters-make-filters emms-filters-decade-filters)
  (emms-filters-make-filters emms-filters-genre-filters)
  (emms-filters-make-filters emms-filters-track-type-filters)
  (emms-filters-make-filters emms-filters-last-played-filters)
  (emms-filters-make-filters emms-filters-duration-filters))

;; Install some default filters.
(emms-filters-make-default-filters)

;; The Meta-Filter stack
;; An interactive multi-filter stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current filter is the multi-filter version of the meta-filter
;; at the top of the filter stack.
;;
;; Adding more filters to the current filter pushes a new filter to the stack.
;; emms-filters-pop pops the stack, returning to the last filter.
;;
;; Other filters can be added to the current filter
;; with 'and', 'or' as well as 'and-not' and 'smash' filter selections.

(defun emms-filters-current-meta-filter ()
  "Return the current meta-filter from the top of the stack."
  (format "%S" (car emms-filters-stack)))

(defun emms-filters-copy-meta-filter (filter)
  "Copy the meta-filter given by FILTER."
  (mapcar 'copy-sequence filter))

(defun emms-filters-filter-name->meta-filter (filter-name)
  "Make a meta filter cons from a FILTER-NAME."
  (cons filter-name
        (list (list filter-name))))

(defun emms-filters-format-meta-filter-groups (filter-list)
  "Format the FILTER-LIST contents to a list of strings."
  (mapconcat (lambda (fname) (format "%s " fname)) filter-list " | "))

(defun emms-filters-make-name (meta-filter)
  "Construct a name from the META-FILTER contents."
  (mapconcat 'identity
             (mapcar 'emms-filters-format-meta-filter-groups meta-filter) " && "))

(defun emms-filters-make-filter-cons-from-meta-filter (filter)
  "Make a filter cons from meta-filter FILTER."
  (cons (emms-filters-make-name filter) filter))

(defun emms-filters-set-filter (filter)
  "Set the current filter to FILTER.
Filter should be a filter cons in the form of `(name . function)."
  (setq emms-filters-current-filter-name (car filter))
  (setq emms-filters-current-filter filter))

;; (defun emms-filters-browse-by ()
;;   "The single interface to emms-browser. Re-render please.
;; Uses the top level type, or the default browse type."
;;   (emms-browse-by (or emms-browser-top-level-type
;;                       emms-browser-default-browse-type)))

(defun  emms-filters-refilter ()
  "Make a multi-filter function from the current meta-filter and set it.
Run the filter changed hooks. Ask the Browser/renderer to re-render with
the render and expand hooks."
  (emms-filters-set-filter (cons (caar emms-filters-stack)
                     (emms-filters-make-multi-filter (cdar emms-filters-stack))))

  ;; filter-changed-hook is a defcustom for users.
  (run-hooks 'emms-filters-filter-changed-hook)
  ;; this hook is for renderers.
  (run-hooks 'emms-filters-make-and-render-hash-hook)
  ;; If it is a search ora filter expand the results.
  (when (or emms-filters-stack emms-filters-search-caches)
    (run-hooks 'emms-filters-expand-render-hook)))

(defun emms-filters-ensure-metafilter (filter)
  "Ensure that FILTER is a meta-filter."
  (cond ((stringp filter)           ; name
         (emms-filters-filter-name->meta-filter filter))
        ((functionp (cdr filter))       ; filter function
         (emms-filters-filter-name->meta-filter (car filter)))
        ;; meta-filter - cdr is a listp
        (t filter)))

(defun emms-filters-push (&optional filter)
  "Push a copy of FILTER to the meta-filter stack.
Should be of the form (filter-name . metafilter/filter)
or a filter-name.

If filter is not supplied select a filter from the
  list of filter functions or create a new one.

Make a filter function and set it. If it is a name,
look it up in our filter list. If it is a function, make
it a meta-filter, if it is a meta-filter use it."
  (interactive)
  (let ((fname (or filter (emms-filters-choose-filter))))
    (push (emms-filters-ensure-metafilter fname)
          emms-filters-stack)
    (emms-filters-refilter)))

;;; base functions
(defun emms-filters-current-meta-filter-name ()
  "Give the constructed name of the current filter."
  (emms-filters-make-name (cdar emms-filters-stack)))

(defun  emms-filters-clear ()
  "Clear the meta filter stack and the current filter function."
  (interactive)
  (setq emms-filters-stack nil)
  (emms-filters-refilter))

(defun emms-filters-clear-all ()
  "Reset the cache stack, the filter stack and the filter-ring."
  (interactive)
  (emms-filters-clear)
  (emms-filters-clear-caches)
  (emms-filters-clear-ring-filter))

(defun  emms-filters-pop ()
  "Pop the stack, set the current filter function and re-render."
  (interactive)
  (pop emms-filters-stack)
  (emms-filters-refilter))

(defun  emms-filters-swap ()
  "Reverse the last two entries in the stack."
  (interactive)
  (let* ((current (pop emms-filters-stack))
         (previous (pop emms-filters-stack)))
    (push current emms-filters-stack)
    (push previous emms-filters-stack)
    (emms-filters-refilter)))

(defun  emms-filters-swap-pop ()
  "Swap and pop the stack."
  (interactive)
  (let* ((current (pop emms-filters-stack)))
    (pop emms-filters-stack)
    (push current emms-filters-stack)))

(defun  emms-filters-squash ()
  "Squash the stack, keep the top."
  (interactive)
  (let* ((current (pop emms-filters-stack)))
    (setq emms-filters-stack nil)
    (push current emms-filters-stack)))

(defun emms-filters-append-string-to-file (string filename)
  "Append STRING to FILENAME."
  (interactive)
  (append-to-file string nil filename))

(defun emms-filters-format-multi-filter (meta-filter)
  "Format the META-FILTER as Lisp code to use with `emms-filters-make-filters'."
  (format "(\"Multi-filter\"\n %S\n %S)\n\n"
          (car meta-filter)
          (cdr meta-filter)))

(defun emms-filters-save-meta-filter (meta-filter)
  "Save the META-FILTER  to the  `emms-filters-multi-filter-save-file' if set."
  (when emms-filters-multi-filter-save-file
    (append-to-file
     (emms-filters-format-multi-filter meta-filter)
     nil emms-filters-multi-filter-save-file)))

(defun  emms-filters-keep ()
  "Register the current filter into the list of filters for the
  session. If emms-filters-multi-filter-save-file is set, append the
  filter definition there."
  (interactive)
  (message "Registering the current meta-filter as a filter for the session")
  (emms-filters-status)

  (when (and emms-filters-stack (consp (car emms-filters-stack)))
    (emms-filters-save-meta-filter (car emms-filters-stack))
    (emms-filters-register-filter (caar emms-filters-stack)
                      (emms-filters-make-multi-filter (cdar emms-filters-stack)))
    (emms-filters-add-to-filter-menu "Kept filters" (caar emms-filters-stack))))

(defun emms-filters-hard-filter ()
  "A hard save of filtered results.
Build a cache of filtered tracks from the current cache
filtered by the current filters.

Emulates a search, pushing a new cache on the cache stack.
This cache is the same as all the rest and emms-cache-db.

See also: ems-pop-cache."
  (interactive)
  (let* ((search-name (emms-filters-full-name))

         (search-cache (make-hash-table
                        :test (if (fboundp 'define-hash-table-test)
                                  'string-hash
                                'equal))))
    (maphash (lambda (path track)
               (when (emms-filters-browser-filter-hook-function track)
                 (puthash path track search-cache)))
             (emms-filters-last-search-cache))

    (emms-filters-push-cache search-name search-cache))
  (emms-filters-refilter))

(defun emms-filters-choose-filter-recursive (&optional choices)
  "Choose a filter from emms-filters-filter-menu tree or the alist given
  as CHOICES. Requires that the lists of filter names be lists of cons
  (name . name). Allows for tree structures of any depth."
  (let* ((choices (or choices emms-filters-filter-menu))
         (choice (assoc (completing-read
                         "Choose a filter or group:" choices nil t)
                        choices)))
    (if (consp choice)
        (emms-filters-choose-filter-recursive (cadr choice))
      (if (string= "new filter" choice)
          (emms-filters-new-filter))
      choice)))

(defun emms-filters-choose-filter ()
  "Choose a filter from our filter menu tree.
Stupid, Assumes our tree is an alist of lists of strings."
  (let* ((choice (completing-read
                  "Choose a filter group:" emms-filters-filter-menu nil t))
         (newlist (cadr (assoc choice emms-filters-filter-menu))))
    (if newlist
        (completing-read "Choose a filter:" newlist nil t)
      (if (string= "new filter" choice)
          (emms-filters-new-filter)
        choice))))

(defun emms-filters-choose-factory ()
  "Choose a filter factory from our list of factories."
  (completing-read
   "Choose a filter factory:"
   (mapcar (lambda (factory)
             (when (car (cddr factory))
               factory))
           emms-filters-filter-factories)
   nil t))

(defun emms-filters-one-shot (&optional filter-name)
  "Push FILTER-NAME given onto the filter stack,
hard filter to create a cache, Then pop the filter.

If not given, Select or create a filter from the list of filter functions.
The filter will be used to create a new entry on the
cache stack and will be added to the filter menu.

Steps are;
 1. Take, Create, or choose a filter,
 2. Push filter,
 3. Push cache with filter,
 4. Pop filter.
If a filter was created it will remain as a filter choice for the session.
This is like browser-search, but with more choices.
"
  (interactive)
  (let ((fname
         (or filter-name
             (emms-filters-choose-filter))))
    (emms-filters-push fname)
    (emms-filters-hard-filter)
    (emms-filters-pop)))

(defun emms-filters-quick-one-shot (factory-name)
  "Create a new filter from FACTORY-NAME, using a generated filter name.
Push the filter, push the resulting cache, then pop.
Leaving a new cache on the search stack. And the filter stack as it was.
The filter will rest under the factory name filter menu for the session.
This imitates the emms browser search."
  (interactive)
  (emms-filters-one-shot (emms-filters-new-filter factory-name t)))

(defun emms-filters-smash ()
  "Clear the stack and Select a filter from the list of filter functions."
  (interactive)
  (emms-filters-clear)
  (let ((fname (emms-filters-choose-filter)))
    (emms-filters-push fname)))

(defun emms-filters-push-or (filter-name meta-filter)
  "Push a new Or with FILTER-NAME to the last Or group in the META-FILTER."
  (let* ((rev-mf (reverse (emms-filters-copy-meta-filter meta-filter)))
         (rest-mf (reverse (cdr rev-mf))))
    (append rest-mf
            (list (append (car rev-mf) (list filter-name))))))

(defun  emms-filters-or ()
  "Add filter to current/last filter list in the current filter.
Creates an `OR' filter."
  (interactive)
  (let ((fname (emms-filters-choose-filter)))
    (emms-filters-push
     (emms-filters-make-filter-cons-from-meta-filter
      (emms-filters-push-or fname (emms-filters-copy-meta-filter (cdar emms-filters-stack)))))))

(defun emms-filters-push-and (filter-name filter)
  "Push a new And list with FILTER-NAME onto FILTER."
  (append filter (list (list filter-name))))

(defun  emms-filters-and ()
  "Select a filter to start a new list of filters.
Creates a new `AND' list of filters."
  (interactive)
  (let ((fname (emms-filters-choose-filter)))
    (emms-filters-push
     (emms-filters-make-filter-cons-from-meta-filter
      (emms-filters-push-and fname (emms-filters-copy-meta-filter (cdar emms-filters-stack)))))))

(defun  emms-filters-and-not ()
  "Select a filter to start a new list of filters.
Creates a new `AND-NOT' list of filters."
  (interactive)
  (let ((fname (emms-filters-choose-filter)))
    (emms-filters-push
     (emms-filters-make-filter-cons-from-meta-filter
      (emms-filters-push-or fname
                   (emms-filters-push-and ':not (emms-filters-copy-meta-filter (cdar emms-filters-stack))))))))

(defun emms-filters-format-stack()
  "Print the stack."
  (format  "\t%s" (mapconcat 'car emms-filters-stack "\n\t")))

(defun emms-filters-full-name ()
  "Give a full name for the current filtering. Includes the ring
  filter name plus current filter name. Does not show the current cache
  name. Only show the ring filter name if its function is not nil. Use
  the current filter name so that `no filter' shows."
  (let ((ring (when (cdr emms-filters-current-ring-filter)
                (car emms-filters-current-ring-filter)))
        (current (car emms-filters-current-filter)))
    (cond ((and ring current)
           (format "%s : %s" ring current))
          (ring ring)
          (current current)
          (t nil))))

(defun emms-filters-status ()
  "Format what we know into something readable."
  (interactive)
  (format "Ring: %s\nMeta Filter: %s\nFilter stack:\n%s\nCache stack:\n %s"
          (car emms-filters-current-ring-filter)
          (emms-filters-current-meta-filter)
          (emms-filters-format-stack)
          (emms-filters-format-cache-stack)))

(defun emms-filters-status-print ()
  "Print what we know."
  (interactive)
  (message (emms-filters-status)))

(defun emms-filters-set-ring-filter (filter-name)
  "Given a FILTER-NAME set the current ring filter and re-render."
  (setq emms-filters-current-ring-filter
        (assoc filter-name emms-filters-filters))
  (emms-filters-refilter))

(defun emms-filters-clear-ring-filter ()
  "Set the ring filter to no filter."
  (interactive)
  (emms-filters-set-ring-filter "no filter"))

(defun emms-filters-current-ring-filter-name ()
  "The current ring filter name, more descriptive than car."
  (if emms-filters-current-ring-filter
      (car emms-filters-current-ring-filter)
    "no filter"))

(defun emms-filters-next-ring-filter()
  "Move to the next filter in the filter ring."
  (interactive)
  (emms-filters-set-ring-filter
   (ring-next emms-filters-filter-ring
              (if emms-filters-current-ring-filter
                  (car emms-filters-current-ring-filter)
                (ring-ref emms-filters-filter-ring 0)))))

(defun emms-filters-previous-ring-filter()
  "Move to the previous filter in the filter ring."
  (interactive)
  (emms-filters-set-ring-filter
   (ring-previous emms-filters-filter-ring
                  (if emms-filters-current-ring-filter
                      (car emms-filters-current-ring-filter)
                    (ring-ref emms-filters-filter-ring 0)))))

;; --------------------------------------------------
;; Searching
;; --------------------------------------------------
;;; The Search Cache Stack
;;
;; The search cache stack is a simply a stack of emms-cache-db style hash tables.
;; Each entry is a subset of the master emms-cache-db created through filtering.
;; Their names are constructed from the filters which created them.
;;
;; Filtering and displaying of tracks is done against the top cache on the stack.
;;
;; A cache of the current filter results can be pushed to the cache stack at any
;; time with hard-filter. These results will reflect the current meta-filter
;; as well as the filter currently chosen in the filter ring.
;;
;; A one-shot filter combined with a hard filter is emms-filters-quick-one-shot.
;; This effectively emulates the former emms-browser search behavior of
;; filtering and saving a cache by pushing a filter, hard-filter, pop.



(defun emms-filters-push-cache (&optional filter-name cache)
  "Cache/Store FILTER-NAME and CACHE in a stack.
If FILTER-NAME and CACHE are not present, interactively,
allow selection of a cache from the cache stash."
  (interactive)
  (if (and filter-name cache)
      (push (cons filter-name cache) emms-filters-search-caches)
    (let ((stashed-cache
           (assoc (completing-read "Select Cache"
                                   emms-filters-cache-stash nil t)
                  emms-filters-cache-stash)))
      (push stashed-cache emms-filters-search-caches)))
  (emms-filters-refilter))

(defun emms-filters-stash-cache ()
  "Stash the current-cache for later."
  (interactive)
  (push (car emms-filters-search-caches) emms-filters-cache-stash))

(defun emms-filters-stash-pop-cache ()
  "Stash the current-cache for later, pop it from the stack."
  (interactive)
  (emms-filters-stash-cache)
  (emms-filters-pop-cache))

(defun emms-filters-get-search-keys ()
  "Return the search-list keys for the current search cache."
  (if (< 0 (length emms-filters-search-caches))
      (mapcar #'car emms-filters-search-caches)
    '()))

(defun emms-filters-current-cache-name ()
  "Return the name of the current search cache."
  (car (reverse (emms-filters-get-search-keys))))

(defun emms-filters-format-search-list (search-list)
  "Create a string format of a SEARCH-LIST.
Search lists are what is used by the old emms-browser search function,
or the emms-filters-filter-factory `search-fields'."
  (let ((infos (append (car (car search-list))))
        (svalue (cdar search-list)))
    (format "%s - %s"
            (mapconcat
             #'(lambda (info)
                 (if (symbolp info)
                     (substring (symbol-name info)  5)
                   info))
             infos " | ")
            svalue)))

(defun emms-filters-format-cache-stack ()
  "Create a list of search crumb strings for the current search cache."
  (format "\t%s" (mapconcat #'identity (emms-filters-get-search-keys) " \n\t")))

(defun emms-filters-show-cache-stack ()
  "Message the current search cache stack."
  (interactive)
  (message "Emms Cache stack:\n  %s\n"
           (mapconcat #'identity (emms-filters-get-search-keys) "\n  ")))

(defun emms-filters-show-cache-stash ()
"Show the cache names in the stash."
(interactive)
(message "Emms cache stash:\n  %s\n"
         (mapconcat 'identity
                    (reverse (mapcar #'car emms-filters-cache-stash))
                    "\n  ")))

(defun emms-filters-last-search-cache ()
  "Return the cache portion of the last search cache entry."
  (if (< 0 (length emms-filters-search-caches))
      (cdar emms-filters-search-caches)
    emms-cache-db))

(defun emms-filters-pop-cache ()
  "Pop the search results cache and then render to show the previous search result."
  (interactive)
  (pop emms-filters-search-caches)
  (emms-filters-refilter))

(defun  emms-filters-clear-caches ()
  "Clear the cache stack."
  (interactive)
  (setq emms-filters-search-caches nil)
  (emms-filters-refilter))

(defun  emms-filters-swap-cache ()
  "Swap / reverse the last two entries in the cache stack."
  (interactive)
  (let* ((current (pop emms-filters-search-caches))
         (previous (pop emms-filters-search-caches)))
    (push current emms-filters-search-caches)
    (push previous emms-filters-search-caches)
    (emms-filters-refilter)))

(defun  emms-filters-swap-pop-cache ()
  "Swap and pop the cache stack."
  (interactive)
  (let* ((current (pop emms-filters-search-caches)))
    (pop emms-filters-search-caches)
    (push current emms-filters-search-caches)))

(defun  emms-filters-squash-caches ()
  "Squash the cache stack, keep the top entry."
  (interactive)
  (let* ((current (pop emms-filters-search-caches)))
    (setq emms-filters-search-caches nil)
    (push current emms-filters-search-caches)))

(defun emms-filters-search-stack-size ()
  "Give the current length of the search cache stack."
  (length emms-filters-search-caches))

(defun emms-filters-is-filtering ()
  "True if there is a search stack or a filter stack or a ring-filter."
  (if (or (> (length emms-filters-search-caches) 0)
          (> (length emms-filters-stack) 0)
          (if emms-filters-current-ring-filter t nil))
      t
    nil))

(defun emms-filters-empty-result-message ()
  "Display some help if the results are empty."
  (concat "No records match with the current search cache and filters.\n\n"
          (format "Cache: %s\nRing: %s\nFilter: %s\n\nEMMS Cache size: %s \n"
                  (emms-filters-current-cache-name)
                  (emms-filters-current-ring-filter-name)
                  (car emms-filters-current-filter)
                  (hash-table-count emms-cache-db))
          "
You may have created a filter with no results found.
If this is the case you may return to your previous
filter by popping the current filter.

You may also have an empty search cache on
the cache stack, popping or stashing and popping
the current searche cache may yield results.

You may also have selected a filter
in the filter ring which has no matches.
Move your filter ring selection to 'no filter'
or select a different filter for different results."))


(defun emms-filters-search-by (filter-factory-name)
  "Search using FILTER-FACTORY-NAME to create a filter.
Emulating the browser search, build a filter using factory name
and cache the results to the cache stack."
  (interactive)
  (emms-filters-quick-one-shot filter-factory-name))

;; replacements for emms-browser search and then some.
(defun emms-filters-search-by-albumartist ()
  "A fields search quick one shot for Album Artist."
  (interactive)
  (emms-filters-quick-one-shot "Album-artist"))

(defun emms-filters-search-by-artist ()
  "A fields search quick one shot for Artist."
  (interactive)
  (emms-filters-quick-one-shot "Artist"))

(defun emms-filters-search-by-composer ()
  "A fields search quick one shot for composer."
  (interactive)
  (emms-filters-quick-one-shot "Composer"))

(defun emms-filters-search-by-performer ()
  "A fields search quick one shot for performer."
  (interactive)
  (emms-filters-quick-one-shot "Performer"))

(defun emms-filters-search-by-title ()
  "A fields search quick one shot for title."
  (interactive)
  (emms-filters-quick-one-shot "Title"))

(defun emms-filters-search-by-album ()
  "A fields search quick one shot for album title."
  (interactive)
  (emms-filters-quick-one-shot "Album"))

(defun emms-filters-search-by-titles ()
  "A fields search quick one shot for album and song titles."
  (interactive)
  (emms-filters-quick-one-shot "Titles"))

(defun emms-filters-search-by-names-and-titles ()
  "A fields search quick one shot for all names and titles."
  (interactive)
  (emms-filters-quick-one-shot "Names and titles"))

(defun emms-filters-search-by-names ()
  "A fields search quick one shot for all names."
  (interactive)
  (emms-filters-quick-one-shot "Names"))

(defun emms-filters-search-by-all-text ()
  "A fields search quick one shot for All text fields."
  (interactive)
  (emms-filters-quick-one-shot "All text"))


;;; Testing
;;; -------------------------------------------------------------------
;;; Some convenience functions to make it easy to test a filter.

(defun emms-filters-test-get-track-samples (cache &optional drop take)
  "Return a list of tracks from the CACHE, DROP tracks then TAKE as indicated.
Will drop 0 and take 1O by default."
  (let* ((tracks (list))
         (drop (or drop 0))
         (take (+ (or take 10) drop))
         (counter 0))
    (maphash (lambda (_path track)
               (when
                   (and
                    (> counter drop)
                    (< counter take))
                 (push track tracks))
               (setq counter (+ counter 1)))
             cache)
    tracks))

(defun emms-filters-test-factory (factory-name parms track)
  "Create and test filter from FACTORY-NAME and PARMS.
Test it against TRACK."
  (funcall
   (emms-filters-make--filter factory-name parms)
   track))

(defun emms-filters-test-factory-interactive (factory-name track)
  "Interactively create and test filter from FACTORY-NAME.
Test it against TRACK."
  (funcall
   (emms-filters-new-filter factory-name t)
   track))

(defun emms-filters-test-filter-name (track filter-name &optional ring-filter-name)
  "Test filters identified by FILTER_NAME and RING-FILTER-NAME against a TRACK."
  (emms-filters-test-filter
   track
   (cdr (assoc filter-name emms-filters-filters))
   (if ring-filter-name
       (cdr (assoc ring-filter-name emms-filters-filters))
     nil)))

(defun emms-filters-test-filter (track filter &optional ring-filter)
  "Test TRACK against FILTER and optional RING-FILTER.
A functional equivalent to the emms-filters-browser-hook function.
First we test the track against the ring-filter, then we combine
the result with the result of the filter."
  (and (if ring-filter
           (funcall ring-filter track)
         t)
       (if filter
           (funcall filter track)
         t)))

(defun emms-filters-test-filter-tracks-direct (factory-name parms tracks)
  "Test a list of TRACKS against a filter created from FACTORY-NAME and
  PARMS. Uses emms-filters-test-factory directly rather than emulating the
  browser-hook-function. Test it against some portion starting with START
  records and stopping at STOP records of the existing cache-db. Returns
  a list of cons with the filter result and the track."
  (mapcar (lambda (track)
            (cons
             (emms-filters-test-factory factory-name parms track)
             track))
          tracks))

(defun emms-filters-test-filter-tracks (factory-name parms tracks)
  "Test a list of TRACKS against a filter created from FACTORY-NAME and PARMS.
Emulates the browser-hook function by using emms-filters-test-filter.
Test it against some portion starting with START records and stopping
at STP records of the existing cache-db.
Returns a list of cons with the filter result and the track."
  (let ((filter (emms-filters-make--filter factory-name parms)))
    (mapcar (lambda (track)
              (cons
               (emms-filters-test-filter track filter)
               track))
            tracks)))

(defun emms-filters-test-filter-tracks-name (filter-name tracks)
  "Test a list of TRACKS against a FILTER-NAME.
Emulates the browser-hook function by using emms-filters-test-filter.
Test it against some portion starting with START records and stopping
at STP records of the existing cache-db.
Returns a list of cons with the filter result and the track."
  (let ((filter (cdr (assoc filter-name emms-filters-filters))))
    (mapcar (lambda (track)
              (cons
               (emms-filters-test-filter-name track filter)
               track))
            tracks)))

(defun emms-filters-test-find-tracks (cache filter)
  "Return a list of tracks from the CACHE filtered by function FILTER."
  (let* ((tracks (list)))
    (maphash (lambda (_path track)
               (when (funcall filter track)
                 (push track tracks)))
             cache)
    tracks))

(defun emms-filters-test-find-tracks-with-name (cache filter-name)
  "Return a list of tracks from the CACHE filtered by function FILTER."
  (let* ((tracks (list)))
    (maphash (lambda (_path track)
               (when (funcall (cdr (assoc filter-name emms-filters-filters)) track)
                 (push track tracks)))
             cache)
    tracks))

;; ;;; Testing
;; ;;; Some actual testing.
;; ;;; Some sample tracks to use for data.
;; (setq emms-filters-test-tracks
;;       '((*track* (type . file)
;;                  (name . "/Someone/Some-album/Some-song/track0001")
;;                  (info-playing-time . 180)
;;                  (info-discnumber . "1")
;;                  (info-artist . "Someone-else")
;;                  (info-title . "Some-song")
;;                  (info-tracknumber . "01")
;;                  (info-album . "Some-album")
;;                  (info-albumartist . "Someone")
;;                  (info-year . 1940)
;;                  (info-genre . "vals"))
;;         (*track* (type . file)
;;                  (name . "/Another-one/Another-album/Another-song/track0002")
;;                  (info-playing-time . 180)
;;                  (info-discnumber . "1")
;;                  (info-artist . "Another-Someone-else")
;;                  (info-title . "Another-song")
;;                  (info-tracknumber . "02")
;;                  (info-album . "Another-album")
;;                  (info-albumartist . "Another-one")
;;                  (info-year . 1935)
;;                  (info-genre . "tango"))))

;; (defun pretty-cons (cons-list)
;;   "pretty print a list of cons."
;;   (mapconcat (lambda (str) (format "%s\n" str))
;;              cons-list))

;; (defun emms-filters-do-tests ()
;;   "A function for isolating and running some tests."
;;   ;; Make some sample data from the first few tracks from the cache.
;;   (let  ((emms-filters-test-tracks-sample
;;           (emms-filters-test-get-track-samples emms-cache-db))
;;          (first-test-track (car emms-filters-test-tracks))
;;          (second-test-track (cadr emms-filters-test-tracks)))

;; A direct use of the generated filter.

;; ;; Create a filter from a factory and test it against a single track.
;; (emms-filters-test-factory "Genre" '("vals") first-test-track)
;; (emms-filters-test-factory "Genre" '("vals") second-test-track)

;; (emms-filters-test-factory "Titles" "Some" first-test-track)
;; (emms-filters-test-factory "Titles" "Some" second-test-track)

;; ;; Test a few tracks against it.
;; (pretty-cons (emms-filters-test-filter-tracks "Genre" '("vals") emms-filters-test-tracks))
;; (pretty-cons (emms-filters-test-filter-tracks "Genre" '("vals") emms-filters-test-tracks-sample))
;; (pretty-cons (emms-filters-test-filter-tracks "Titles" '("Some") emms-filters-test-tracks))
;; (pretty-cons (emms-filters-test-filter-tracks "Titles" '("Some") emms-filters-test-tracks-sample))
;; (pretty-cons (emms-filters-test-filter-tracks "Titles" '("Viv") emms-filters-test-tracks-sample))

;; (emms-filters-test-find-tracks emms-cache-db (emms-filters-make--filter "Titles" '("sollo")))

;; ;; Test interactive creation of a filter from a factory.
;; ;; create a filter from a factory and test it against a single track.
;; (emms-filters-test-factory-interactive "Genre" first-test-track)
;; (emms-filters-test-factory-interactive "Titles" first-test-track)))

;; Testing Backward compatibility with the emms-browser.
;; -------------------------------------------------------
;; Make some old style browser filters to test
;; the filter-ring backward compatibility.
;; Steps to test:
;; 1. Make some old style emms-browser filters,
;; 3. Try them out directly by name.
;;
;; emms-browser-make-filter now inverts the filter result
;; for compatibility with emms-filters. The only interface to them
;; were next and previous functions.
;; That functionality is replicated with the emms-filters-filter-ring.

;; (defun emms-browser-make-filter-genre (genre)
;;   "Make a filter by GENRE."
;;   (let ((filter (funcall emms-filters-make-filter-genre genre)))
;;     (lambda (track)
;;       (not (filter track)))))

;; (defun emms-browser-make-filter-genre (genre)
;;   "Make a filter by GENRE."
;;   (lambda (track)
;;     (let ((info (emms-track-get track 'info-genre)))
;;       (not (and info (string-equal-ignore-case genre info))))))
;;
;; (emms-browser-make-filter "test-vals"
;;                           (emms-browser-make-filter-genre "vals"))
;; (emms-browser-make-filter "test-tango"
;;                           (emms-browser-make-filter-genre "tango"))
;; (emms-browser-make-filter "test-milonga"
;;                           (emms-browser-make-filter-genre "milonga"))

;; emms-filters-filter-ring
;; (pretty-cons (emms-filters-test-filter-tracks-name "test-vals" emms-filters-test-tracks))

(provide 'emms-filters)
;;; emms-filters.el ends here.
