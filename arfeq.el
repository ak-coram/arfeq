;;; arfeq.el --- ᐊᕐᕕᒃ porcelain -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ákos Kiss <ak@coram.pub>

;; Author: Ákos Kiss <ak@coram.pub>
;; Version: 0.0.1
;; Keywords: jira

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'aio)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'json)
(require 'parse-time)

(setq arfeq--max-results-default 1000000
      arfeq--pagination-batch-size 5
      arfeq--indent-size 2
      arfeq--jira-url (concat jira-url "/rest/api/3/")
      arfeq--agile-url (concat jira-url "/rest/agile/1.0/")
      arfeq--headers
      `(("Authorization" .
         ,(format "Basic %s" (base64-encode-string
                              (concat jira-username ":" jira-token))))
        ("Content-Type" . "application/json; charset=utf8")
        ("Accept-Charset" . "utf-8")))

(aio-defun arfeq--api (api method endpoint params data)
  (let* ((url-request-method method)
         (url-request-extra-headers arfeq--headers)
         (url-request-data (json-serialize data))
         (url-http-attempt-keepalives nil)
         (result (aio-await (aio-url-retrieve (concat (pcase api
                                                        ('jira arfeq--jira-url)
                                                        ('agile arfeq--agile-url))
                                                      endpoint)
                                              nil
                                              t)))
         (contents (with-current-buffer (cdr result)
                     (narrow-to-region url-http-end-of-headers (point-max))
                     (prog1 (buffer-string)
                       (kill-buffer)))))
    (json-parse-string contents)))

(defun arfeq--get-pagination-batches
    (start-at-value-idx values-per-rq total-value-count)
  (-partition-all arfeq--pagination-batch-size
                  (cl-loop for i = (+ start-at-value-idx
                                      values-per-rq)
                           then (+ i values-per-rq)
                           until (< total-value-count i)
                           collect `(startAt . ,i))))

(aio-defun arfeq--whack-pagination (api-fn params result-key &optional acc)
  (let* ((results (aio-await (funcall api-fn params)))
         (start-at-value-idx (ht-get results "startAt"))
         (values-per-rq (ht-get results "maxResults"))
         (total-value-count (ht-get results "total"))
         (values (ht-get results result-key))
         (batches (arfeq--get-pagination-batches start-at-value-idx
                                                 values-per-rq
                                                 total-value-count)))
    (cl-loop with acc = values
             for batch in batches
             for promises = (cl-loop
                             for start-param in batch
                             collect (funcall api-fn
                                              (cons start-param params)))
             do (cl-loop for promise in promises
                         do (setq acc (vconcat acc (ht-get (aio-await promise)
                                                           result-key))))
             finally (return acc))))

(aio-defun arfeq--get-issue-completion-list (query)
  (let* ((issues (aio-await
                  (arfeq--whack-pagination (-partial #'arfeq--api
                                                     'jira
                                                     "POST"
                                                     "search"
                                                     nil)
                                           `((jql . ,query)
                                             (maxResults . ,arfeq--max-results-default)
                                             (fields . ,(vector "key" "summary")))
                                           "issues"))))
    (-map (lambda (issue)
            (let* ((issue-key (ht-get issue "key"))
                   (fields (ht-get issue "fields"))
                   (summary (ht-get fields "summary")))
              `(,(concat issue-key " " summary) . ,issue-key)))
          issues)))

(aio-defun arfeq--complete-epic (prompt)
  (let ((epics (aio-await (arfeq--get-issue-completion-list
                           "type = epic"))))
    (-> (completing-read prompt epics)
        (assoc-string epics)
        cdr)))

;; (arfeq--complete-epic "Select epic: ")

