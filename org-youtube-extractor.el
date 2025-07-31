(defun org-youtube-extractor--get-title (url)
  "Fetches the title of a YouTube video from its URL using yt-dlp."
  (let ((process-connection-type nil)) ; Use a pipe
    (with-temp-buffer
      (let ((exit-code (call-process "yt-dlp" nil t nil "--get-title" url)))
        (if (zerop exit-code)
            (string-trim (buffer-string))
          (message "yt-dlp for %s exited with code %s" url exit-code)
          nil)))))

(provide 'org-youtube-extractor)
