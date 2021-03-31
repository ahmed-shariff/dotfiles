;;; package --- Summary

;;; Commentary:

;;; Code:

(setq configurations (make-hash-table :test 'equal))
(puthash 'use-pdf-tools nil configurations)
(puthash 'use-jupyter nil configurations)
(puthash 'use-tabnine t configurations)

(provide 'configurations)
;;; configurations.el ends here
