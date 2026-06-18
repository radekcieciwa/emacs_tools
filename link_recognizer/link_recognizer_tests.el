
;; Eval checks with C-M-x

(load-file "link_recognizer.el")

;;(setq debug-on-error t)

(let (
      (t1 (jira-link-recognizer "https://bmbl.atlassian.net/browse/IOS-33361"))
      )
      (assert (equal '("https://bmbl.atlassian.net/browse/IOS-33361" . "IOS-33361") t1))
      )

;; usergroup-link-recognizer

(let (
      (t1 (usergroup-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081"))
      (t2 (usergroup-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081" . "UserGroup-2081") t1))
      (assert (equal nil t2))
  )

;; usersplit-link-recognizer

(let (
      (t1 (usersplit-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))
      (t2 (usersplit-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595" . "UserSplit-9595") t1))
      (assert (equal nil t2))
      )

;; mapped-domain-link-recognizer

;; (car (car recognized-domains))
;; (cdr recognized-domains)
(mapped-domain-link-recognizer "https://www.youtube.com/watch?v=XNgRgBP0chY")
(mapped-domain-link-recognizer "https://console.cloud.google.com/cloud-build/builds;region=us-central1/e80b096d-4136-4cdb-9f28-fb181c771590?project=p-b4f-bld-1428")

(let (
      (t1 (mapped-domain-link-recognizer "https://www.youtube.com/watch?v=XNgRgBP0chY"))
      (t2 (mapped-domain-link-recognizer "https://www.onet.pl"))      
      )
      (assert (equal '("https://www.youtube.com/watch?v=XNgRgBP0chY" . "YouTube") t1))
      (assert (equal nil t2))
      )

;; teamcity-link-recognizer

(let (
      (t1 (teamcity-link-recognizer "https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522?buildTab=log&focusLine=950&logView=flowAware&linesState=190"))
      (t2 (teamcity-link-recognizer "https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522"))
      (t3 (teamcity-link-recognizer "https://mobile-ci.bumble.dev/project/IOS_StaticQualityGate_Infrastructure"))
      )
      (assert (equal '("https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522?buildTab=log&focusLine=950&logView=flowAware&linesState=190" . "TeamCity/IOS_StaticQualityGate_Infrastructure/31245522") t1))
      (assert (equal '("https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522" . "TeamCity/IOS_StaticQualityGate_Infrastructure/31245522") t2))
      (assert (equal nil t3))
      )

;; teamcity-link-recognizer via interceptor

(let (
      (t1 (insert-link-interceptor-for-value "https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522?buildTab=log&focusLine=950&logView=flowAware&linesState=190"))
      )
      (assert (equal '("https://mobile-ci.bumble.dev/buildConfiguration/IOS_StaticQualityGate_Infrastructure/31245522?buildTab=log&focusLine=950&logView=flowAware&linesState=190" . "TeamCity/IOS_StaticQualityGate_Infrastructure/31245522") t1))
      )

;; github-enterprise-pull-link-recognizer

(let (
      (t1 (github-enterprise-pull-link-recognizer "https://github.bumble.dev/ios/bumble/pull/22891/files"))
      (t2 (github-enterprise-pull-link-recognizer "https://github.bumble.dev/ios/bumble/pull/22891"))
      (t3 (github-enterprise-pull-link-recognizer "https://github.com/ios/bumble/pull/22891"))
      )
      (assert (equal '("https://github.bumble.dev/ios/bumble/pull/22891/files" . "ghe/ios/bumble/PR#22891") t1))
      (assert (equal '("https://github.bumble.dev/ios/bumble/pull/22891" . "ghe/ios/bumble/PR#22891") t2))
      (assert (equal nil t3))
      )

;; github-enterprise-pull-link-recognizer via interceptor

(let (
      (t1 (insert-link-interceptor-for-value "https://github.bumble.dev/ios/bumble/pull/22891/files"))
      )
      (assert (equal '("https://github.bumble.dev/ios/bumble/pull/22891/files" . "ghe/ios/bumble/PR#22891") t1))
      )

;; org-insert-link-interceptor

;; (shell-command-to-string "pbpaste")
;; (funcall-interactively #'org-insert-link)

(insert-link-interceptor-for-value "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595")
(insert-link-interceptor-for-value "https://www.youtube.com/watch?v=RQ0f7dBcDtI")

(let (
      (t1 (insert-link-interceptor-for-value "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))
      (t2 (insert-link-interceptor-for-value "https://www.youtube.com/watch?v=RQ0f7dBcDtI"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595" . "UserSplit-9595") t1))
      (assert (equal nil t2))
  )
