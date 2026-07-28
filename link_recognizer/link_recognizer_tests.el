
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

;; github-url-parts

(let (
      (t1 (github-url-parts "https://github.com/ios/bumble/pull/22891/files?diff=split#discussion_r1"))
      (t2 (github-url-parts "https://github.bumble.dev/ios/bumble"))
      (t3 (github-url-parts "https://gitlab.com/ios/bumble/pull/1"))
      )
      (assert (equal '("gh" "ios" "bumble" "pull/22891/files" "discussion_r1") t1))
      (assert (equal '("ghe" "ios" "bumble" "" nil) t2))
      (assert (equal nil t3))
      )

;; github-link-recognizer -- pull requests

(let (
      (t1 (github-link-recognizer "https://github.com/apple/swift/pull/12345"))
      (t2 (github-link-recognizer "https://github.com/apple/swift/pull/12345/files"))
      (t3 (github-link-recognizer "https://github.com/apple/swift/pull/12345/commits/1a2b3c4d5e6f7a8b"))
      (t4 (github-link-recognizer "https://github.com/apple/swift/pull/12345/checks?check_run_id=987"))
      (t5 (github-link-recognizer "https://github.com/apple/swift/pull/12345#discussion_r55555"))
      (t6 (github-link-recognizer "https://github.bumble.dev/ios/bumble/pull/22891/files"))
      )
      (assert (equal "gh/apple/swift/PR#12345" (cdr t1)))
      (assert (equal "gh/apple/swift/PR#12345" (cdr t2)))
      (assert (equal "gh/apple/swift/PR#12345" (cdr t3)))
      (assert (equal "gh/apple/swift/PR#12345" (cdr t4)))
      (assert (equal "gh/apple/swift/PR#12345" (cdr t5)))
      (assert (equal "ghe/ios/bumble/PR#22891" (cdr t6)))
      (assert (equal "https://github.com/apple/swift/pull/12345/files" (car t2)))
      )

;; github-link-recognizer -- GitHub Actions

(let (
      (t1 (github-link-recognizer "https://github.com/apple/swift/actions/runs/16123456789"))
      (t2 (github-link-recognizer "https://github.com/apple/swift/actions/runs/16123456789/job/45566778899"))
      (t3 (github-link-recognizer "https://github.com/apple/swift/actions/runs/16123456789/job/45566778899#step:5:120"))
      (t4 (github-link-recognizer "https://github.com/apple/swift/actions/runs/16123456789/attempts/2"))
      (t5 (github-link-recognizer "https://github.com/apple/swift/actions/runs/16123456789/workflow"))
      (t6 (github-link-recognizer "https://github.com/apple/swift/actions/jobs/45566778899"))
      (t7 (github-link-recognizer "https://github.com/apple/swift/actions/workflows/ci.yml?query=branch%3Amain"))
      (t8 (github-link-recognizer "https://github.com/apple/swift/actions"))
      (t9 (github-link-recognizer "https://github.bumble.dev/ios/bumble/actions/runs/778899/job/112233"))
      )
      (assert (equal "gh/apple/swift/run#16123456789" (cdr t1)))
      (assert (equal "gh/apple/swift/run#16123456789 job#45566778899" (cdr t2)))
      (assert (equal "gh/apple/swift/run#16123456789 job#45566778899" (cdr t3)))
      (assert (equal "gh/apple/swift/run#16123456789 (attempt 2)" (cdr t4)))
      (assert (equal "gh/apple/swift/run#16123456789" (cdr t5)))
      (assert (equal "gh/apple/swift/job#45566778899" (cdr t6)))
      (assert (equal "gh/apple/swift/workflow ci.yml" (cdr t7)))
      (assert (equal "gh/apple/swift/actions" (cdr t8)))
      (assert (equal "ghe/ios/bumble/run#778899 job#112233" (cdr t9)))
      )

;; github-link-recognizer -- other repository places

(let (
      (t1 (github-link-recognizer "https://github.com/apple/swift/issues/777"))
      (t2 (github-link-recognizer "https://github.com/apple/swift/discussions/42"))
      (t3 (github-link-recognizer "https://github.com/apple/swift/commit/1a2b3c4d5e6f7a8b9c0d"))
      (t4 (github-link-recognizer "https://github.com/apple/swift/compare/main...feature-branch"))
      (t5 (github-link-recognizer "https://github.com/apple/swift/releases/tag/swift-6.0-RELEASE"))
      (t6 (github-link-recognizer "https://github.com/apple/swift/blob/main/Sources/Core/Engine.swift#L10-L24"))
      (t7 (github-link-recognizer "https://github.com/apple/swift/blob/main/README.md"))
      (t8 (github-link-recognizer "https://github.com/apple/swift/tree/release/6.0"))
      (t9 (github-link-recognizer "https://github.com/apple/swift/wiki/Getting-Started"))
      (t10 (github-link-recognizer "https://github.com/apple/swift/milestone/3"))
      (t11 (github-link-recognizer "https://github.com/apple/swift"))
      (t12 (github-link-recognizer "https://github.com/apple/swift/settings/hooks"))
      (t13 (github-link-recognizer "https://gitlab.com/apple/swift/pull/1"))
      )
      (assert (equal "gh/apple/swift/issue#777" (cdr t1)))
      (assert (equal "gh/apple/swift/discussion#42" (cdr t2)))
      (assert (equal "gh/apple/swift/commit 1a2b3c4" (cdr t3)))
      (assert (equal "gh/apple/swift/compare main...feature-branch" (cdr t4)))
      (assert (equal "gh/apple/swift/release swift-6.0-RELEASE" (cdr t5)))
      (assert (equal "gh/apple/swift/Engine.swift:10-24" (cdr t6)))
      (assert (equal "gh/apple/swift/README.md" (cdr t7)))
      (assert (equal "gh/apple/swift/tree release/6.0" (cdr t8)))
      (assert (equal "gh/apple/swift/wiki Getting Started" (cdr t9)))
      (assert (equal "gh/apple/swift/milestone#3" (cdr t10)))
      (assert (equal "gh/apple/swift" (cdr t11)))
      (assert (equal "gh/apple/swift" (cdr t12)))
      (assert (equal nil t13))
      )

;; github-org-link-recognizer / github-owner-link-recognizer / github-gist-link-recognizer

(let (
      (t1 (github-org-link-recognizer "https://github.com/orgs/apple/projects/7/views/1"))
      (t2 (github-org-link-recognizer "https://github.com/apple/swift/pull/1"))
      (t3 (github-owner-link-recognizer "https://github.com/apple"))
      (t4 (github-owner-link-recognizer "https://github.com/apple/swift"))
      (t5 (github-gist-link-recognizer "https://gist.github.com/radek/1a2b3c4d5e6f7a8b9c0d1e2f"))
      )
      (assert (equal "gh/apple/project#7" (cdr t1)))
      (assert (equal nil t2))
      (assert (equal "gh/apple" (cdr t3)))
      (assert (equal nil t4))
      (assert (equal "gist/radek/1a2b3c4" (cdr t5)))
      )

;; github recognizers via interceptor

(let (
      (t1 (insert-link-interceptor-for-value "https://github.com/apple/swift/pull/12345/files"))
      (t2 (insert-link-interceptor-for-value "https://github.bumble.dev/ios/bumble/actions/runs/778899/job/112233"))
      (t3 (insert-link-interceptor-for-value "https://github.com/orgs/apple/projects/7"))
      (t4 (insert-link-interceptor-for-value "https://gist.github.com/radek/1a2b3c4d5e6f7a8b9c0d1e2f"))
      (t5 (insert-link-interceptor-for-value "https://github.com/apple"))
      )
      (assert (equal "gh/apple/swift/PR#12345" (cdr t1)))
      (assert (equal "ghe/ios/bumble/run#778899 job#112233" (cdr t2)))
      (assert (equal "gh/apple/project#7" (cdr t3)))
      (assert (equal "gist/radek/1a2b3c4" (cdr t4)))
      (assert (equal "gh/apple" (cdr t5)))
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
      (assert (equal '("https://www.youtube.com/watch?v=RQ0f7dBcDtI" . "YouTube") t2))
  )
