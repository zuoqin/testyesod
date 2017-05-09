# Adding new route
1. Copy templates\storiespage.hamlet and templates\storiespage.lucius
2. Copy Handler\Page
3. Add isAuthorized PageR _ = return Authorized in Foundation.hs
4. Add import Handler.Page in Application.hs
5. Copy test\Handler\PageSpec.hs
6. Correct templates\default-layout.hamlet: $if not $ Just PageR == mcurrentRoute

