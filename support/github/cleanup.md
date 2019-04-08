<!-- Search and Delete Files with Suffix: example .diary suffix -->
git ls-files | grep '\.diary$' | xargs git rm
<!-- git ls-files | grep '\.mat$' | xargs git rm -->
