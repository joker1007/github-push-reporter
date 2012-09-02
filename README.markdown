githubのリポジトリに対するpushEventをまとめて出力するツール。

出力フォーマット:
- HTML
- Markdown
- RST
- Mediawiki

### Usage
```sh
github-push-reporter -r joker1007/github-push-reporter,joker1007/pasokara_player3 -l joker1007
# -> output report.html

github-push-reporter -c repositories.txt -l joker1007 -f markdown
# -> output report.md
```
