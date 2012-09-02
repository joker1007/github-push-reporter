githubのリポジトリに対するpushEventをまとめて出力するツール。

出力フォーマット:
- HTML
- Markdown
- RST
- Mediawiki

### Usage
```
Usage: github-push-reporter [OPTION...]

  -h               --help                       Show this help.
  -r REPOSITORIES  --repositories=REPOSITORIES  Target Repositories (separated by comma)
  -l LOGIN         --login=LOGIN                Github login ID
  -c CONF          --conf=CONF                  Repository config
  -f FORMAT        --format=FORMAT              Output file format
  -d DATE          --date=DATE                  Filter by date
  -a AUTHOR        --author=AUTHOR              Filter by author
```

### Example
```sh
github-push-reporter -r joker1007/github-push-reporter,joker1007/pasokara_player3 -l joker1007
# -> output report.html

github-push-reporter -c repositories.txt -l joker1007 -f markdown
# -> output report.md
```
