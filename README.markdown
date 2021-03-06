githubのリポジトリに対するpushEventをまとめて出力するツール。

出力フォーマット:
- HTML
- Markdown
- RST
- Mediawiki

### Install
```sh
git clone git://github.com/joker1007/github-push-reporter.git
cd github-push-reporter
cabal-dev install
```

### Usage
```
Usage: github-push-reporter [OPTION...]

  -h                --help                         Show this help.
  -r REPOSITORIES   --repositories=REPOSITORIES    Target Repositories (separated by comma)
  -l LOGIN          --login=LOGIN                  Github login ID
  -f FILE           --file=FILE                    Repository config (per line '<user>/<repository name>'
  -o OUTPUT-FORMAT  --output-format=OUTPUT-FORMAT  Output file format
  -d DATE           --date=DATE                    Filter by date
  -a AUTHOR         --author=AUTHOR                Filter by author
```

Config file format is following:
```
joker1007/github-push-reporter
joker1007/pasokara_player3
```

### Example
```sh
github-push-reporter -r joker1007/github-push-reporter,joker1007/pasokara_player3 -l joker1007
# -> output report.html

github-push-reporter -f repositories.txt -l joker1007 -o markdown
# -> output report.md
```
