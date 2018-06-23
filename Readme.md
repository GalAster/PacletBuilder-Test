

### Download all

```bash
git clone --recursive git@github.com:paclets/PacletBuilder-Test.git
```

Do not need `--recursive` unless on a build server.

### Add your repo

```bash
git submodule add <repo> Repos/<name>/source
```

Add `-b <branch>` if not use **master**


### Update

```bash
git submodule update --recursive
```


### Configs

Do some configs like other projects.


### Remove


```bash
git submodule deinit --force <path>
git rm --cached <path>
```

Delete both **the file, git config and gitmodules record**.