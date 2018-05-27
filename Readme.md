

download all

```bash
git clone --recursive <this>
```

update all

```bash
git submodule update --recursive
```




add one from github repo

```bash
git submodule add <repo> Repos/<name>/source
```

add `-b <branch>` if not use **master**



```bash
git submodule update --remote
```


 remove


```bash
git submodule deinit --force <path>
```


git submodule add git@github.com:Moe-Net/BilibiliLink.git Repos/BilibiliLink/source