# DockerManager
使用`xterm.js`, `Node.js` 和 `Socket.io`构建的简单网络UI界面，
中文版本，此外发现目前的错误在cmd下面退出机制有问题，会导致系统停止。

## Requirement

- Node.js
- Docker remote api >= v1.24
- macOs or Linux or windows

## 开发模式

```bash
git clone https://github.com/mdranger/mytest.git
cd mytest
npm i 
npm start
```

之后可以使用 url 访问浏览器：http://localhost:8091

## Docker模式

首先启动DOCKER，确定Dockerfile文件在当前目录下，
然后构建Docker image：
```bash
 docker build --tag docker-manager:1.0 .
```

创建成功之后会显示创建DOCKER IMAGE的信息：
```bash
Successfully built 2f2f241c2878
Successfully tagged docker-manager:1.0
```

Docker 容器可以启动如下： 
```bash
docker run -p8090:8091 -v /var/run/docker.sock:/var/run/docker.sock docker-manager:1.0
```
注意这里必须加上 -v 的路径用于连接 DOCKER 和运行的容器，否则无法正常访问 Docker 接口，会报错误

```bash
Docker/Node error: Error: connect ENOENT /var/run/docker.sock
```

成功启动容器后，可以使用 url 访问浏览器：http://localhost:8090





