# bee-cloud-factory-static 蜜云工厂配置

## 目录结构

#### 最外层
* 打包后文件夹（发行文件夹）  dist
* 源码目录  src
* umi 配置 .umirc.js

#### 里层
* 常量配置文件(请求地址前缀、后天服务api前缀) src/constants/
* 公共文件文件夹（全局路由文件） src/common/
* 最外层父组件布局文件（处理一些子组件渲染前的初始化事件） src/layouts
* dva的model文件 */models
* 请求服务文件夹（包含api与services文件） */services
* 本地组件库 */components
* 静态资源 src/assets
* 工具文件夹 src/utils/
* 业务代码文件夹 src/pages/
* 全局js，项目初始化时执行 src/global.js
* 全局less，会打包进公共css，可用于覆盖原生样式或者第三方组件库样式 src/global.less

#### umi.js项目目录(官方)

├── dist/                          // 默认的 build 输出目录
├── mock/                          // mock 文件所在目录，基于 express
├── config/
    ├── config.js                  // umi 配置，同 .umirc.js，二选一
└── src/                           // 源码目录，可选
    ├── layouts/index.js           // 全局布局
    ├── pages/                     // 页面目录，里面的文件即路由
        ├── .umi/                  // dev 临时目录，需添加到 .gitignore
        ├── .umi-production/       // build 临时目录，会自动删除
        ├── document.ejs           // HTML 模板
        ├── 404.js                 // 404 页面
        ├── page1.js               // 页面 1，任意命名，导出 react 组件
        ├── page1.test.js          // 用例文件，umi test 会匹配所有 .test.js 和 .e2e.js 结尾的文件
        └── page2.js               // 页面 2，任意命名
    ├── global.css                 // 约定的全局样式文件，自动引入，也可以用 global.less
    ├── global.js                  // 可以在这里加入 polyfill
    ├── app.js                     // 运行时配置文件
├── .umirc.js                      // umi 配置，同 config/config.js，二选一
├── .env                           // 环境变量
└── package.json

---
---

## 分支介绍

* 无前缀分支（如master）  公共分支/主分支
* jinda-* 金达分支

---
---

## 项目环境配置
### 项目基础配置
####①　npm install -g umi(如未安装umi依赖)
####②　npm install
####③　启动本地:umi dev (默认监听8000端口) 
####③　启动本地:port=端口号 umi dev (监听端口启动) 
####④　打包(dist文件夹) 
*  npm run build (开发环境打包，默认打包)
*  npm run build:pro (正式环境打包)
*  npm run build:qa (测试qa打包)
*  npm run build:qa1 (测试qa1打包)

