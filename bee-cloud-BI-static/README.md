#前端代码规范 ###① 　组件的文件命名与组件名一致。 ###② 　同一模块内组件用 index.js 导出。 ###③ 　组件代码内 export default 组件统一命名为组件名。 ###④ 　注释(api.js、model 文件夹下、) ###⑤ 　变量命名语义化 ###⑤ 　函数、判断语句、循环语句前空一行 ###⑤ 　文件夹命名使用 大驼峰、 ###⑤ 　公用组件 注释需写明接收参数，抛出方法所带数据标明。 ###⑤ 　不能有大量冗余。

#环境配置 ###① 　 npm install -g umi(如未安装 umi 依赖) ###② 　 npm install ###③ 　启动本地:umi dev (默认监听 8000 端口) ###③ 　启动本地:port=8888 umi dev (监听 8888 端口启动) ###④ 　打包(dist 文件夹) :

#### npm run build (开发环境打包，默认打包)

#### npm run build:pro (正式环境打包)

#### npm run build:qa (测试 qa 打包)

#### npm run build:qa1 (测试 qa1 打包)

#### npm run build:scft (客户测试 scft 打包)

#地址修改 ###① 　后台访问域名修改路径:/src/utils/api.js ( baseUrl 为 bussiness 模块,userUrl 为 user 模块,)

### config

项目打包配置， 项目路由

### mock

项目 mock 接口

### src/components

项目业务组件，

### constants && consts

项目常量

### services

公共接口，即可多次复用的接口

### utils

工具集

### 列表查询组件模板

```js
// 组件位置
src\components\FormWidget

// 使用示例
src\pages\InventoryManage\productStorage

```
