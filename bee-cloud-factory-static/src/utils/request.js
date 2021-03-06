import fetch from 'dva/fetch'
import { notification, message } from 'antd'
import router from 'umi/router'
import { domain } from '@/constants/prefix'
const codeMessage = {
  200: '服务器成功返回请求的数据。',
  201: '新建或修改数据成功。',
  202: '一个请求已经进入后台排队（异步任务）。',
  204: '删除数据成功。',
  400: '发出的请求有错误，服务器没有进行新建或修改数据的操作。',
  401: '用户没有权限（令牌、用户名、密码错误）。',
  403: '用户得到授权，但是访问是被禁止的。',
  404: '发出的请求针对的是不存在的记录，服务器没有进行操作。',
  406: '请求的格式不可得。',
  410: '请求的资源被永久删除，且不会再得到的。',
  422: '当创建一个对象时，发生一个验证错误。',
  500: '服务器发生错误，请检查服务器。',
  502: '网关错误。',
  503: '服务不可用，服务器暂时过载或维护。',
  504: '网关超时。'
}
function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response
  }
  const errortext = codeMessage[response.status] || response.statusText
  notification.error({
    message: `请求错误 ${response.status}: ${response.url}`,
    description: errortext
  })
  const error = new Error(errortext)
  error.name = response.status
  error.response = response
  throw error
}

/**
 * Requests a URL, returning a promise.
 *
 * @param  {string} url       The URL we want to request
 * @param  {object} [options] The options we want to pass to "fetch"
 * @param  {boolean} false  get file
 * @return {object}           An object containing either "data" or "err"
 */
export default function request(url, options = {}, isDownload) {
  //非正式环境添加请求地址端口与模拟mock
  if (url.indexOf('http') === -1) {
    // url = str + url
    url = domain + url
  }

  const defaultOptions = {
    credentials: 'include',
    headers: {
      // financeToken: localStorage.financeToken,
      sysToken: localStorage.sysToken,
      cloudMafType: 'cloud_maf_web'
      // beeRouter: window.location.pathname
    }
    // mode:'no-cors'
  }
  const newOptions = { ...defaultOptions, ...options }
  if (
    newOptions.method === 'POST' ||
    newOptions.method === 'PUT' ||
    newOptions.method === 'DELETE'
  ) {
    if (!(newOptions.body instanceof FormData)) {
      newOptions.headers = {
        Accept: 'application/json',
        'Content-Type': 'application/json; charset=utf-8',
        ...newOptions.headers
      }
      newOptions.body = JSON.stringify(newOptions.body)
    } else {
      // newOptions.body is FormData
      newOptions.headers = {
        Accept: 'application/json',
        ...newOptions.headers
      }
    }
  }
  return (
    fetch(url, newOptions)
      .then(checkStatus)
      .then(response => {
        // if (newOptions.method === 'DELETE' || response.status === 204) {
        //   return response.text()
        // }
        if (isDownload) {
          return response.blob()
        }

        return response.json()
      })
      // .then(result => {//统一处理后端返回的一部分异常代码
      //   if (result.code === -20) {
      //     message.success('没有角色权限，请联系管理员开通');
      //     router.push('/role')
      //   }
      //   return new Promise((resolve, reject) => {
      //     resolve(result)
      //   })
      // })
      .catch(e => {
        return {
          code: 500,
          message: '服务器异常，请重试！',
          msg: '服务器异常，请重试！'
        }
      })
  )
}
