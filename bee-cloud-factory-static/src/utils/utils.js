import moment from 'moment'
import { parse, stringify } from 'qs'

export function fixedZero(val) {
  return val * 1 < 10 ? `0${val}` : val
}

export const moneyExp = /(^[1-9]([0-9]{1,9})?(\.[0-9]{1,2})?$)|(^(0){1}$)|(^[0-9]\.[0-9]{1,9}([0-9])?$)/gi //金额填写， 最多两位小数
export const moneyExp1 = /(^[-]?[1-9]([0-9]{1,9})?(\.[0-9]{1,2})?$)|(^(0){1}$)|(^[-]?[0-9]\.[0-9]{1,9}([0-9])?$)/gi //金额填写， 最多两位小数

export function getTimeDistance(type) {
  const now = new Date()
  const oneDay = 1000 * 60 * 60 * 24

  if (type === 'today') {
    now.setHours(0)
    now.setMinutes(0)
    now.setSeconds(0)
    return [moment(now), moment(now.getTime() + (oneDay - 1000))]
  }

  if (type === 'week') {
    let day = now.getDay()
    now.setHours(0)
    now.setMinutes(0)
    now.setSeconds(0)

    if (day === 0) {
      day = 6
    } else {
      day -= 1
    }

    const beginTime = now.getTime() - day * oneDay

    return [moment(beginTime), moment(beginTime + (7 * oneDay - 1000))]
  }

  if (type === 'month') {
    const year = now.getFullYear()
    const month = now.getMonth()
    const nextDate = moment(now).add(1, 'months')
    const nextYear = nextDate.year()
    const nextMonth = nextDate.month()

    return [
      moment(`${year}-${fixedZero(month + 1)}-01 00:00:00`),
      moment(
        moment(
          `${nextYear}-${fixedZero(nextMonth + 1)}-01 00:00:00`
        ).valueOf() - 1000
      )
    ]
  }

  if (type === 'year') {
    const year = now.getFullYear()

    return [moment(`${year}-01-01 00:00:00`), moment(`${year}-12-31 23:59:59`)]
  }
}

const buildParams = function (prefix, obj, traditional, add) {
  var name

  if (Array.isArray(obj)) {
    // Serialize array item.
    for (let i in obj) {
      if (traditional || /\[\]$/.test(prefix)) {
        // Treat each array item as a scalar.
        add(prefix, obj[i])
      } else {
        // Item is non-scalar (array or object), encode its numeric index.
        buildParams(
          prefix +
          '[' +
          (typeof obj[i] === 'object' && obj[i] != null ? i : '') +
          ']',
          obj[i],
          traditional,
          add
        )
      }
    }
  } else if (
    !traditional &&
    Object.prototype.toString.call(obj) === '[object Object]'
  ) {
    // Serialize object item.
    for (name in obj) {
      buildParams(prefix + '[' + name + ']', obj[name], traditional, add)
    }
  } else {
    // Serialize scalar item.
    add(prefix, obj)
  }
}

// paramString
export const queryString = (a, traditional) => {
  var prefix,
    s = [],
    add = function (key, valueOrFunction) {
      // If value is a function, invoke it and use its return value
      var value =
        Object.prototype.toString.call(valueOrFunction) === '[object Function]'
          ? valueOrFunction()
          : valueOrFunction

      s[s.length] =
        encodeURIComponent(key) +
        '=' +
        encodeURIComponent(value == null ? '' : value)
    }

  // If an array was passed in, assume that it is an array of form elements.
  //if ( Array.isArray( a ) || ( a.jquery && !jQuery.isPlainObject( a ) ) ) {
  if (Array.isArray(a)) {
    // Serialize the form elements
    for (let i in a) {
      add(i, a[i])
    }
  } else {
    // If traditional, encode the "old" way (the way 1.3.2 or older
    // did it), otherwise encode params recursively.
    for (prefix in a) {
      buildParams(prefix, a[prefix], traditional, add)
    }
  }

  // Return the resulting serialization
  return s.join('&')
}

export function getPlainreNode(nodeList, parentPath = '') {
  const arr = []
  nodeList.forEach(node => {
    const item = node
    item.path = `${parentPath}/${item.path || ''}`.replace(/\/+/g, '/')
    item.exact = true
    if (item.children && !item.component) {
      arr.push(...getPlainNode(item.children, item.path))
    } else {
      if (item.children && item.component) {
        item.exact = false
      }
      arr.push(item)
    }
  })
  return arr
}

function accMul(arg1, arg2) {
  let m = 0
  const s1 = arg1.toString()
  const s2 = arg2.toString()
  m += s1.split('.').length > 1 ? s1.split('.')[1].length : 0
  m += s2.split('.').length > 1 ? s2.split('.')[1].length : 0
  return (Number(s1.replace('.', '')) * Number(s2.replace('.', ''))) / 10 ** m
}

export function digitUppercase(n) {
  const fraction = ['角', '分']
  const digit = ['零', '壹', '贰', '叁', '肆', '伍', '陆', '柒', '捌', '玖']
  const unit = [['元', '万', '亿'], ['', '拾', '佰', '仟', '万']]
  let num = Math.abs(n)
  let s = ''
  fraction.forEach((item, index) => {
    s += (digit[Math.floor(accMul(num, 10 * 10 ** index)) % 10] + item).replace(
      /零./,
      ''
    )
  })
  // s = s || '整';
  num = Math.floor(num)
  for (let i = 0; i < unit[0].length && num > 0; i += 1) {
    let p = ''
    for (let j = 0; j < unit[1].length && num > 0; j += 1) {
      p = digit[num % 10] + unit[1][j] + p
      num = Math.floor(num / 10)
    }
    s = p.replace(/(零.)*零$/, '').replace(/^$/, '零') + unit[0][i] + s
  }

  return s
    .replace(/(零.)*零元/, '元')
    .replace(/(零.)+/g, '零')
    .replace(/^整$/, '零元整')
}

function getRelation(str1, str2) {
  if (str1 === str2) {
  }
  const arr1 = str1.split('/')
  const arr2 = str2.split('/')
  if (arr2.every((item, index) => item === arr1[index])) {
    return 1
  } else if (arr1.every((item, index) => item === arr2[index])) {
    return 2
  }
  return 3
}

function getRenderArr(routes) {
  let renderArr = []
  renderArr.push(routes[0])
  for (let i = 1; i < routes.length; i += 1) {
    // 去重
    renderArr = renderArr.filter(item => getRelation(item, routes[i]) !== 1)
    // 是否包含
    const isAdd = renderArr.every(item => getRelation(item, routes[i]) === 3)
    if (isAdd) {
      renderArr.push(routes[i])
    }
  }
  return renderArr
}

export function getPageQuery() {
  return parse(window.location.href.split('?')[1])
}

export function getQueryPath(path = '', query = {}) {
  const search = stringify(query)
  if (search.length) {
    return `${path}?${search}`
  }
  return path
}

/* eslint no-useless-escape:0 */
const reg = /(((^https?:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+(?::\d+)?|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)$/


export function getRoutes(mDatas) {
  let newArr = new Array()
  mDatas.forEach(element => {
    const root = `${element.path}`
    newArr.push(root)
    if (!element.routes || element.routes.length === 0) {
      // newArr.push(root)
    } else {
      const childRoutes = getRoutes(element.routes)
      childRoutes.forEach(item => {
        newArr.push(`${item}`)
      })
    }
  })
  return newArr
}

//获得当前移动设备的类型
export const phoneModel = () => {
  var u = window.navigator.userAgent
  if (u.indexOf('Android') > -1 || u.indexOf('Linux') > -1) {
    return 'android'
  } else if (u.indexOf('iPhone') > -1) {
    return 'iPhone'
  } else if (u.indexOf('Windows Phone') > -1) {
    return 'winPhone'
    //winphone手机
  }
}

//获取URL参数
export const getUrlParam = name => {
  var reg = new RegExp('(^|&)' + name + '=([^&]*)(&|$)') //构造一个含有目标参数的正则表达式对象
  var r = window.location.search.substr(1).match(reg) //匹配目标参数
  if (r != null) return unescape(r[2])
  return null //返回参数值
}

//冒泡排序
export const bubbleSort = array => {
  for (let i = 0; i < array.length; i++) {
    for (let n = 0; n < array.length - i - 1; n++) {
      if (array[n] > array[n + 1]) {
        const swap = array[n]
        array[n] = array[n + 1]
        array[n + 1] = swap
      }
    }
  }
  return array
}

//生成uuid
export const guid = () => {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
    var r = (Math.random() * 16) | 0,
      v = c == 'x' ? r : (r & 0x3) | 0x8
    return v.toString(16)
  })
}

//将http协议重定向到https
export const httpToHttps = function (url) {
  var newUrlArr = url.split('://')
  return newUrlArr[0] + 's://' + newUrlArr[1]
}

//清除字符串所有空格
export const trim = string => {
  if (!string) {
    return ''
  }
  return string.replace(/\s/g, '')
}

//注册全局变量
export const regGlobalVars = () => {
  if (window) {
    String.prototype.replaceAll = function (s1, s2) {
      return this.replace(new RegExp(s1, 'gm'), s2)
    }
    // window.jyGlobal = true;
    // window.imgPath = imgPath;
  }
}

//清空cookie
export const clearAllCookie = () => {
  if (document && document.cookie) {
    var keys = document.cookie.match(/[^ =;]+(?=\=)/g)
    if (keys) {
      for (var i = keys.length; i--;)
        document.cookie = keys[i] + '=0;expires=' + new Date(0).toUTCString()
    }
  }
}

//当前hash版本判断与是否是协议判断
export const versionCheck = () => {
  // if (process.env.RUN_ENV === 'dev') { return }
  const hashVersion = process.env.HASH_VERSION || '001'
  if (localStorage.hashVersion + '' !== hashVersion + '') {
    localStorage.clear()
    sessionStorage.clear()
    clearAllCookie()
    const v =
      localStorage.hashVersion > hashVersion ? localStorage : hashVersion
    localStorage.setItem('hashVersion', v)
    setTimeout(() => {
      window.location.reload()
    }, 1000)
  }
}

function timestamp(hashVersion) {
  //  var getTimestamp=Math.random();
  let url = window.location.href
  if (url.indexOf('?') > -1) {
    url = url + '&t=' + hashVersion
  } else {
    url = url + '?t=' + hashVersion
  }
  return url
}

//当前协议判断
export const protocolCheck = () => {
  if (window.location.href.indexOf('http://') > -1 &&
    window.location.href.indexOf('http://1') === -1 &&
    window.location.href.indexOf('http://localhost') === -1) {//http重定向到https
    window.location.href = window.location.href.replace('http://', 'https://');
  }
}

//获得富文本里面的文字
export function getSimpleText(html) {
  var re1 = new RegExp('<.+?>', 'g') //匹配html标签的正则表达式，"g"是搜索匹配多个符合的内容
  var msg = html.replace(re1, '') //执行替换成空字符
  return msg
}

//json转表单FormData格式
export const formDates = datas => {
  let result = new FormData()
  for (const i in datas) {
    result.append(i, datas[i])
  }
  return result
}

//获得图片资源 name文件名,path：assets下文件路径，type:文件类型
export const imgGet = (name, path, type = 'png') => {
  return require(`@/assets/${path}/${name}.${type}`)
}

//判断
export const isObject = param =>
  Object.prototype.toString.call(param) === '[object Object]'
export const isArray = param =>
  Object.prototype.toString.call(param) === '[object Array]'
export const isPhoneNumber = string => {
  var reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/ //验证规则
  return reg.test(string)
}

//地区选择数据处理
export const districtDataHandle = data => {
  let newData = []
  newData = data.map(item => {
    let newTtem = { label: item.district, value: item.id }
    if (item.children && item.children.length) {
      newTtem.children = districtDataHandle(item.children)
    }
    return newTtem
  })
  return newData;
}


//获得特定时间与当前的文字描述eg:刚刚、一天前

export const getTimeDes = (time = new Date()) => {
  const now = +new Date();
  time = +moment(time);
  const distance = (now - time) / 1000;
  if (distance <= 1 * 60) {//一分钟
    return '刚刚'
  }
  if (1 * 60 < distance && distance <= 60 * 60) {
    return '1小时内'
  }
  if (60 * 60 < distance && distance <= 60 * 60 * 24) {
    return '1天内'
  }
  if (60 * 60 * 24 < distance && distance <= 60 * 60 * 24 * 30) {
    return '1个月内'
  }
  if (60 * 60 * 24 * 30 < distance && distance < 60 * 60 * 24 * 365) {
    return Math.ceil(distance / 60 * 60 * 24 * 30) + '月前'
  }
}


export const isUrl = (path) => {
  return reg.test(path)
}

//格式化菜单
export const formatter = (data, parentPath = '', parentAuthority) => {
  return data.map(item => {
    let { path } = item
    if (!isUrl(path)) {
      path = parentPath + item.path
    }
    const result = {
      ...item,
      path,
      authority: item.authority || parentAuthority
    }
    if (item.children) {
      result.children = formatter(
        item.children,
        `${parentPath}${item.path}/`,
        item.authority
      )
    }
    return result
  })
}
//递归数菜单 取出授权控制的路由
export const getAuthorityRouters = (menuData = [], parentAuthority = false) => {
  let result = [];
  menuData.forEach(element => {
    if (element.authority || parentAuthority) {
      if (element.children && element.children.length) {
        result = [...result, element.path, ...getAuthorityRouters(element.children, true)];
      } else {
        result.push(element.path)
      }
    } else {
      if (element.children && element.children.length) {
        result = [...result, ...getAuthorityRouters(element.children, false)];
      }
    }
  });
  return result;
}

//递归数菜单 取出所有菜单
export const getAllRouters = (menuData = []) => {
  let result = [];
  menuData.forEach(element => {
    if (element.children && element.children.length) {
      result = [...result, element.path, ...getAllRouters(element.children)];
    } else {
      result.push(element.path)
    }
  });
  return result;
}