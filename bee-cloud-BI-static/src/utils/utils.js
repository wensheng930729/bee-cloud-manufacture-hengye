import moment from 'moment'
import { parse, stringify } from 'qs'

export function fixedZero(val) {
  return val * 1 < 10 ? `0${val}` : val
}

export const moneyExp = /(^[1-9]([0-9]{1,12})?(\.[0-9]{1,2})?$)|(^(0){1}$)|(^[0-9]\.[0-9]{1,12}([0-9])?$)/gi //金额填写， 最多两位小数
export const moneyExp1 = /(^[-]?[1-9]([0-9]{1,9})?(\.[0-9]{1,2})?$)|(^(0){1}$)|(^[-]?[0-9]\.[0-9]{1,9}([0-9])?$)/gi //金额填写， 最多两位小数
export const moneyExp5 = /(^[1-9]([0-9]{1,12})?(\.[0-9]{1,5})?$)|(^(0){1}$)|(^[0-9]\.[0-9]{1,5}?$)/gi//金额填写， 最多五位小数

export const regExps = {
  phone: /^1[3|4|5|6|7|8|9][0-9]{9}$/, //手机号
  password: /^[a-zA-Z0-9][^%&',;=?$*\x22]{5,15}$/,//6-16位密码
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

export function getPlainNode(nodeList, parentPath = '') {
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

//数组换位
export function swapArr(arr, index1, index2) {
  arr[index1] = arr.splice(index2, 1, arr[index1])[0];
  return arr;
}

//金额大写
export function chineseMon(price) {
  if (price == '') {
    return ''
  }
  let money = parseFloat(price)
  let cnNums = new Array(
    '零',
    '壹',
    '贰',
    '叁',
    '肆',
    '伍',
    '陆',
    '柒',
    '捌',
    '玖'
  )
  let cnIntRadice = new Array('', '拾', '佰', '仟')
  let cnIntUnits = new Array('', '万', '亿', '兆')
  let cnDecUnits = new Array('角', '分', '毫', '厘')
  let cnInteger = '整',
    cnIntLast = '元'
  let maxNum = 999999999999999.9999
  let integerNum,
    decimalNum,
    chineseStr = '',
    parts
  // 处理大写数字
  if (money >= maxNum) {
    return ''
  }
  if (money == 0) {
    chineseStr = cnNums[0] + cnIntLast + cnInteger
    return chineseStr
  }
  //转换为字符串
  money = money.toString()
  if (money.indexOf('.') == -1) {
    integerNum = money
    decimalNum = ''
  } else {
    parts = money.split('.')
    integerNum = parts[0]
    decimalNum = parts[1].substr(0, 4)
  }
  //获取整型部分转换
  if (parseInt(integerNum, 10) > 0) {
    var zeroCount = 0
    var IntLen = integerNum.length
    for (var i = 0; i < IntLen; i++) {
      var n = integerNum.substr(i, 1)
      var p = IntLen - i - 1
      var q = p / 4
      var m = p % 4
      if (n == '0') {
        zeroCount++
      } else {
        if (zeroCount > 0) {
          chineseStr += cnNums[0]
        }
        //归零
        zeroCount = 0
        chineseStr += cnNums[parseInt(n)] + cnIntRadice[m]
      }
      if (m == 0 && zeroCount < 4) {
        chineseStr += cnIntUnits[q]
      }
    }
    chineseStr += cnIntLast
  }
  //小数部分
  if (decimalNum != '') {
    var decLen = decimalNum.length
    for (var i = 0; i < decLen; i++) {
      var n = decimalNum.substr(i, 1)
      if (n != '0') {
        chineseStr += cnNums[Number(n)] + cnDecUnits[i]
      }
    }
  }

  if (chineseStr == '') {
    chineseStr += cnNums[0] + cnIntLast + cnInteger
  } else if (decimalNum == '') {
    chineseStr += cnInteger
  }

  return chineseStr
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

/**
 * Get router routing configuration
 * { path:{name,...param}}=>Array<{name,path ...param}>
 * @param {string} path
 * @param {routerData} routerData
 */
// export function getRoutes(path, routerData) {
//   let routes = Object.keys(routerData).filter(
//     routePath => routePath.indexOf(path) === 0 && routePath !== path
//   );
//   // Replace path to '' eg. path='user' /user/name => name
//   routes = routes.map(item => item.replace(path, ''));
//   // Get the route to be rendered to remove the deep rendering
//   const renderArr = getRenderArr(routes);
//   // Conversion and stitching parameters
//   const renderRoutes = renderArr.map(item => {
//     const exact = !routes.some(route => route !== item && getRelation(route, item) === 1);
//     return {
//       exact,
//       ...routerData[`${path}${item}`],
//       key: `${path}${item}`,
//       path: `${path}${item}`,
//     };
//   });
//   return renderRoutes;
// }

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

export function isUrl(path) {
  return reg.test(path)
}

export function formatter(data, parentPath = '', parentAuthority) {
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

//处理服务器返回的菜单数据，将chirldren改为routes
export function setRoutes(mDatas = []) {
  return mDatas.map(element => {
    let item = { path: element.path, name: element.name, icon: element.icon, isHide: element.isHide || false };
    if (element.children && element.children.length) {
      item.routes = setRoutes(element.children);
    }
    return item;
  })
}

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

export function accAdd(arg) {
  var r1, r2, m
  function sortNumber(a, b) {
    return a - b
  }
  const max = arg.sort(sortNumber).reverse()[0]
  m = Math.pow(10, max.length * 2)

  return (arg1 * m + arg2 * m) / m
}

export function format(money) {//金钱 千位显示格式化
  if (isNaN(money)) {
    money = ''
  }
  var str = money.toString()
  var reg =
    str.indexOf('.') > -1 ? /(\d)(?=(\d{3})+\.)/g : /(\d)(?=(?:\d{3})+$)/g
  return str.replace(reg, '$1,')
}

//获取浏览器类型
export function getExplorer() {
  var explorer = window.navigator.userAgent;
  //ie
  if (explorer.indexOf("MSIE") >= 0) {
    return 'ie';
  }
  //firefox
  else if (explorer.indexOf("Firefox") >= 0) {
    return 'Firefox';
  }
  //Chrome
  else if (explorer.indexOf("Chrome") >= 0) {
    return 'Chrome';
  }
  //Opera
  else if (explorer.indexOf("Opera") >= 0) {
    return 'Opera';
  }
  //Safari
  else if (explorer.indexOf("Safari") >= 0) {
    return 'Safari';
  }
}

const isObject = param =>
  Object.prototype.toString.call(param) === '[object Object]'
const isArray = param =>
  Object.prototype.toString.call(param) === '[object Array]'

// paramString
const queryString = (a, traditional) => {
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

//手机号码验证 **antd input的type为phone会自动添加空格
const isPhone = string => {
  var reg = regExps.phone //验证规则
  return reg.test(trim(string))
}
//银行卡号码验证
const isBankNum = string => {
  var reg = /^([1-9]{1})(\d{14}|\d{18})$/ //验证规则
  return reg.test(trim(string))
}

//人名校验
const isName = string => {
  var reg = /^[\u4E00-\u9FA5\uf900-\ufa2d·s]{2,20}$/ //验证规则
  return reg.test(trim(string))
}
const phoneModel = () => {
  var u = window.navigator.userAgent
  if (u.indexOf('Android') > -1 || u.indexOf('Linux') > -1) {
    return 'Android'
  } else if (u.indexOf('iPhone') > -1) {
    return 'iPhone'
  } else if (u.indexOf('Windows Phone') > -1) {
    return 'winPhone'
    //winphone手机
  }
}

//清除字符串所有空格
const trim = string => {
  if (!string) {
    return ''
  }
  return string.replace(/\s/g, '')
}

//获取URL
const getUrlParam = name => {
  var reg = new RegExp('(^|&)' + name + '=([^&]*)(&|$)') //构造一个含有目标参数的正则表达式对象
  var r = window.location.search.substr(1).match(reg) //匹配目标参数
  if (r != null) return unescape(r[2])
  return null //返回参数值
}

const timeFormat = (
  date,
  mode = 'date',
  seperator1 = '-',
  seperator2 = ':'
) => {
  // var date = new Date(date);;
  var month = date.getMonth() + 1
  var strDate = date.getDate()
  var hour = date.getHours()
  var minute = date.getMinutes()
  var second = date.getSeconds()
  if (strDate >= 0 && strDate <= 9) {
    strDate = '0' + strDate
  }
  if (hour >= 0 && hour <= 9) {
    hour = '0' + hour
  }
  if (minute >= 0 && minute <= 9) {
    minute = '0' + minute
  }
  if (second >= 0 && second <= 9) {
    second = '0' + second
  }

  let currentdate = ''
  switch (mode) {
    case 'date':
      currentdate =
        date.getFullYear() + seperator1 + month + seperator1 + strDate;
      break
    case 'minute':
      currentdate =
        date.getFullYear() + seperator1 + month + seperator1 + strDate + ' ' + hour + seperator2 + minute;
      break
    case 'second':
      currentdate =
        date.getFullYear() + seperator1 + month + seperator1 + strDate + ' ' + hour + seperator2 + minute + seperator2 + second;
      break
    default:
      break
  }

  return currentdate
}

//返回某一时间到N天后的时间数组格式YYYY-MM-dd;beginDate未Date格式，daysLater未int
const dateArray = (beginDate, daysLater, seperator = '/') => {
  if (!beginDate) beginDate = new Date()
  if (!daysLater) daysLater = 30
  var arr = []
  beginDate.setDate(beginDate.getDate() - 1)
  for (var i = 0; i <= daysLater; i++) {
    //获取三十天的日期
    beginDate.setDate(beginDate.getDate() + 1)
    var year = beginDate.getFullYear()
    var month = beginDate.getMonth() + 1
    var day = beginDate.getDate()
    arr.push(year + seperator + month + seperator + day)
  }
  return arr
}

const dataURLtoBlob = dataurl => {
  if (dataurl.split(',').length < 2 || dataurl == null) {
    return ''
  }

  var arr = dataurl.split(','),
    mime = arr[0].match(/:(.*?);/)[1],
    bstr = atob(arr[1]),
    n = bstr.length,
    u8arr = new Uint8Array(n)
  while (n--) {
    u8arr[n] = bstr.charCodeAt(n)
  }
  return new Blob([u8arr], { type: mime })
}

const formDates = datas => {
  let result = new FormData()
  for (const i in datas) {
    result.append(i, datas[i])
  }
  return result
}

//冒泡排序
const bubbleSort = array => {
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
const guid = () => {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
    var r = (Math.random() * 16) | 0,
      v = c == 'x' ? r : (r & 0x3) | 0x8
    return v.toString(16)
  })
}

//浏览器类型
const u2 = navigator.userAgent.toLowerCase()
const browserType = {
  isWeixinBrowser: () => u2.match(/MicroMessenger/i) == 'micromessenger',
  isAndroidBrowser: () => u2.indexOf('android') > -1,
  isIOSBrowser: () => u2.indexOf('iphone') > -1,
  isH5App: () => u2.indexOf('html5plus') > -1
}
const httpToHttps = function (url) {
  // if(BaseUrl.imgUrl){
  var newUrlArr = url.split('://')
  return newUrlArr[0] + 's://' + newUrlArr[1]
  // }else{
  //     return url
  // }
}

const titleToUpperCase = (str) => {
  return str.replace(/( |^)[a-z]/, (L) => L.toUpperCase());
}
export {
  isObject,
  isArray,
  queryString,
  isPhone,
  isBankNum,
  isName,
  browserType,
  getUrlParam,
  trim,
  timeFormat,
  dateArray,
  phoneModel,
  dataURLtoBlob,
  formDates,
  bubbleSort,
  guid,
  httpToHttps,
  titleToUpperCase
}
