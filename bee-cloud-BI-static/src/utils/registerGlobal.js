// const imgPath = `http://cba360.cn/vpayFile`;
import { message } from 'antd'
import { accAdd } from '@/utils/utils'

//注册全局变量
const regGlobalVars = () => {
    if (window) {
        String.prototype.replaceAll = function (s1, s2) {
            return this.replace(new RegExp(s1, "gm"), s2);
        }
        String.prototype.toFixed = function (num) {
            if (Number(this) === NaN) {
                message.error('数字录入有特殊符号或为空')
                return '0.00'
            }
            return Number(this).toFixed(num)
        }
        Number.prototype.accurateAdd = function(arg) {
            const a = [].slice.call(arguments)
            return accAdd(a, this)
          }
        // window.jyGlobal = true;
        // window.imgPath = imgPath;
    }
}

export default regGlobalVars;