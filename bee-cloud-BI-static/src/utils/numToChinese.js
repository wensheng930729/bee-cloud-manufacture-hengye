const UnitMap = {
  0: '零',
  1: '一',
  2: '二',
  3: '三',
  4: '四',
  5: '五',
  6: '六',
  7: '七',
  8: '八',
  9: '九',
};
const BigUnitMap = {
  10: '十',
  100: '百',
  1000: '千',
};
const getUnit = num => {
  const n = num.toString().split('');
  const units = (function(n) {
    const ns = [];
    for (let i = 0, I = n.length; i < I; i++) {
      ns.push({
        Unit: n[i],
        BigUnit: `1${Array(I - i).join('0')}`,
      });
    }
    return ns;
  }(n));
  let str = '';
  let last;
  for (let i = 0, I = units.length; i < I; i++) {
    if (last != '0' || units[i].Unit != '0') {
      str +=
        (function() {
          const list = n.slice(i).join('');
          if (i != 0 && list.match(/^0+$/i)) return '';
          if (i == 0 && units[i].BigUnit == '10' && units[i].Unit == '1') return '';
          return UnitMap[units[i].Unit];
        }()) +
        (units[i].Unit != '0' && typeof BigUnitMap[units[i].BigUnit] !== 'undefined'
          ? BigUnitMap[units[i].BigUnit]
          : '');
      last = units[i].Unit;
    }
  }
  return str;
};
const numToChinese = num => {
  num = num || 0;
  const str = num
    .toString()
    .split('')
    .reverse()
    .join('');
  const reg = /(\d{1,4})(\d{1,4})?(\d{1,4})?(\d{1,4})?/i;
  const ex = reg.exec(str);
  let qian;
  let wang;
  let yi;
  if (ex[1]) {
    qian = ex[1]
      .split('')
      .reverse()
      .join('');
  }
  if (ex[2]) {
    wang = ex[2]
      .split('')
      .reverse()
      .join('');
  }
  if (ex[3]) {
    yi = ex[3]
      .split('')
      .reverse()
      .join('');
  }
  let result = '';
  if (yi && yi.match(/[^0]/i)) {
    result = `${result + getUnit(yi)}亿`;
  }
  if (wang && wang.match(/[^0]/i)) {
    result = `${result + getUnit(wang)}万`;
  }
  if (!qian || !result || !qian.match(/^0+$/i)) {
    result += getUnit(qian);
  }
  return result;
};

const chnNumChar = {
  零: 0,
  一: 1,
  二: 2,
  三: 3,
  四: 4,
  五: 5,
  六: 6,
  七: 7,
  八: 8,
  九: 9,
};

const chnNameValue = {
  十: { value: 10, secUnit: false },
  百: { value: 100, secUnit: false },
  千: { value: 1000, secUnit: false },
  万: { value: 10000, secUnit: true },
  亿: { value: 100000000, secUnit: true },
};

function chineseToNum(chnStr) {
  let rtn = 0;
  let section = 0;
  let number = 0;
  let secUnit = false;
  const str = chnStr.split('');

  for (let i = 0; i < str.length; i++) {
    const num = chnNumChar[str[i]];
    if (typeof num !== 'undefined') {
      number = num;
      if (i === str.length - 1) {
        section += number;
      }
    } else {
      const unit = chnNameValue[str[i]].value;
      secUnit = chnNameValue[str[i]].secUnit;
      if (secUnit) {
        section = (section + number) * unit;
        rtn += section;
        section = 0;
      } else {
        section += number * unit;
      }
      number = 0;
    }
  }
  return rtn + section;
}

export { chineseToNum, numToChinese };
